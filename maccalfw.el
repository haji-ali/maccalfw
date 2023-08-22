;;; maccalfw.el --- Calendar view for Mac Calendars -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Al Haji-Ali

;; Author: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/haji-ali/maccalfw
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bridge from Mac Calendar to calfw. The API and interfaces have not been
;; confirmed yet.

;;; Installation:

;; Here is a minimum sample code:
;; (require 'maccalfw)
;; To open a calendar buffer, execute the following function.
;; (cfw:open-maccal-calendar 'all)

;;; Code:

(require 'calfw)
(require 'wid-edit)

(defcustom maccalfw-event-save-hook nil
  "Hook called when an event is saved successfully.
Takes one argument which is the new event data."
  :type 'hook
  :group 'maccalfw)

(defface maccalfw-event-notes-field
  '((t
     :inherit widget-field
     :box (:line-width (0 . 0))))
  "Face used for editable fields."
  :version "28.1")

(defface maccalfw-event-title-field
  '((t
     :inherit widget-field
     :height 1.8
     :weight bold
     :family sans-serif))
  "Face used for editable fields."
  :version "28.1")

(defface maccalfw-event-field-names
  '((t
     :weight bold))
  "Face used for field names."
  :version "28.1")

(defvar-keymap maccalfw-event--custom-map
  :doc "Keymap including custom bindings for `maccalfw-event'."
  "C-c C-k" #'maccalfw-event-kill
  "C-c C-s" #'maccalfw-event-date-field-pick
  "C-c C-w" #'maccalfw-event-save
  "C-x C-s" #'maccalfw-event-save)

(defvar-keymap maccalfw-event-mode-map
  :doc "Keymap for `maccalfw-event'."
  :parent (make-composed-keymap
           maccalfw-event--custom-map
           widget-keymap))

(defvar-keymap maccalfw-event-field-map
  :doc "Keymap for fields in `maccalfw-event'."
  :parent (make-composed-keymap
           maccalfw-event--custom-map
           widget-field-keymap))

(defvar-keymap maccalfw-event-text-map
  :doc "Keymap for text fields in `maccalfw-event'."
  :parent (make-composed-keymap
           maccalfw-event--custom-map
           widget-text-keymap))

(define-derived-mode maccalfw-event-mode fundamental-mode "Calendar Event"
  "Major mode for editing calendar events."
  :lighter " Calfw event"
  (use-local-map maccalfw-event-mode-map)
  (make-local-variable 'kill-buffer-query-functions)
  (add-to-list 'kill-buffer-query-functions
               'maccalfw-event-save-maybe))

(defvar maccalfw-event--timezones nil)
(defvar maccalfw-event--default-timezone nil)

;; These need to be dynamically bound when using `org-pick-date'
(defvar org-time-was-given)
(defvar org-end-time-was-given)

(defun maccalfw--load-module (&optional force)
  "Load an compile dynamic module for maccalfw.
If FORCE is nil, then the module is not compiled nor re-loaded if
already loaded. \\='compile forces recompilation before
re-loading and \\='compile-only forces recompilation without
loading."
  (unless (and (not force) (fboundp #'maccalfw-get-calendars))
    (unless module-file-suffix
      (error "maccalfw: Dynamic modules are not supported"))
    (let* ((mod-name (file-name-with-extension
                      "libmaccalfw"
                      module-file-suffix))
           (mod-file (locate-library mod-name t)))
      (unless (and mod-file (not (member force '(compile compile-only))))
        (let* ((swift (or (getenv "SWIFTC")
                          (executable-find "swiftc")
                          (error "maccalfw: No swift compiler found")))
               (default-directory (file-name-directory
                                   (feature-file 'maccalfw)))
               (command
                `(,swift "-Xcc" "-fmodule-map-file=src/module.modulemap"
                         "-I/opt/homebrew/include/"
                         "src/EmacsUtil.swift"
                         "src/MacCalfw.swift"
                         "-O" "-emit-library"
                         "-o" ,mod-name)))
          (with-current-buffer
              (get-buffer-create "*maccalfw module compilation*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (compilation-mode)
              (insert (string-join command " ") "\n")
              (if (equal 0 (apply #'call-process (car command) nil
                                  (current-buffer) t (cdr command)))
                  (insert (message "maccalfw: %s compiled successfully"
                                   mod-name))
                (let ((msg (format "maccalfw: Compilation of %s failed"
                                   mod-name)))
                  (insert msg)
                  (pop-to-buffer (current-buffer))
                  (error msg)))))
          (setq mod-file (expand-file-name mod-name))))
      (unless (eq force 'compile-only)
        (module-load mod-file)))))

(defun maccalfw--decode-date (time)
  "Return a calendar date from encoded TIME.
The return value is (month day year)."
  (list (decoded-time-month time)
        (decoded-time-day time)
        (decoded-time-year time)))

(defun maccalfw--encode-date (date &optional end-of-day)
  "Encode a calendar DATE.
DATE is of the format (month day year). If END-OF-DAY is nil, the
time is midnight, otherwise it is a second before midnight of the
next day."
  (encode-time (append
                (if end-of-day
                    (list 59 59 23)
                  (list 0 0 0))
                (list (nth 1 date)
                     (nth 0 date)
                      (nth 2 date)))))

(defun maccalfw--decode-time (time)
  "Return a calendar time from encoded TIME."
  (list (decoded-time-hour time)
        (decoded-time-minute time)))

(defun maccalfw--convert-event (event)
  "Convert an EVENT to a calfw event.
The event is returned `maccalfw-fetch-events'."
  (let ((start (decode-time (plist-get event :start)))
        (end (decode-time (plist-get event :end)))
        (all-day-p (plist-get event :all-day-p)))
    (make-cfw:event
     :start-date  (maccalfw--decode-date start)
     :start-time  (unless all-day-p
                    (maccalfw--decode-time start))
     :end-date    (when all-day-p
                    (maccalfw--decode-date end))
     :end-time    (unless all-day-p
                    (maccalfw--decode-time end))
     :title       (plist-get event :title)
     :location    (plist-get event :location)
     :description (plist-get event :summary)
     :status       (plist-get event :status)
     :data        event)))

(defun maccalfw--convert-to-calfw (events-list)
  "Convert an EVENTS-LIST to calfw events."
  (cl-loop for e in events-list
        for event = (maccalfw--convert-event e)
        if event
        if (cfw:event-end-date event)
        collect event into periods
        else
        collect event into contents
        else do
        (progn
          (message "Ignoring event \"%s\"" e)
          (message "Cannot handle this event, tag: %s" e))
        finally (return `((periods ,periods) ,@contents))))

(defun maccalfw--get-calendar-events (cal-id begin end)
  "Return all calendar event corresponding CAL-ID.
BEING and END are dates with the format (month day year). The
events between BEGIN and END are returned."
  (cl-loop for event in
           (maccalfw--convert-to-calfw
            (maccalfw-fetch-events cal-id
                                   (maccalfw--encode-date begin)
                                   (maccalfw--encode-date end t)))
        if (and (listp event)
                (equal 'periods (car event)))
        collect
        (cons
         'periods
         (cl-loop for evt in (cadr event)
               collect evt))
        else
        collect event))

(defun maccalfw--create-source (name cal-id color)
  "Create a cfw:source out of a calendar.
CAL-ID is the ID of the calendar and get be obtained with
`maccalfw-get-calendars'. The calendar's NAME and COLOR are set
accordingly."
  (make-cfw:source
   :name name
   :color color
   :update #'ignore
   :data (lambda (begin end)
           (maccalfw--get-calendar-events cal-id begin end))))

(defun maccalfw-get-calendars-by-name (names)
  "Return the calendar IDs with NAMES."
  (cl-remove-if-not
   (lambda (x) (member (plist-get x :title) names))
   (maccalfw-get-calendars)))

(defun maccalfw-open (&optional calendars)
  "Open a calfw calendar with CALENDARS from Apple's Calendar.
This command displays any CALENDARS obtained using
`maccalfw-get-calendars' or all of them if it is \\='all."
  (interactive (list 'all))
  (maccalfw--load-module)
  (when (eq calendars 'all)
    (setq calendars (maccalfw-get-calendars)))
  (cfw:open-calendar-buffer
   :view (if (featurep 'calfw-blocks)
             'block-week
           'week)
   :contents-sources
   (mapcar
    (lambda (x)
      (maccalfw--create-source (plist-get x :title)
                               (plist-get x :id)
                               (plist-get x :color)))
    calendars)
   :sorter (if (fboundp #'calfw-blocks-default-sorter)
               #'calfw-blocks-default-sorter
             #'string-lessp)))


(defun maccalfw-event-kill ()
  "Kill event buffer.
Warn if the buffer is modified and offer to save."
  (interactive)
  (when (maccalfw-event-save-maybe)
    ;; If `maccalfw-event-save-maybe' return t, then ignore modifications
    (set-buffer-modified-p nil)
    (quit-window t)))

(defun maccalfw-event-save ()
  "Save event."
  (interactive)
  (let* ((widgets (maccalfw-event--get-widgets))
         (title-wid (maccalfw-event--find-widget 'title widgets))
         (old-data (widget-get title-wid :event-data))
         (tz (widget-value (maccalfw-event--find-widget 'timezone widgets)))
         (all-day (widget-value (maccalfw-event--find-widget
                                 'all-day widgets)))
         (start (maccalfw-event--parse-datetime
                 (if all-day
                     "00:00"
                   (widget-value (maccalfw-event--find-widget
                                  'start-time widgets)))
                 (widget-value (maccalfw-event--find-widget
                                'start-date widgets))
                 tz))
         (end (maccalfw-event--parse-datetime
               (if all-day
                   "23:59:59"
                 (widget-value (maccalfw-event--find-widget
                                'end-time widgets)))
               (widget-value (maccalfw-event--find-widget 'end-date widgets))
               tz))
         (new-data
          (list
           :id (plist-get old-data :id)
           :calendar-id (widget-value (maccalfw-event--find-widget
                                       'calendar-id widgets))
           :title (widget-value title-wid)
           :timezone (if all-day "" tz)
           :all-day-p all-day
           :url (widget-value (maccalfw-event--find-widget 'url widgets))
           :location (widget-value (maccalfw-event--find-widget
                                    'location widgets))
           :availability (widget-value (maccalfw-event--find-widget
                                        'availability widgets))
           :notes (widget-value (maccalfw-event--find-widget
                                 'notes widgets))))
         (new-event (null (plist-get old-data :id))))
    (if (plist-get old-data :read-only)
        (user-error "Event is not editable.?"))
    ;; Only keep old-data

    (setq new-data
          (cl-loop for (key val) on new-data by #'cddr
                   for old-val = (plist-get old-data key)
                   when (or (and (not new-event) (not (equal val old-val)))
                            (and (or new-event (null old-val))
                                 (not (equal val ""))))
                   append (list key val)))
    (unless (and (not new-event)
                 (time-equal-p start (plist-get old-data :start)))
      (setq new-data (plist-put new-data :start start)))
    (unless (and (not new-event)
                 (time-equal-p end (plist-get old-data :end)))
                 (setq new-data
            (plist-put new-data :end end)))
    (when (and new-data (plist-get old-data :id))
      (setq new-data
            (plist-put new-data :id (plist-get old-data :id))))

    (if new-data
        (progn (widget-put title-wid
                           :event-data
                           (maccalfw-update-event new-data))
               (when (called-interactively-p 'interactive)
                 (message "Event saved."))
                 (run-hook-with-args
                  'maccalfw-event-save-hook
                (widget-get title-wid :event-data)))
      (when (called-interactively-p 'interactive)
        (message "(No changes to event to be saved)")))
    (set-buffer-modified-p nil)))

(defun maccalfw-event-save-maybe ()
  "Save event if buffer is modified and user agrees."
  (if (not (buffer-modified-p))
      t
    (let ((response
           (cadr
            (read-multiple-choice
             (format "Event %s modified; kill anyway?"
                     (buffer-name))
             '((?y "yes" "kill buffer without saving")
               (?n "no" "exit without doing anything")
               (?s "save and then kill" "save the even and then kill buffer"))
             nil nil (and (not use-short-answers)
                          (not (use-dialog-box-p)))))))
      (if (equal response "no")
          nil
        (unless (equal response "yes")
          (maccalfw-event-save)
          t)
        t))))

(defun maccalfw-event-date-field-pick (for-end-date)
  "Display date picker and assign date fields.
Sets both start and end dates/times, preserving the duration.
If FOR-END-DATE is non-nil, set the end-date only."
  (interactive (list
                (or current-prefix-arg
                    (when-let ((wid (widget-at (point)))
                               (key (widget-get wid :field-key)))
                      (member key '(end-time end-date))))))

  (let ((widgets (maccalfw-event--get-widgets)))
    (if (widget-get (maccalfw-event--find-widget 'start-time widgets)
                    :inactive)
      (maccalfw-event-read-only)
      (let* ((start-time-wid (maccalfw-event--find-widget 'start-time widgets))
             (start-date-wid (maccalfw-event--find-widget 'start-date widgets))
             (end-time-wid (maccalfw-event--find-widget 'end-time widgets))
             (end-date-wid (maccalfw-event--find-widget 'end-date widgets))
             (all-day-wid (maccalfw-event--find-widget 'all-day widgets))
           (all-day-p  (widget-value all-day-wid))
           (start-time
            (maccalfw-event--parse-datetime
             (if all-day-p
                 "00:00"
               (widget-value start-time-wid))
             (widget-value start-date-wid)))
           (end-time
            (maccalfw-event--parse-datetime
             (if all-day-p
                 "23:59"
               (widget-value end-time-wid))
             (widget-value end-date-wid)))
           ;; Define these two to make sure they are bound for `org-read-date'
           org-time-was-given
           org-end-time-was-given
           (new-time (org-read-date
                      (not (widget-value all-day-wid))
                      t
                      nil
                      "Event"
                      (if for-end-date
                          end-time
                        start-time))))
      (save-excursion
        (widget-value-set (if (and for-end-date all-day-p)
                              end-date-wid
                            start-date-wid)
                          (format-time-string "%F" new-time))

        (when (not for-end-date)
          ;; Shift end date as well
          (widget-value-set
           end-date-wid
           (format-time-string "%F"
                               (time-add new-time
                                         (* (- (time-to-days end-time)
                                               (time-to-days start-time))
                                            24 60 60)))))

        (when (and (not all-day-p) org-time-was-given)
          ;; Update time as well
          (if (or (not for-end-date) org-end-time-was-given)
              (progn
                (widget-value-set start-time-wid
                                  (maccalfw-event--format-time new-time))
                  (widget-value-set
                   end-time-wid
                                  (or org-end-time-was-given
                                      (maccalfw-event--format-time
                                       (time-add new-time
                                                 (time-subtract end-time
                                                                start-time))))))
            ;; for-end-date and range not given
            (widget-value-set end-time-wid
                                (maccalfw-event--format-time new-time)))))))))

(defun maccalfw-event-open (event)
  "Open a buffer to display the details of EVENT."
  (pop-to-buffer (generate-new-buffer "*calender event*"))
  (maccalfw-event-mode)
  (maccalfw-event--create-form event)

  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<maccalfw-event-mode-map>Event details. \
Save `\\[maccalfw-event-save]', \
abort `\\[maccalfw-event-kill]'."))
  (set-buffer-modified-p nil))

(defun maccalfw-event-mouse-down (event)
  "Call `mouse-drag-region' but disable double clicking.
Assigning this commend to [down-mouse-1] ensures the commands
assigned to [double-mouse-1] is called.
EVENT defaults to the event data."
  (interactive "e")
  (let (mouse-selection-click-count)
    (if (and (consp event)
             (nthcdr 2 event))
        (setcar (nthcdr 2 event) 1))
    (mouse-drag-region event)))

(defun maccalfw-event-new-event (event-data)
  "Create an events-details buffer for a new event.
EVENT-DATA contains the initial event information."
  (interactive
   (list
    (if (and (derived-mode-p 'cfw:calendar-mode)
             ;; TODO: Check that the view is indeed a block
             ;; (cfw:component-view (cfw:cp-get-component))
             ;; should return a block view
             (fboundp 'calfw-blocks-region-to-time))
        (if-let ((event (and current-prefix-arg
                             (get-text-property (point) 'cfw:event)))
                 (old-event-data (cfw:event-data event)))
            (cl-loop for (key val) on
                     old-event-data by #'cddr
                     if (member key '(:start :end :title
                                             :all-day-p
                                             :timezone
                                             :location
                                             :availability
                                             :url
                                             :notes))
                     append (list key val))
          (cl-destructuring-bind (start end all-day)
              (calfw-blocks-region-to-time)
            (list :start start
                  :end (or end (time-add start 3600))
                  :all-day-p all-day)))
      (list :start (current-time)
            :end (time-add (current-time) 3600)))))
  (maccalfw-event-open event-data))

(defun maccalfw-event-goto-details (event)
  "Open event details for the calfw EVENT."
  (interactive
   (list (or (get-text-property (point) 'cfw:event)
             (error "No event at location"))))
  (maccalfw-event-open (cfw:event-data event)))

(defun maccalfw-event-read-only (&rest _junk)
  "Ignoring the arguments, signal an error."
  (unless inhibit-read-only
    (error "The event is read-only")))

(defun maccalfw-event-delete-event (event)
  "Delete calfw EVENT."
  (interactive
   (list (or (get-text-property (point) 'cfw:event)
             (error "No event at location"))))
  (prog1 (maccalfw-remove-event
          (plist-get (cfw:event-data event) :id))
    (message "Event deleted")
    (cfw:refresh-calendar-buffer nil)))

(defun maccalfw-event--widget-overlay (widget key delete &rest props)
  "Create an overlay around WIDGET, setting its PROPS.
KEY is used to save the overlay in the widget.
If DELETE is non-nil, delete the widget instead."
  (unless (eq (null (widget-get widget key))
              (not (null delete)))
    (if delete
        (progn
          (delete-overlay (widget-get widget key))
          (widget-put widget key nil))
      (let* ((from (widget-get widget :from))
             (to (widget-get widget :to))
             (overlay (make-overlay from to nil t nil)))
        (cl-loop for (key val) on
                 props by #'cddr
                 do (overlay-put overlay key val))
        (widget-put widget key overlay)))))

(defun maccalfw-event--show-hide-widget (widget visible)
  "Show/hide WIDGET based on value of VISIBLE."
  (maccalfw-event--widget-overlay
   widget
   :hidden visible
   'evaporate t
   'priority 101
   'invisible (not visible))
  (widget-put widget
              :tab-order (if visible 1 -1)))

(defun maccalfw-event--make-inactive (&optional active)
  "Make all widgets in the current buffer inactive.
If ACTIVE is t, activate widgets instead"
  ;; widget-specify-active
  ;; How to properly loop over a plist?
  (save-excursion
    (cl-loop for wid in (maccalfw-event--get-widgets)
             do
             (maccalfw-event--widget-overlay
              wid :inactive active
              'evaporate t
              'priority 100
              'modification-hooks '(maccalfw-event-read-only)))))

(defun maccalfw-event--find-widget (key widgets)
  "Find widget field corresponding to KEY in WIDGETS."
  (cl-find-if
   (lambda (x) (eq key (widget-get x :field-key))) widgets) )

(defun maccalfw-event--get-widgets ()
  "Return all field widget in the current form."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     for wid = (cl-loop
                with old = (widget-at)
                do (cond
                    (widget-use-overlay-change
	             (goto-char (next-overlay-change (point))))
                    (t (forward-char 1)))
                for new = (widget-at)
                until (or (and new (not (eq new old)))
                          (eobp))
                finally return (and (not (eq new old)) new))
     while wid
     append
     (cl-loop
      for parent = wid then (widget-get parent :parent)
      while parent
      if (widget-get parent :field-key)
      collect parent))))

(defun maccalfw-event--create-wid (key &rest args)
  "Create widget and associate it to KEY.
Passes ARGS to `widget-create'"
  (let ((widget (apply 'widget-create args)))
    (widget-put widget :field-key key)
    widget))

(defun maccalfw-event--format-time (time &optional timezone)
  "Convert TIME to new TIMEZONE and format it as a string.
Assumes time is in the default timezone."
  (let ((tz (and timezone (alist-get timezone maccalfw-event--timezones
                                     nil nil #'equal))))
    (format-time-string
     "%R"
     (if tz
         (time-add
          time
          (-
           (plist-get tz :offset)
           (plist-get (cdr maccalfw-event--default-timezone) :offset)))
       time))))

(defun maccalfw-event--parse-datetime (date-str time-str &optional timezone)
  "Parse time and return the time in the default-time zone.
Time is in DATE-STR and TIME-STR is assumed to be in a given
TIMEZONE."
  (let ((tz (and timezone (alist-get timezone maccalfw-event--timezones
                                     nil nil #'equal)))
        (time (encode-time
               (parse-time-string (format "%s %s" date-str time-str)))))
    (if tz
        (time-add
         time
         (-
          (plist-get (cdr maccalfw-event--default-timezone) :offset)
          (plist-get (alist-get timezone maccalfw-event--timezones
                                nil nil #'equal)
                     :offset)))
      time)))

(defun maccalfw-event--timezone-widget-notify (widget &rest _)
  "Action for timezone action.
Assumes that WIDGET has an additional attributes `:old-value'
which is the old value of the timezone (will be updated in this
function)."
  (let ((widgets (maccalfw-event--get-widgets)))
    (unless (widget-value (maccalfw-event--find-widget 'all-day widgets))
  (let* ((old-tz (widget-get widget :old-value))
         (tz (widget-value widget)))
    (save-excursion
      (cl-loop for (date-wid . time-wid) in '((start-date . start-time)
                                              (end-date . end-time))
                   for time-widget = (maccalfw-event--find-widget
                                      time-wid widgets)
                   for date-widget = (maccalfw-event--find-widget
                                      date-wid widgets)
               do
               (widget-value-set
                time-widget
                (maccalfw-event--format-time
                 (maccalfw-event--parse-datetime
                    (widget-value time-widget)
                  (widget-value date-widget)
                  old-tz)
                 tz)))
          (widget-put widget :old-value tz))))))

(defun maccalfw-event--all-day-notify (widget &rest _)
  "Action for all-day WIDGET."
  (let ((checked (widget-value widget))
        (widgets (maccalfw-event--get-widgets)))
    (maccalfw-event--show-hide-widget
     (maccalfw-event--find-widget 'end-date widgets)
     checked)
    (mapc
     (lambda (x)
       (when-let (field (maccalfw-event--find-widget x widgets))
         (maccalfw-event--show-hide-widget field (not checked))))
     '(start-time end-time timezone))))

(defun maccalfw-event--create-form (event)
  "Create form in current buffer corresponding to EVENT."
  (let ((timezone (or (plist-get event :timezone)
                      (car maccalfw-event--default-timezone))))
    (widget-insert "\n\n")

    (maccalfw-event--create-wid
     'title 'editable-field
     :event-data event
     :keymap maccalfw-event-field-map
     :value-face 'maccalfw-event-title-field
     :format "%v \n" ; Text after the field!
     (or (plist-get event :title) ""))

    (let* ((cals (maccalfw-get-calendars))
           (options (cl-loop
                     for x in cals
                     when (or (plist-get x :editable)
                              (equal (plist-get x :id)
                                     (plist-get event :calendar-id)))
                     collect
                     `(item :tag ,(plist-get x :title)
                            :value ,(plist-get x :id)
                            :editable ,(plist-get x :editable)))))
      (apply
       'maccalfw-event--create-wid
       'calendar-id
       'menu-choice
       :tag "Calendar"
       :format "%[%t%]: %v\n\n"
       :value (or (plist-get event :calendar-id)
                  (plist-get
                   (cl-find-if
                    (lambda (x) (plist-get x :default))
                    cals)
                   :id)
                  (plist-get
                   (cl-find-if
                    (lambda (x) (plist-get x :editable))
                    cals)
                   :id))
       options))

    (maccalfw-event--create-wid
     'start-date
     'editable-field
     :keymap maccalfw-event-field-map
     :format " %v "
     :size 10
     (and event
          (format-time-string "%F"
                              (plist-get event :start))))

    (maccalfw-event--create-wid
     'end-date
     'editable-field
     :keymap maccalfw-event-field-map
     :format "  --    %v   "
     :size 10
     (format-time-string "%F"
                         (plist-get event :end)))
    (maccalfw-event--create-wid
     'start-time
     'editable-field
     :keymap maccalfw-event-field-map
     :format " %v -- "
     :size 6
     (maccalfw-event--format-time
      (plist-get event :start)
      timezone))
    (maccalfw-event--create-wid
     'end-time
     'editable-field
     :keymap maccalfw-event-field-map
     :format " %v   "
     :size 6
     (maccalfw-event--format-time
      (plist-get event :end)
      timezone))
    (maccalfw-event--create-wid
     'all-day
     'checkbox
     :format " %[%v%] All day\n\n"
     :notify #'maccalfw-event--all-day-notify
     (plist-get event :all-day-p))

    (unless maccalfw-event--timezones
      (setq
       maccalfw-event--timezones (maccalfw-timezones)
       maccalfw-event--default-timezone
       (cl-find-if
        (lambda (x) (plist-get (cdr x) :default))
        maccalfw-event--timezones)))

    (let* ((options (mapcar
                     (lambda (x)
                       `(item :tag ,(format "%s (%s)"
                                            (car x)
                                            (plist-get (cdr x) :abbrev))
                              :value ,(car x)
                              :details x))
                     maccalfw-event--timezones)))
      (apply
       'maccalfw-event--create-wid
       'timezone
       'menu-choice
       :notify #'maccalfw-event--timezone-widget-notify
       :tag "Timezone"
       :format "%[%t%]: %v\n\n"
       :value timezone
       :old-value timezone
       options))

    (maccalfw-event--create-wid
     'location
     'editable-field
     :keymap maccalfw-event-field-map
     :format
     (concat
      (propertize "Location: " 'face 'maccalfw-event-field-names)
      "%v\n")
     (or (plist-get event :location) ""))

    (when-let (stat (plist-get event :status))
      (widget-insert
       (propertize "Status: " 'face 'maccalfw-event-field-names)
       (symbol-name stat) "\n\n"))

    (when-let (org (plist-get event :organizer))
      (widget-insert
       (propertize "Organizer: " 'face 'maccalfw-event-field-names)
       org "\n\n"))

    (maccalfw-event--create-wid
     'availability
     'radio-button-choice
     :entry-format "  %b %v "
     :inline t
     :format "%v\n\n"
     :value (or (plist-get event :availability) 'busy)
     '(item :format "%[Tentative%] " :value tentative)
     '(item :format "%[Free%] " :value free)
     '(item :format "%[Busy%] " :value busy)
     '(item :format "%[Unavailable%] " :value unavailable))

    (maccalfw-event--create-wid
     'url
     'editable-field
     :keymap maccalfw-event-field-map
     :format
     (concat
      (propertize "URL: " 'face 'maccalfw-event-field-names)
      "%v\n\n")
     (or (plist-get event :url) ""))

    (maccalfw-event--create-wid
     'notes 'text
     :format "%v" ; Text after the field!
     :keymap maccalfw-event-text-map
     :value-face 'maccalfw-event-notes-field
     (or (plist-get event :notes) ""))

    (widget-setup)

    (let ((widgets (maccalfw-event--get-widgets)))
      (maccalfw-event--all-day-notify (maccalfw-event--find-widget 'all-day
                                                                   widgets))
      (cl-loop
       for child in (widget-get
                     (maccalfw-event--find-widget 'availability widgets)
                     :children)
       do
       (widget-put child :tab-order -1)))

    (goto-char (point-min))
    (widget-move 1) ;; Go to next widget (should be title)
    (widget-end-of-line) ;; Go to end of line

    (when (plist-get event :read-only)
      (maccalfw-event--make-inactive))))

(provide 'maccalfw)
;;; maccalfw.el ends here
