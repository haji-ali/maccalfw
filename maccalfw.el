;;; maccalfw.el --- Calendar view for Mac Calendars -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Al Haji-Ali

;; Author: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (calfw "1.7"))
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
(require 'org)

(defcustom maccalfw-event-save-hook nil
  "Hook called when an event is saved successfully.
Takes one argument which is the new event data."
  :type 'hook
  :group 'maccalfw)

(defvar maccalfw-event-modify-future-events-p 'ask
  "If non-nil, modifying events with recurrences applies to future events.
Special value \\='ask, prompts the user.")

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
                                   (locate-library "maccalfw")))
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
  (let* ((start (decode-time (plist-get event :start)))
        (end (decode-time (plist-get event :end)))
         (all-day-p (plist-get event :all-day-p))
         (args
          (list
     :start-date  (maccalfw--decode-date start)
     :start-time  (unless all-day-p
                    (maccalfw--decode-time start))
     :end-date    (when all-day-p
                    (maccalfw--decode-date end))
     :end-time    (unless all-day-p
                    (maccalfw--decode-time end))
     :title       (plist-get event :title)
     :location    (plist-get event :location)
           :description (plist-get event :summary))))
    (when (and (alist-get 'status (cl-struct-slot-info 'cfw:event))
               (alist-get 'data (cl-struct-slot-info 'cfw:event)))
      (setq args
            (append args (list
     :status       (plist-get event :status)
                          :data        event))))
    (apply 'make-cfw:event args)))

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

(cl-defun maccalfw--diff-plist (A B
                                  &key
                                  symmetric
                                  test
                                  test-plist)
  "Compare plists A and B and return differing properties.
If SYMMETRIC t, return properties in B not in A. TEST-PLIST is a
plist mapping property names to comparison functions, or defaults
to TEST or \\='eq for all properties. Returns a cons of A and B
values which are different, or nil if no values are different."
  (let ((test (or test 'eq))
        (result-A nil)
        (result-B nil)
        (AA A))
    ;; Process A
    (while A
      (let* ((key (car A))
             (value-a (cadr A))
             (value-b (plist-get B key))
             (cmp (or (plist-get test-plist key) test)))
        (unless (funcall cmp value-a value-b)
          (setq result-A (append result-A (list key value-a)))
          (setq result-B (append result-B (list key value-b)))))
      (setq A (cddr A)))
    ;; Process B for keys not in A
    (while (and symmetric B)
      (let* ((key (car B))
             (value-b (cadr B))
             (cmp (or (plist-get test-plist key) test)))
        (unless (or (plist-get AA key) ;; was processed already
                    (funcall cmp nil value-b))
          (setq result-A (append result-A (list key nil)))
          (setq result-B (append result-B (list key value-b)))))
      (setq B (cddr B)))
    (if (or result-A result-B)
        (list result-A result-B)
      nil)))

(defun maccalfw-event-save (&optional duplicate)
  "Save event.
If DUPLICATE is non-nil, save the event as a new one."
  (interactive "P")
  (let* ((widgets (maccalfw-event--get-widgets))
         (title-wid (maccalfw-event--find-widget 'title widgets))
         (old-data (widget-get title-wid :event-data))
         (tz (maccalfw-event--value 'timezone widgets))
         (all-day (maccalfw-event--value 'all-day widgets))
         (start (maccalfw-event--parse-datetime
                 (if all-day
                     "00:00"
                   (maccalfw-event--value 'start-time widgets))
                 (maccalfw-event--value 'start-date widgets)
                 tz))
         (end (maccalfw-event--parse-datetime
               (if all-day
                   "23:59:59"
                 (maccalfw-event--value 'end-time widgets))
               (if all-day
                   (maccalfw-event--value 'end-date widgets)
                 (maccalfw-event--value 'start-date widgets))
               tz))
         (old-id (unless duplicate (plist-get old-data :id)))
         (new-data
          (list
           :id old-id
           :calendar-id (maccalfw-event--value 'calendar-id widgets)
           :title (widget-value title-wid)
           :timezone (if all-day "" tz)
           :all-day-p all-day
           :url (maccalfw-event--value 'url widgets)
           :recurrence (when (maccalfw-event--value 'recurrence-p widgets)
                         (list (maccalfw-event--value 'recurrence widgets)))
           :location (maccalfw-event--value 'location widgets)
           :availability (maccalfw-event--value 'availability widgets)
           :notes (maccalfw-event--value 'notes widgets)))
         (new-event (null old-id)))
    (when (plist-get old-data :read-only)
      (user-error "Event is not editable.?"))

    ;; Only keep modified data
    (cl-flet ((str-null-p (x)
                (if (stringp x) (string= x "") (null x))))
      (setq new-data
            (car
             (maccalfw--diff-plist
              new-data
              old-data
              :test (lambda (x y)
                      ;; If there's no old event, then as long as x is
                      ;; non-nil, consider it unique.
                      ;; If there's an old event, then consider it unique
                      ;; Either x is nil (or empty) and y is not nil.
                      ;; or x is not nil (nor empty) and different
                      ;; from y.
                      (and
                       (or (not new-event) (str-null-p x))
                       (or new-event
                           (and (or (not (str-null-p x)) (not y))
                                (or (str-null-p x) (equal x y))))))
              :test-plist `(:recurrence
                            ,(lambda (x y)
                               (not (maccalfw--diff-plist
                                     (car-safe x)
                                     (car-safe y)
                                     :test 'seq-set-equal-p
                                     :test-plist
                                     '(;
                                       ;; ignore this value
                                       :week-first-day always
                                       :end-date equal
                                       :occurrence-count eq
                                       :interval eq
                                       :frequency eq)))))))))
    (unless (and (not new-event)
                 (time-equal-p start (plist-get old-data :start)))
      (setq new-data (plist-put new-data :start start)))
    (unless (and (not new-event)
                 (time-equal-p end (plist-get old-data :end)))
      (setq new-data
            (plist-put new-data :end end)))
    (when (and new-data old-id)
      (setq new-data
            (plist-put new-data :id old-id)))
    (if new-data
        ;; if old event has a recurrence, check with use if all future events
        ;; should be editied or just the current one
        (let ((future
               (and (or (plist-get old-data :recurrence)
                        (plist-get new-data :recurrence))
                    ;; If changing recurrence rule, then we should modify all
                    ;; future events. Otherwise, we should ask the user
                    (or (plist-get new-data :recurrence)
                        (maccalfw-event-modify-future-events-p)))))
          (widget-put title-wid
                      :event-data
                      (maccalfw-update-event new-data
                                             (plist-get old-data :start)
                                             future))
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

(defun maccalfw-event-date-field-pick (widget)
  "Open date picker to set the value of WIDGET.
WIDGET defaults to the one at `(point)' if it is for a date.
Otherwise, the widgets for start time/date are set, unless prefix
is given or the widget at (point) is for end time/date, in which
case the end time/date is set."
  (interactive (list
                (let ((wid (widget-at)))
                  (if (and wid
                           (eq (widget-get wid :value-to-external)
                               'maccalfw-event--parse-date-field))
                      wid
                    (if (or current-prefix-arg
                            (when-let ((wid (widget-at (point)))
                                       (key (widget-get wid :field-key)))
                              (member key '(end-time end-date))))
                        'end-time
                      'start-time)))))

  (let ((widgets (maccalfw-event--get-widgets))
        (for-end-date (eq widget 'end-time)))
    (if (widgetp widget)
        (let* ((old-time
                (maccalfw-event--parse-datetime
                 "00:00"
                 (or (widget-value widget)
                     (maccalfw-event--value
                      'start-date widgets)))))
          (widget-value-set widget
                            (org-read-date
                             nil nil nil
                             "Date"
                             old-time)))
      (if (widget-get
           (maccalfw-event--find-widget 'start-time widgets)
           :inactive)
          (maccalfw-event-read-only)
        (let* ((start-time-wid (maccalfw-event--find-widget 'start-time
                                                            widgets))
               (start-date-wid (maccalfw-event--find-widget 'start-date
                                                            widgets))
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
               ;; Define these two to make sure they are bound for
               ;; `org-read-date'
               org-time-was-given
               org-end-time-was-given
               (new-time (org-read-date
                          (not (widget-value all-day-wid))
                          t
                          nil
                          (if for-end-date
                              "End"
                            "Start")
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
                (widget-value-set
                 end-time-wid
                 (maccalfw-event--format-time new-time))))))))))

(defun maccalfw-event-open (event)
  "Open a buffer to display the details of EVENT."
  (pop-to-buffer (generate-new-buffer "*calender event*"))
  (maccalfw-event-mode)
  (maccalfw-event-rebuild-buffer event t))

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

(defun maccalfw-event-modify-future-events-p (&optional prompt)
  "Return non-nil if modification should affect all future events.
Check the value of the variable
`maccalfw-event-modify-future-events-p', and potentially prompt
the user, displaying the message PROMPT."
  (if (eq maccalfw-event-modify-future-events-p 'ask)
      (let ((response
             (cadr
              (read-multiple-choice
               (format (or prompt "Which events to modify?")
                       (buffer-name))
               '((?f "future" "Modification applies to all future events.")
                 (?c "current" "Modification applies only to current event."))
               nil nil (and (not use-short-answers)
                            (not (use-dialog-box-p)))))))
        (equal response "future"))
    maccalfw-event-modify-future-events-p))

(defun maccalfw-event-delete-event (event)
  "Delete calfw EVENT."
  (interactive
   (list (or (get-text-property (point) 'cfw:event)
             (error "No event at location"))))
  (or (prog1 (maccalfw-remove-event
              (plist-get (cfw:event-data event) :id)
              (plist-get (cfw:event-data event) :start)
              (maccalfw-event-modify-future-events-p))
        (message "Event deleted")
        (cfw:refresh-calendar-buffer nil))
      (error "Deleting event failed")))

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
             overlay)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char from)
            ;; Hide any space characters until the beginning of the line, if
            ;; no other text appears
            (while (looking-back " " nil)
              (backward-char))
            (when (looking-back "\n" nil)
              (setq from (point)))))
        (setq overlay (make-overlay from to nil t nil))
        (cl-loop for (key val) on
                 props by #'cddr
                 do (overlay-put overlay key val))
        (widget-put widget key overlay)))))

(defun maccalfw-event--make-widget-untabbale (widget untabbable)
  "Make a WIDGET tabbable or not based on UNTABBABLE.

Recursively go childeren and buttons inside WIDGET. Hidden
children and children of `radio-button-choice' widgets
\(typically labels) are always made untabbable regardless of the
value of UNTABBABLE."
  ;; Need to check if any of the parents are hidden
  (widget-put widget :tab-order (when untabbable -1))
  ;; Do the same to child widgets
  (cl-loop for
           (lst untabbable) in
           (list (list (widget-get widget :children)
                       (or
                        (eq (widget-type widget) 'radio-button-choice)
                        untabbable))
                 ;; Some widgets have buttons, which are not
                 ;; children. Make these untabbable as well
                 (list (widget-get widget :buttons)
                       untabbable))
           do
           (cl-loop for child in lst
                    do
                    (maccalfw-event--make-widget-untabbale
                     child
                     (or
                      (widget-get child :hidden)
                      untabbable)))))

(defun maccalfw-event--show-hide-widget (widget visible)
  "Show/hide WIDGET based on value of VISIBLE.
Also make it untabbable if hidden."
  (maccalfw-event--widget-overlay
   widget
   :hidden visible
   'evaporate t
   'priority 101
   'invisible (not visible))

  ;; Make widget untabbable if hidden, or any of its parents are hidden
  (maccalfw-event--make-widget-untabbale
   widget
   (or (not visible)
       (cl-some
        'identity
        (cl-loop
         with parent = widget
         while (setq parent (widget-get parent :parent))
         collect (widget-get parent :hidden))))))

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

(defun maccalfw-event--value (key widgets)
  "Get value of widget field corresponding to KEY in WIDGETS."
  (widget-value (maccalfw-event--find-widget key widgets)))

(defun maccalfw-event--find-widget (key widgets)
  "Find widget field corresponding to KEY in WIDGETS."
  (cl-find-if
   (lambda (x) (eq key (widget-get x :field-key))) widgets))

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

(defun maccalfw-event--parse-datetime (time-str date-str &optional timezone)
  "Parse time and return the time in the default-time zone.
Time is in DATE-STR and TIME-STR is assumed to be in a given
TIMEZONE."
  (let ((tz (and timezone (alist-get timezone maccalfw-event--timezones
                                     nil nil #'equal)))
        (time (encode-time
               (parse-time-string (format "%s %s" time-str date-str)))))
    (if tz
        (time-add
         time
         (-
          (plist-get (cdr maccalfw-event--default-timezone) :offset)
          (plist-get (alist-get timezone maccalfw-event--timezones
                                nil nil #'equal)
                     :offset)))
      time)))

(defun maccalfw-event--parse-integer-field (_widget value)
  "Parse VALUE of WIDGET as an integer."
  (unless (string-empty-p value)
    (string-to-number value)))

(defun maccalfw-event--parse-integer-list-field (_widget value)
  "Parse VALUE of WIDGET as a list of integers delimited by non-numbers."
  (unless (string-empty-p value)
    ;; TODO: This doesn't resolve cases such as "1-2"
    (mapcar
     'string-to-number
     (string-split value "[^-[:digit:]]+" t))))

(defun maccalfw-event--parse-date-field (_widget value)
  "Parse VALUE of WIDGET as a date."
  (unless (string-empty-p value)
    (parse-time-string value)))

(defun maccalfw-event--timezone-widget-notify (widget &rest _)
  "Action for timezone action.
Assumes that WIDGET has an additional attributes `:old-value'
which is the old value of the timezone (will be updated in this
function)."
  (let ((widgets (maccalfw-event--get-widgets)))
    (unless (maccalfw-event--value 'all-day widgets)
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

(defun maccalfw-event--checkbox-hs (t-widgets &optional nil-widgets)
  "Construct a value for `:hs' suitable for a checkbox.
See `maccalfw-event--hs-action'. This function returns the `:hs'
value such that T-WIDGETS are shown when the checkbox is checked
and NIL-WIDGETS are shown when the checkbox is unchecked."
  `(((identity) . ,t-widgets)
    ((null) . ,nil-widgets)))

(defun maccalfw-event--hs-action (widget &rest _)
  "Action for all-day WIDGET.
Uses widget attribute `:hs' to determine which widgets to toggle.
`:hs' can be a list containing items of the form `(VAL .
WIDGET-ID)' where WIDGET-ID is a single widget ID or a list of
widgets IDs that are made visible when the value is VAL, and are
hidden otherwise. If VAL is a cons, its car is treated as a
function name that is called with the value to check for
visibility, while its cons are used as the remaining arguments of
the function call.

See `maccalfw-event--checkbox-hs' for constructing `:hs' for a
checkbox."
  (let ((val (widget-value widget))
        (hs (widget-get widget :hs))
        (widgets (maccalfw-event--get-widgets))
        wid-all)
    (cl-loop for rule in hs
             for valchk = (car rule)
             for valeq = (if (consp valchk)
                             (apply (car valchk) val (cdr valchk))
                           (eq val valchk))
             for wid-ids = (ensure-list (cdr rule))
             do
             (cl-loop for wid-id in wid-ids
                      do
                      (setf
                       (alist-get wid-id wid-all)
                       (cons valeq
                             (alist-get wid-id wid-all))))
             finally
             (cl-loop for (wid-id . vis) in wid-all
                      for wid = (maccalfw-event--find-widget wid-id
                                                             widgets)
                      when wid
                      do
                      (maccalfw-event--show-hide-widget
                       wid (cl-some 'identity vis))))))

(defun maccalfw-event-rebuild-buffer (&optional event no-erase)
  "Rebuild buffer of maccalfw EVENT.
If NO-ERASE is non-nil, do not reset the buffer before rebuilding
it."
  (interactive
   (let* ((widgets (maccalfw-event--get-widgets))
          (title-wid (maccalfw-event--find-widget 'title widgets))
          (event (widget-get title-wid :event-data)))
     (list (or (maccalfw-get-event (plist-get event :id))
               event)
           nil)))
  (when (and
         (derived-mode-p 'maccalfw-event-mode)
         (or (not (buffer-modified-p))
             (kill-buffer--possibly-save (current-buffer))))
    (unless no-erase
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (kill-all-local-variables)
        (erase-buffer)
        (delete-all-overlays)))

    (maccalfw-event-mode)
    (maccalfw-event--create-form event)

    (setq-local
     header-line-format
     (substitute-command-keys
      "\\<maccalfw-event-mode-map>Event details. \
Save `\\[maccalfw-event-save]', \
abort `\\[maccalfw-event-kill]'."))
    (set-buffer-modified-p nil)))

(defun maccalfw-event--create-form (event)
  "Create form in current buffer corresponding to EVENT."
  (let ((timezone (or (plist-get event :timezone)
                      (car maccalfw-event--default-timezone))))
    (widget-insert "\n\n")

    (widget-create 'editable-field
                   :field-key 'title
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
       'widget-create
       'menu-choice
       :field-key 'calendar-id
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

    (widget-create 'editable-field
                   :field-key 'start-date
                   :keymap maccalfw-event-field-map
                   :format " %v "
                   :size 10
                   (and event
                        (format-time-string "%F"
                                            (plist-get event :start))))

    (widget-create 'editable-field
                   :field-key 'end-date
                   :keymap maccalfw-event-field-map
                   :format "  --    %v   "
                   :size 10
                   (format-time-string "%F"
                                       (plist-get event :end)))
    (widget-create 'editable-field
                   :field-key 'start-time
                   :keymap maccalfw-event-field-map
                   :format " %v -- "
                   :size 6
                   (maccalfw-event--format-time
                    (plist-get event :start)
                    timezone))
    (widget-create 'editable-field
                   :field-key 'end-time
                   :keymap maccalfw-event-field-map
                   :format " %v   "
                   :size 6
                   (maccalfw-event--format-time
                    (plist-get event :end)
                    timezone))
    (widget-create 'checkbox
                   :field-key 'all-day
                   :format " %[%v%] All day\n\n"
                   :notify #'maccalfw-event--hs-action
                   :hs (maccalfw-event--checkbox-hs
                        'end-date
                        '(start-time end-time timezone))
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
       'widget-create
       'menu-choice
       :field-key 'timezone
       :notify #'maccalfw-event--timezone-widget-notify
       :tag "Timezone"
       :format "%[%t%]: %v\n\n"
       :value timezone
       :old-value timezone
       options))

    (widget-create
     'radio-button-choice
     :field-key 'availability
     :entry-format "%b %v "
     :format "%v\n\n"
     :value (or (plist-get event :availability) 'busy)
     '(item :format "%[Tentative%] " :value tentative)
     '(item :format "%[Free%] " :value free)
     '(item :format "%[Busy%] " :value busy)
     '(item :format "%[Unavailable%] " :value unavailable))

    (widget-create
     'editable-field
     :field-key 'location
     :keymap maccalfw-event-field-map
     :format
     (concat
      (propertize "Location: " 'face 'maccalfw-event-field-names)
      "%v\n")
     (or (plist-get event :location) ""))

    (let* ((recur (car-safe (plist-get event :recurrence)))
           (group-items
            (list
             `(editable-field
               :field-key recurrence-interval
               :value-to-external maccalfw-event--parse-integer-field
               :keymap maccalfw-event-field-map
               :format "every %v "
               :size 5
               ,(or (when-let (interval
                               (plist-get recur :interval))
                      (format "%d" interval))
                    "1"))

             `(radio-button-choice
               :field-key recurrence-frequency
               :entry-format "%b %v"
               :format "%v\n"
               :hs ((weekly . recurrence-week-days)
                    (monthly recurrence-week-days
                             recurrence-month-days)
                    (yearly recurrence-week-days
                            recurrence-year-months
                            recurrence-year-weeks
                            recurrence-year-days))
               :notify maccalfw-event--hs-action
               :value ,(or (plist-get recur :frequency)
                           'weekly)
               (item :format "%[Day%] " :value daily)
               (item :format "%[Week%] " :value weekly)
               (item :format "%[Month%] " :value monthly)
               (item :format "%[Year%] " :value yearly))

             (append
              `(checklist
                :field-key recurrence-week-days
                :indent 3
                :format "on %v\n"
                :value-to-external
                (lambda (widget value)
                  (cl-loop for v in value
                           collect (list :week-day v)))
                :value ,(cl-loop for day in (plist-get recur :week-days)
                                 collect (plist-get day :week-day)))
              (cl-loop
               with lst = '(sunday monday tuesday wednesday thursday friday
                                   saturday)
               for w in lst
               collect `(item :format "%t "
                              :tag ,(capitalize (substring (symbol-name w) 0 3))
                              ,w)))

             `(editable-field
               :field-key recurrence-month-days
               :value-to-external maccalfw-event--parse-integer-list-field
               :keymap maccalfw-event-field-map
               :format "on days of month [-31 to 31]: %v\n"
               :size 10
               ,(or (when-let (mdays (plist-get recur :month-days))
                      (string-join (cl-loop for i in mdays
                                            collect (number-to-string i))
                                   ", "))
                    ""))
             (append
              `(checklist
                :field-key recurrence-year-months
                :format "on %v\n"
                :value ,(plist-get recur :year-months))
              (cl-loop
               with lst = parse-time-months
               with len = (1+ (length lst))
               for w in lst
               for idx from 0
               while (< idx (/ len 2))
               collect `(item :format "%t "
                              :tag ,(capitalize (car w))
                              ,(cdr w))))
             `(editable-field
               :field-key recurrence-year-weeks
               :value-to-external maccalfw-event--parse-integer-list-field
               :keymap maccalfw-event-field-map
               :format "on weeks of year [-53 to 53]: %v\n"
               :size 10
               ,(or (when-let (mdays (plist-get recur :year-weeks))
                      (string-join (cl-loop for i in mdays
                                            collect (number-to-string i))
                                   ", "))
                    ""))
             `(editable-field
               :field-key recurrence-year-days
               :value-to-external maccalfw-event--parse-integer-list-field
               :keymap maccalfw-event-field-map
               :format "on days of year [-366 to 366]: %v\n"
               :size 10
               ,(or (when-let (mdays (plist-get recur :year-days))
                      (string-join (cl-loop for i in mdays
                                            collect (number-to-string i))
                                   ", "))
                    ""))
             `(radio-button-choice
               :field-key recurrence-end-rule
               :do-not-save t
               :entry-format "%b %v"
               :format "%v"
               :hs ((on . recurrence-end-date)
                    (after . recurrence-occurrence-count))
               :notify maccalfw-event--hs-action
               :value ,(or (and (plist-get recur :end-date) 'on)
                           (and (plist-get recur :occurrence-count) 'after))
               (item :format "%[Until%] " :value on)
               (item :format "%[After%] " :value after))

             `(editable-field
               :field-key recurrence-end-date
               :value-to-external maccalfw-event--parse-date-field
               :keymap maccalfw-event-field-map
               ;; additional space is needed, otherwise :from and :to of the widget
               ;; change as text is added to it
               :format " %v "
               :size 10
               ,(or (when-let (end-date (plist-get recur :end-date))
                      (format-time-string "%F" end-date))
                    ""))

             `(editable-field
               :field-key recurrence-occurrence-count
               :value-to-external maccalfw-event--parse-integer-field
               :keymap maccalfw-event-field-map
               :format " %v occurrences"
               :size 5
               ,(or (when-let (occurrence-count
                               (plist-get recur :occurrence-count))
                      (format "%d" occurrence-count))
                    ""))
             ;; TODO: Unimplemented features:
             ;; - How do we handle :week-number?
             ;; - set-positions.
             ;; - week-first-day? Indicates which day of the week the recurrence
             ;; rule treats as the first day of the week.
             ))
           (group-value
            (cl-loop for x in group-items
                     collect
                     (if (eq (car x) 'editable-field)
                         (car (last x))
                       (plist-get (cdr x) :value)))))
      (widget-create
       'checkbox
       :field-key 'recurrence-p
       :format "%[%v%] Repeat "
       :notify #'maccalfw-event--hs-action
       :hs (maccalfw-event--checkbox-hs 'recurrence)
       (plist-get event :recurrence))

      (apply 'widget-create
             'group
             :format (concat (propertize ":" 'display "") "%v")
             :field-key 'recurrence
             :value-to-external
             (lambda (widget _value)
               (unless (widget-get widget :hidden)
                 (cl-loop
                  with value = nil
                  for child in (widget-get widget :children)
                  for field-key = (widget-get child :field-key)
                  when (and field-key
                            (not (widget-get child :do-not-save))
                            ;; If it's hidden, it shouldn't be part of the
                            ;; value.
                            (not (widget-get child :hidden)))
                  do (setq value (widget-value child))
                  and when value
                  nconc
                  (list
                   (intern (concat ":" (string-trim-left
                                        (symbol-name field-key)
                                        "recurrence-")))
                   value))))
             :indent 3
             ;; We have to set the group value here because otherwise the
             ;; checklists are not set correctly. This is because the group
             ;; value is nil by default which leads to resetting of all
             ;; checklists. See bug#75171
             :value group-value
             group-items))

    (when-let (stat (plist-get event :status))
      (widget-insert
       (propertize "Status: " 'face 'maccalfw-event-field-names)
       (symbol-name stat) "\n\n"))

    (when-let (org (plist-get event :organizer))
      (widget-insert
       (propertize "Organizer: " 'face 'maccalfw-event-field-names)
       org "\n\n"))

    (widget-insert "\n\n")

    (widget-create
     'editable-field
     :field-key 'url
     :keymap maccalfw-event-field-map
     :format
     (concat
      (propertize "URL: " 'face 'maccalfw-event-field-names)
      "%v\n\n")
     (or (plist-get event :url) ""))

    (widget-create
     'text
     :field-key 'notes
     :format "%v" ; Text after the field!
     :keymap maccalfw-event-text-map
     :value-face 'maccalfw-event-notes-field
     (or (plist-get event :notes) ""))

    (widget-setup)

    (let ((widgets (maccalfw-event--get-widgets)))
      ;; Call every hide-show notification so we have the correct initial
      ;; state.
      (cl-loop for wid in widgets
               for notify = (widget-get wid :notify)
               when (eq notify 'maccalfw-event--hs-action)
               do (funcall notify wid))

      ;; This causes all lists of radio buttons to skip text when tabbing,
      ;; instead just going through the buttons
      (cl-loop for wid in widgets
               when (eq (widget-type wid) 'radio-button-choice)
               do
               (cl-loop for child in (widget-get wid :children)
                        do (widget-put child :tab-order -1))))
    (goto-char (point-min))
    (widget-move 1) ;; Go to next widget (should be title)
    (widget-end-of-line) ;; Go to end of line

    (when (plist-get event :read-only)
      (maccalfw-event--make-inactive))))

(defun maccalfw-event-data ()
  "Return event data of current event."
  (let* ((widgets (maccalfw-event--get-widgets))
         (title-wid (maccalfw-event--find-widget 'title widgets)))
    (widget-get title-wid :event-data)))

(defun maccalfw-event-duplicate ()
  "Duplicate current event.
Should be called on an event-details buffer. Make the event
editable and remove ID information so that the event will be
treated as new when saved."
  (interactive)
  (let* ((data (maccalfw-event-data)))
    (plist-put data :id nil)
    (plist-put data :read-only nil)

    ;; If calendar is read-only, we'll an error will be issued when trying to
    ;; save. So might as well not change it.

    ;; reactivate form
    (maccalfw-event--make-inactive t)))

(provide 'maccalfw)
;;; maccalfw.el ends here
