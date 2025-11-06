;;; ical-form.el --- A widget form for editing ical events -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Al Haji-Ali

;; Author: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Created: 2023
;; Version: 0.2
;; Package-Requires: ((emacs "29.1"))
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

;; This package creates a widget form for editing ical events.

;;; Code:
(require 'wid-edit)
(require 'org)

(defcustom ical-form-event-updated-hook nil
  "Hook called when an event is updated successfully.
Takes two arguments, the first is the old event data and the
second is the new event data."
  :type 'hook
  :group 'ical-form)

(defcustom ical-form-render-html-p #'ignore
  "A function to detect if an event content should be rendered in shr."
  :type 'function
  :group 'ical-form)

(defcustom ical-form-update-event-function
  (lambda (&rest _) (error "`ical-form-update-event-function' not set correctly."))
  "Function to call to update/create event.

Expected arguments are (OLD-DATA CHANGED-DATA) where OLD-DATA is
the event OLD-DATA, if any. CHANGED-DATA contain the fields that
were modified, relative to the old values."
  :type 'function
  :group 'ical-form)

(defface ical-form-notes-field
  '((t
     :inherit widget-field
     :box nil))
  "Face used for editable fields."
  :group 'ical-form)

(defface ical-form-title-field
  '((t
     :inherit widget-field
     :height 1.8
     :weight bold
     :family sans-serif))
  "Face used for editable fields."
  :group 'ical-form)

(defface ical-form-field-names
  '((t
     :weight bold))
  "Face used for field names."
  :group 'ical-form)

(defvar-keymap ical-form--custom-map
  :doc "Keymap including custom bindings for `ical-form'."
  "C-c C-k" #'ical-form-kill
  "C-c C-s" #'ical-form-date-field-pick
  "C-c C-w" #'ical-form-save
  "C-x C-s" #'ical-form-save)

(defvar-keymap ical-form-mode-map
  :doc "Keymap for `ical-form'."
  :parent (make-composed-keymap
           ical-form--custom-map
           widget-keymap))

(defvar-keymap ical-form-field-map
  :doc "Keymap for fields in `ical-form'."
  :parent (make-composed-keymap
           ical-form--custom-map
           widget-field-keymap))

(defvar-keymap ical-form-text-map
  :doc "Keymap for text fields in `ical-form'."
  :parent (make-composed-keymap
           ical-form--custom-map
           widget-text-keymap))

(define-derived-mode ical-form-mode fundamental-mode "Calendar Event"
  "Major mode for editing calendar events."
  :lighter " Calfw event"
  (use-local-map ical-form-mode-map)
  (make-local-variable 'kill-buffer-query-functions)
  (add-to-list 'kill-buffer-query-functions #'ical-form-save-maybe))

(defvar-local ical-form--timezones nil)
(defvar-local ical-form--calendars nil)
(defvar-local ical-form--default-timezone nil)


;; These need to be dynamically bound when using `org-pick-date'
(defvar org-time-was-given)
(defvar org-end-time-was-given)


(defun ical-form--parse-ical-date (ical-list)
  "Parse an iCal list format ICAL-LIST into date components.
Returns a list (DATE (ALL-DAY-P val) (TZ val))."
  (let ((params (car ical-list))
        (date-string (cadr ical-list)))
    (let* ((is-all-day (and params
                            (eq (car params) 'VALUE)
                            (equal (cadr params) "DATE")))
           (time-zone-id (and params
                              (eq (car params) 'TZID)
                              (cadr params)))
           (parsed-date (parse-time-string
                         (concat
                          date-string
                          (if is-all-day
                              "T000000"
                            "")))))
      (if parsed-date
          (list (apply #'encode-time parsed-date)
                `(ALL-DAY-P . ,is-all-day)
                `(TZID . ,time-zone-id))
        (error "Failed to parse iCal list")))))

(defun ical-form--format-ical-date (date &optional all-day time-zone-id)
  "Convert DATE to iCal list format.
If ALL-DAY is non-nil, return all-day format. TIME-ZONE-ID
specifies the timezone."
  (let* ((params (cond
                  (all-day (list 'VALUE "DATE"))
                  (time-zone-id (list 'TZID time-zone-id))
                  (t nil)))
         (format (cond
                  (all-day "%Y%m%d")
                  (time-zone-id "%Y%m%dT%H%M%S")
                  (t "%Y%m%dT%H%M%SZ")))
         (time-string (format-time-string format date)))
    (list params time-string)))

(defun ical-form--parse-ical-rrule (ical-list)
  "Parse an iCal list format ICAL-LIST into date components.
Returns a list (DATE IS-ALL-DAY TIME-ZONE)."
  (when ical-list
  (let ((trim "[[:space:]]*"))
    (append
     (list 'rrule)
     (mapcar
      (lambda (item)
        (let* ((key-val (string-split item "=" t trim))
               (value (string-join (cdr key-val) "="))
               (key (intern (car key-val))))
          (cons key
                (cl-case key
                  (UNTIL (encode-time (parse-time-string value)))
                  (FREQ (intern (downcase value)))
                  ((COUNT INTERVAL)
                   (string-to-number value))
                  (BYDAY
                   (mapcar
                    (lambda (x)
                      (save-match-data
                        (string-match
                         "^\\(?:\\([-+]?[[:digit:]]+\\)\\)?\\([A-Z]+\\)$"
                         x)
                        (cons (intern (match-string 2 x))
                              (when (match-string 1 x)
                                (string-to-number (match-string 1 x))))))
                    (string-split value "," t trim)))
                  (otherwise (mapcar
                              #'string-to-number
                              (string-split value "," t trim)))))))
        (and (cadr ical-list)
             (string-split (cadr ical-list) ";" t trim)))))))

(defun ical-form-event-get (event prop &optional subprop)
  "Get property PROP from EVENT.

If SUBPROP is nil, return the element value of PROP. If SUBPROP
is t, return (PROP-VALUE ALIST) where ALIST is a a list of
subproperties. Otherwise, return value corresponding to SUBPROP
from ALIST."
  (when-let (prop-val (alist-get prop event))
    (let* ((parse-quote-string (lambda (x) (list (intern
                                                  (downcase (cadr x))))))
           (trans
            (or (alist-get
                 prop
                 `((DTSTART . ical-form--parse-ical-date)
                   (DTEND . ical-form--parse-ical-date)
                   (DTSTAMP . ical-form--parse-ical-date)
                   (RRULE . ical-form--parse-ical-rrule)
                   (STATUS . ,parse-quote-string)
                   (X-EMACS-AVAILABILITY . ,parse-quote-string)))
                (lambda (x) (list (cadr x)))))
           (prop-val (funcall trans prop-val)))
      (if (eq subprop 't)
          prop-val
        (if subprop
            (alist-get subprop (cdr prop-val))
          (car prop-val))))))


(defun ical-form-kill ()
  "Kill event buffer.
Warn if the buffer is modified and offer to save."
  (interactive)
  (when (ical-form-save-maybe)
    ;; If `ical-form-save-maybe' return t, then ignore modifications
    (set-buffer-modified-p nil)
    (quit-window t)))

(cl-defun ical-form--diff-plist (A B
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

(cl-defun ical-form--diff-alist (A B
                                        &key
                                        symmetric
                                        test
                                        test-plist)
  "Compare alists A and B and return differing properties.
If SYMMETRIC t, return properties in B not in A. TEST-PLIST is a
plist mapping property names to comparison functions, or defaults
to TEST or \\='eq for all properties. Returns a cons of A and B
values which are different, or nil if no values are different."
  (let ((test (or test 'equal))
        (result-A nil)
        (result-B nil)
        (AA A))
    ;; Process A
    (while A
      (let* ((kv (car A))
             (key (car kv))
             (value-a (cdr kv))
             (value-b (alist-get key B))
             (cmp (or (plist-get test-plist key) test)))
        (unless (funcall cmp value-a value-b)
          (setq result-A (append result-A (list (cons key value-a))))
          (setq result-B (append result-B (list (cons key value-b))))))
      (setq A (cdr A)))
    ;; Process B for keys not in A
    (while (and symmetric B)
      (let* ((kv (car B))
             (key (car kv))
             (value-b (cdr kv))
             (cmp (or (plist-get test-plist key) test)))
        (unless (or (alist-get key AA) ;; was processed already
                    (funcall cmp nil value-b))
          (setq result-A (append result-A (list (cons key nil))))
          (setq result-B (append result-B (list (cons key value-b))))))
      (setq B (cddr B)))
    (if (or result-A result-B)
        (list result-A result-B)
      nil)))

(defun ical-form-save (&optional duplicate)
  "Save event.
If DUPLICATE is non-nil, save the event as a new one."
  (interactive "P")
  (let* ((widgets (ical-form--get-widgets))
         (title-wid (ical-form--find-widget 'title widgets))
         (old-data (widget-get title-wid :event-data))
         (tz (ical-form--value 'timezone widgets))
         (all-day (ical-form--value 'all-day widgets))
         (start (ical-form--parse-datetime
                 (if all-day
                     "00:00"
                   (ical-form--value 'start-time widgets))
                 (ical-form--value 'start-date widgets)))
         (end (ical-form--parse-datetime
               (if all-day
                   "23:59:59"
                 (ical-form--value 'end-time widgets))
               (if all-day
                   (ical-form--value 'end-date widgets)
                 (ical-form--value 'start-date widgets))))
         (old-id (unless duplicate
                   (ical-form-event-get old-data 'UID)))
         (new-data
          `((X-EMACS-CALID nil ,(ical-form--value 'calendar-id widgets))
            (SUMMARY nil ,(widget-value title-wid))
            (URL nil ,(ical-form--value 'url widgets))
            (RRULE nil
                   ,(when (ical-form--value 'recurrence-p widgets)
                      (ical-form--value 'recurrence widgets)))
            (LOCATION nil ,(ical-form--value 'location widgets))
            (DESCRIPTION nil ,(ical-form--value 'notes widgets))
            (X-EMACS-AVAILABILITY
             nil
             ,(upcase (symbol-name
                       (ical-form--value 'availability widgets))))))
         (new-event (null old-id)))
    (when (ical-form-event-get old-data 'X-EMACS-READ-ONLY)
      (user-error "Event is not editable.?"))

    (setq new-data
          (append
           new-data
           (list (cons 'DTSTART (ical-form--format-ical-date start all-day tz))
                 (cons 'DTEND (ical-form--format-ical-date end all-day tz)))))


    (cl-flet ((non-trivial-p (x)
                ;; If the list is just nil empty strings, it's trivial
                (cl-some
                 (lambda (z) (if (stringp z) (not (string= z "")) z)) x)))
      (if new-event
          ;; Keep all fields except those which are null
          (setq new-data
                (cl-loop for x in new-data
                         if (non-trivial-p (cdr x))
                         collect x))
        (setq new-data
              (car
               (ical-form--diff-alist
                new-data
                old-data
                :test (lambda (x y)
                        (equal (and (non-trivial-p x) x)
                               (and (non-trivial-p y) y)))
                :test-plist `(RRULE
                              (lambda (x y)
                                (not (ical-form--diff-alist
                                      (cdr (ical-form--parse-ical-rrule x))
                                      (cdr (ical-form--parse-ical-rrule y))
                                      :test #'seq-set-equal-p
                                      :test-plist
                                      '(;
                                        ;; ignore this value
                                        WKST always
                                        UNTIL equal
                                        COUNT eq
                                        INTERVAL eq
                                        FREQ equal))))))))))
    (if new-data
        (progn
          (widget-put title-wid
                      :event-data
                      (funcall
                       ical-form-update-event-function
                       (unless new-event old-data)
                     new-data))
          (when (called-interactively-p 'interactive)
            (message "Event saved."))
          (run-hook-with-args
           'ical-form-event-updated-hook
           old-data
           (widget-get title-wid :event-data)))
      (when (called-interactively-p 'interactive)
        (message "(No changes to event to be saved)")))
    (set-buffer-modified-p nil)))

(defun ical-form-save-maybe ()
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
                          (and (fboundp #'use-dialog-box-p)
                               (not (use-dialog-box-p))))))))
      (if (equal response "no")
          nil
        (unless (equal response "yes")
          (ical-form-save)
          t)
        t))))

(defun ical-form-date-field-pick (widget)
  "Open date picker to set the value of WIDGET.
WIDGET defaults to the one at `(point)' if it is for a date.
Otherwise, the widgets for start time/date are set, unless prefix
is given or the widget at (point) is for end time/date, in which
case the end time/date is set."
  (interactive (list
                (let ((wid (widget-at)))
                  (if (and wid
                           (eq (widget-get wid :value-to-external)
                               #'ical-form--parse-date-field))
                      wid
                    (if (or current-prefix-arg
                            (when-let ((wid (widget-at (point)))
                                       (key (widget-get wid :field-key)))
                              (member key '(end-time end-date))))
                        'end-time
                      'start-time)))))

  (let ((widgets (ical-form--get-widgets))
        (for-end-date (eq widget 'end-time)))
    (if (widgetp widget)
        (let* ((old-time
                (ical-form--parse-datetime
                 "00:00"
                 (or (widget-value widget)
                     (ical-form--value
                      'start-date widgets)))))
          (widget-value-set widget
                            (org-read-date
                             nil nil nil
                             "Date"
                             old-time)))
      (if (widget-get
           (ical-form--find-widget 'start-time widgets)
           :inactive)
          (ical-form-read-only)
        (let* ((start-time-wid (ical-form--find-widget 'start-time
                                                            widgets))
               (start-date-wid (ical-form--find-widget 'start-date
                                                            widgets))
               (end-time-wid (ical-form--find-widget 'end-time widgets))
               (end-date-wid (ical-form--find-widget 'end-date widgets))
               (all-day-wid (ical-form--find-widget 'all-day widgets))
               (all-day-p  (widget-value all-day-wid))
               (start-time
                (ical-form--parse-datetime
                 (if all-day-p
                     "00:00"
                   (widget-value start-time-wid))
                 (widget-value start-date-wid)))
               (end-time
                (ical-form--parse-datetime
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
                                      (ical-form--format-time new-time))
                    (widget-value-set
                     end-time-wid
                     (or org-end-time-was-given
                         (ical-form--format-time
                          (time-add new-time
                                    (time-subtract end-time
                                                   start-time))))))
                ;; for-end-date and range not given
                (widget-value-set
                 end-time-wid
                 (ical-form--format-time new-time))))))))))

(defun ical-form-open (event calendars timezones &optional update-fn)
  "Open a buffer to display the details of EVENT.
CALENDARS and TIMEZONES should be a list of calendars, and
timezones to use in the form.

UPDATE-FN, if provided, is used to set
`ical-form-update-event-function' locally in the new buffer."
  (let ((buf (generate-new-buffer "*calender event*")))
    (pop-to-buffer buf)
    (ical-form-mode)
    (when update-fn
      (setq-local ical-form-update-event-function update-fn))
    (setq
     ical-form--calendars calendars
     ical-form--timezones timezones
     ical-form--default-timezone
     (cl-find-if
      (lambda (x) (plist-get (cdr x) :default))
      ical-form--timezones))
    (ical-form-rebuild-buffer event t)
    buf))

(defun ical-form-read-only (&rest _junk)
  "Ignoring the arguments, signal an error."
  (unless inhibit-read-only
    (error "The event is read-only")))

(defun ical-form--widget-overlay (widget key delete &rest props)
  "Create an overlay around WIDGET, setting its PROPS.
KEY is used to save the overlay in the widget.
If DELETE is non-nil, delete the widget instead."
  (unless (xor (widget-get widget key) delete)
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

(defun ical-form--make-widget-untabbale (widget untabbable)
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
                    (ical-form--make-widget-untabbale
                     child
                     (or
                      (widget-get child :hidden)
                      untabbable)))))

(defun ical-form--show-hide-widget (widget visible)
  "Show/hide WIDGET based on value of VISIBLE.
Also make it untabbable if hidden."
  (ical-form--widget-overlay
   widget
   :hidden visible
   'evaporate t
   'priority 101
   'invisible (not visible))

  ;; Make widget untabbable if hidden, or any of its parents are hidden
  (ical-form--make-widget-untabbale
   widget
   (or (not visible)
       (cl-some
        'identity
        (cl-loop
         with parent = widget
         while (setq parent (widget-get parent :parent))
         collect (widget-get parent :hidden))))))

(defun ical-form--make-inactive (&optional active)
  "Make all widgets in the current buffer inactive.
If ACTIVE is t, activate widgets instead"
  ;; widget-specify-active
  ;; How to properly loop over a plist?
  (save-excursion
    (cl-loop for wid in (ical-form--get-widgets)
             do
             (ical-form--widget-overlay
              wid :inactive active
              'evaporate t
              'priority 100
              'modification-hooks '(ical-form-read-only)))))

(defun ical-form--value (key widgets)
  "Get value of widget field corresponding to KEY in WIDGETS."
  (widget-value (ical-form--find-widget key widgets)))

(defun ical-form--find-widget (key widgets)
  "Find widget field corresponding to KEY in WIDGETS."
  (cl-find-if
   (lambda (x) (eq key (widget-get x :field-key))) widgets))

(defun ical-form--get-widgets ()
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

(defun ical-form--format-time (time &optional timezone)
  "Convert TIME to new TIMEZONE and format it as a string.
Assumes time is in the default timezone."
  (let ((tz (and timezone (alist-get timezone ical-form--timezones
                                     nil nil #'equal))))
    (format-time-string
     "%R"
     (if tz
         (time-add
          time
          (-
           (plist-get tz :offset)
           (plist-get (cdr ical-form--default-timezone) :offset)))
       time))))

(defun ical-form--parse-datetime (time-str date-str &optional timezone)
  "Parse time and return the time in the default-time zone.
Time is in DATE-STR and TIME-STR is assumed to be in a given
TIMEZONE. If TIMEZONE, convert back to default time zone in
`ical-form--default-timezone'."
  (let ((tz (and timezone (alist-get timezone ical-form--timezones
                                     nil nil #'equal)))
        (time (encode-time
               (parse-time-string (format "%s %s" time-str date-str)))))
    (if tz
        (time-add
         time
         (-
          (plist-get (cdr ical-form--default-timezone) :offset)
          (plist-get (alist-get timezone ical-form--timezones
                                nil nil #'equal)
                     :offset)))
      time)))

(defun ical-form--parse-integer-field (_widget value)
  "Parse VALUE of WIDGET as an integer."
  (unless (string-empty-p value)
    (string-to-number value)))

(defun ical-form--parse-integer-list-field (_widget value)
  "Parse VALUE of WIDGET as a list of integers delimited by non-numbers."
  (unless (string-empty-p value)
    ;; TODO: This doesn't resolve cases such as "1-2"
    (mapcar #'string-to-number (string-split value "[^-[:digit:]]+" t))))

(defun ical-form--parse-date-field (_widget value)
  "Parse VALUE of WIDGET as a date."
  (unless (string-empty-p value)
    (encode-time (parse-time-string (format "%s 00:00:00"
                                            value)))))

(defun ical-form--timezone-widget-notify (widget &rest _)
  "Action for timezone action.
Assumes that WIDGET has an additional attributes `:old-value'
which is the old value of the timezone (will be updated in this
function)."
  (let ((widgets (ical-form--get-widgets)))
    (unless (ical-form--value 'all-day widgets)
      (let* ((old-tz (widget-get widget :old-value))
             (tz (widget-value widget)))
        (save-excursion
          (cl-loop for (date-wid . time-wid) in '((start-date . start-time)
                                                  (end-date . end-time))
                   for time-widget = (ical-form--find-widget
                                      time-wid widgets)
                   for date-widget = (ical-form--find-widget
                                      date-wid widgets)
                   do
                   (widget-value-set
                    time-widget
                    (ical-form--format-time
                     (ical-form--parse-datetime
                      (widget-value time-widget)
                      (widget-value date-widget)
                      old-tz)
                     tz)))
          (widget-put widget :old-value tz))))))

(defun ical-form--checkbox-hs (t-widgets &optional nil-widgets)
  "Construct a value for `:hs' suitable for a checkbox.
See `ical-form--hs-action'. This function returns the `:hs'
value such that T-WIDGETS are shown when the checkbox is checked
and NIL-WIDGETS are shown when the checkbox is unchecked."
  `(((identity) . ,t-widgets)
    ((null) . ,nil-widgets)))

(defun ical-form--hs-action (widget &rest _)
  "Action for all-day WIDGET.
Uses widget attribute `:hs' to determine which widgets to toggle.
`:hs' can be a list containing items of the form `(VAL .
WIDGET-ID)' where WIDGET-ID is a single widget ID or a list of
widgets IDs that are made visible when the value is VAL, and are
hidden otherwise. If VAL is a cons, its car is treated as a
function name that is called with the value to check for
visibility, while its cons are used as the remaining arguments of
the function call.

See `ical-form--checkbox-hs' for constructing `:hs' for a
checkbox."
  (let ((val (widget-value widget))
        (hs (widget-get widget :hs))
        (widgets (ical-form--get-widgets))
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
                      for wid = (ical-form--find-widget wid-id
                                                             widgets)
                      when wid
                      do
                      (ical-form--show-hide-widget
                       wid (cl-some #'identity vis))))))

(defun ical-form--html-content-maybe (content)
  "Insert content rendered as HTML using shr.
If the function in `ical-form-render-html-p' returns nil, just
return CONTENT as is, otherwise return a rendering in SHR."
  ;; Inspired by `notmuch-show--insert-part-text/html-shr'
  (if (funcall ical-form-render-html-p content)
      (with-temp-buffer
        (shr-insert-document
         (with-temp-buffer
           (insert content)
           (libxml-parse-html-region (point-min) (point-max))))
        (buffer-substring (point-min) (point-max)))
    content))

(defun ical-form-rebuild-buffer (event &optional no-erase)
  "Rebuild ical-form buffer from EVENT.
If NO-ERASE is non-nil, do not reset the buffer before rebuilding
it."
  (interactive
   (let* ((widgets (ical-form--get-widgets))
          (title-wid (ical-form--find-widget 'title widgets))
          (event (widget-get title-wid :event-data)))
     (list event nil)))
  (let ((timezones ical-form--timezones)
        (calendars ical-form--calendars)
        (default-timezone ical-form--default-timezone)
        (local-update-fn (local-variable-p 'ical-form-update-event-function))
        (update-fn ical-form-update-event-function))
    (when (and
           (derived-mode-p 'ical-form-mode)
           (or (not (buffer-modified-p))
               (ical-form-save-maybe)))
      (unless no-erase
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (kill-all-local-variables)
          (erase-buffer)
          (delete-all-overlays)))

      (ical-form-mode)

      ;; Recover local-update function. We could instead mark the variable
      ;; permanently local. But my thinking is that this variable is
      ;; mode-specific and should not be
      (when local-update-fn
        (setq-local ical-form-update-event-function update-fn))

      (setq
       ical-form--calendars calendars
       ical-form--timezones timezones
       ical-form--default-timezone default-timezone)

      (ical-form--create-form event)

      (setq-local
       header-line-format
       (substitute-command-keys
        "\\<ical-form-mode-map>Event details. \
Save `\\[ical-form-save]', \
abort `\\[ical-form-kill]'."))
      (set-buffer-modified-p nil))))

(defun ical-form--create-form (event)
  "Create form in current buffer corresponding to EVENT."
  (let* ((cal-id (ical-form-event-get event 'X-EMACS-CALID))
         (dt-start (ical-form-event-get event 'DTSTART t))
         (timezones ical-form--timezones)
         (calendars ical-form--calendars)
         (timezone (or (alist-get 'TZID (cdr dt-start))
                       (car-safe ical-form--default-timezone)))
         (all-day-p (alist-get 'ALL-DAY-P (cdr dt-start)))
         (end (ical-form-event-get event 'DTEND)))
    (widget-insert "\n\n")

    (widget-create 'editable-field
                   :field-key 'title
                   :event-data event
                   :keymap ical-form-field-map
                   :value-face 'ical-form-title-field
                   :format "%v \n" ; Text after the field!
                   (or (ical-form-event-get event 'SUMMARY) ""))

    (let* ((options (cl-loop
                     for x in calendars
                     when (or (plist-get x :editable)
                              (equal (plist-get x :id)
                                     cal-id))
                     collect
                     `(item :tag ,(plist-get x :title)
                            :value ,(plist-get x :id)
                            :editable ,(plist-get x :editable)))))
      (apply
       #'widget-create
       'menu-choice
       :field-key 'calendar-id
       :tag "Calendar"
       :format "%[%t%]: %v\n\n"
       :value (or cal-id
                  (plist-get
                   (cl-find-if
                    (lambda (x) (plist-get x :default))
                    calendars)
                   :id)
                  (plist-get
                   (cl-find-if
                    (lambda (x) (plist-get x :editable))
                    calendars)
                   :id))
       options))

    (widget-create 'editable-field
                   :field-key 'start-date
                   :keymap ical-form-field-map
                   :format " %v "
                   :size 10
                   (and event
                        (format-time-string "%F" (car dt-start))))

    (widget-create 'editable-field
                   :field-key 'end-date
                   :keymap ical-form-field-map
                   :format "  --    %v   "
                   :size 10
                   (format-time-string "%F" end))
    (widget-create 'editable-field
                   :field-key 'start-time
                   :keymap ical-form-field-map
                   :format " %v -- "
                   :size 6
                   (ical-form--format-time
                    (car dt-start)
                    timezone))
    (widget-create 'editable-field
                   :field-key 'end-time
                   :keymap ical-form-field-map
                   :format " %v   "
                   :size 6
                   (ical-form--format-time end timezone))
    (widget-create 'checkbox
                   :field-key 'all-day
                   :format " %[%v%] All day\n\n"
                   :notify #'ical-form--hs-action
                   :hs (ical-form--checkbox-hs
                        'end-date
                        '(start-time end-time timezone))
                   all-day-p)
    (let* ((options (mapcar
                     (lambda (x)
                       `(item :tag ,(format "%s (%s)"
                                            (car x)
                                            (plist-get (cdr x) :abbrev))
                              :value ,(car x)
                              :details x))
                     timezones)))
      (apply
       #'widget-create
       'menu-choice
       :field-key 'timezone
       :notify #'ical-form--timezone-widget-notify
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
     :value (or
             (ical-form-event-get event 'X-EMACS-AVAILABILITY) 'busy)
     '(item :format "%[Tentative%] " :value tentative)
     '(item :format "%[Free%] " :value free)
     '(item :format "%[Busy%] " :value busy)
     '(item :format "%[Unavailable%] " :value unavailable))

    (widget-create
     'editable-field
     :field-key 'location
     :keymap ical-form-field-map
     :format
     (concat
      (propertize "Location: " 'face 'ical-form-field-names)
      "%v\n")
     (or (ical-form-event-get event 'LOCATION) ""))

    (let* ((recur (cdr (ical-form-event-get event 'RRULE t)))
           (group-items
            (list
             `(editable-field
               :field-key recurrence-interval
               :value-to-external ical-form--parse-integer-field
               :keymap ical-form-field-map
               :format "every %v "
               :size 5
               ,(or (when-let (interval
                               (alist-get 'INTERVAL recur))
                      (format "%d" interval))
                    "1"))

             `(radio-button-choice
               :field-key recurrence-freq
               :entry-format "%b %v"
               :format "%v\n"
               :hs ((weekly . recurrence-byday)
                    (monthly recurrence-byday
                             recurrence-bymonthday)
                    (yearly recurrence-byday
                            recurrence-bymonth
                            recurrence-byweekno
                            recurrence-byyearday))
               :notify ical-form--hs-action
               :value ,(or (alist-get 'FREQ recur)
                           'weekly)
               (item :format "%[Day%] " :value daily)
               (item :format "%[Week%] " :value weekly)
               (item :format "%[Month%] " :value monthly)
               (item :format "%[Year%] " :value yearly))

             (append
              `(checklist
                :field-key recurrence-byday
                :indent 3
                :format "on %v\n"
                :value ,(cl-loop for day in (alist-get 'BYDAY recur)
                                 collect (car day)))
              (cl-loop
               with lst = '("SUNDAY" "MONDAY" "TUESDAY"
                            "WEDNESDAY" "THURSDAY" "FRIDAY"
                            "SATURDAY")
               for w in lst
               collect `(item :format "%t "
                              :tag ,(capitalize (substring w 0 3))
                              ,(intern (substring w 0 2)))))

             `(editable-field
               :field-key recurrence-bymonthday
               :value-to-external ical-form--parse-integer-list-field
               :keymap ical-form-field-map
               :format "on days of month [-31 to 31]: %v\n"
               :size 10
               ,(or (when-let (mdays (alist-get 'BYMONTHDAY recur))
                      (string-join (cl-loop for i in mdays
                                            collect (number-to-string i))
                                   ", "))
                    ""))
             (append
              `(checklist
                :field-key recurrence-bymonth
                :format "on %v\n"
                :value ,(alist-get 'BYMONTH recur))
              (cl-loop
               with lst = '("JAN" "FEB" "MAR" "APR"
                            "MAY" "JUN" "JUL" "AUG"
                            "SEP" "OCT" "NOV" "DEC")
               for w in lst
               for idx from 1
               collect `(item :format "%t "
                              :tag ,w
                              ,idx)))
             `(editable-field
               :field-key recurrence-byweekno
               :value-to-external ical-form--parse-integer-list-field
               :keymap ical-form-field-map
               :format "on weeks of year [-53 to 53]: %v\n"
               :size 10
               ,(or (when-let (mdays (alist-get 'BYWEEKNO recur))
                      (string-join (cl-loop for i in mdays
                                            collect (number-to-string i))
                                   ", "))
                    ""))
             `(editable-field
               :field-key recurrence-byyearday
               :value-to-external ical-form--parse-integer-list-field
               :keymap ical-form-field-map
               :format "on days of year [-366 to 366]: %v\n"
               :size 10
               ,(or (when-let (mdays (alist-get 'BYYEARDAY recur))
                      (string-join (cl-loop for i in mdays
                                            collect (number-to-string i))
                                   ", "))
                    ""))
             `(radio-button-choice
               :field-key recurrence-end-rule
               :do-not-save t
               :entry-format "%b %v"
               :format "%v"
               :hs ((on . recurrence-until)
                    (after . recurrence-count))
               :notify ical-form--hs-action
               :value ,(or (and (alist-get 'UNTIL recur) 'on)
                           (and (alist-get 'COUNT recur) 'after))
               (item :format "%[Until%] " :value on)
               (item :format "%[After%] " :value after))

             `(editable-field
               :field-key recurrence-until
               :value-to-external ical-form--parse-date-field
               :keymap ical-form-field-map
               ;; additional space is needed, otherwise :from and :to of the widget
               ;; change as text is added to it
               :format " %v "
               :size 10
               ,(or (when-let (end-date (alist-get 'UNTIL recur))
                      (format-time-string "%F" end-date))
                    ""))

             `(editable-field
               :field-key recurrence-count
               :value-to-external ical-form--parse-integer-field
               :keymap ical-form-field-map
               :format " %v occurrences"
               :size 5
               ,(or (when-let (occurrence-count
                               (alist-get 'COUNT recur))
                      (format "%d" occurrence-count))
                    ""))
             ;; TODO: Unimplemented features:
             ;; - How do we handle BYDAY's week-number (in cdr)?
             ;; - set-positions.
             ;; - WKST Indicates which day of the week the recurrence
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
       :notify #'ical-form--hs-action
       :hs (ical-form--checkbox-hs 'recurrence)
       recur)

      (apply #'widget-create
             'group
             :format (concat (propertize ":" 'display "") "%v")
             :field-key 'recurrence
             :value-to-external
             (lambda (widget _value)
               (unless (widget-get widget :hidden)
                 (string-join
                  (cl-loop
                   with value = nil
                   for child in (widget-get widget :children)
                   for field-key = (widget-get child :field-key)
                   for up-field-key = (upcase (string-trim-left
                                               (symbol-name field-key)
                                               "recurrence-"))
                   when (and field-key
                             (not (widget-get child :do-not-save))
                             ;; If it's hidden, it shouldn't be part of the
                             ;; value.
                             (not (widget-get child :hidden)))
                   do (setq value (widget-value child))
                   and when value
                   collect
                   (format "%s=%s"
                           up-field-key
                           (cond
                            ((equal up-field-key "UNTIL")
                             (concat (format-time-string "%Y%m%d" value nil)
                                     ;; Get the time from the current entry
                                     (if-let ((prev-until (ical-form-event-get
                                                           (ical-form-data)
                                                           'RRULE
                                                           'UNTIL)))
                                         (format-time-string "T%H%M%SZ"
                                                             prev-until t)
                                       "T235959Z")))
                            ((equal up-field-key "FREQ")
                             (upcase (symbol-name value)))
                            ((equal up-field-key "BYDAY")
                             (string-join (cl-loop for v in value
                                                   collect (upcase
                                                            (symbol-name v)))
                                          ","))
                            ((listp value) (string-join value ","))
                            (t (format "%s" value)))))
                  ";")))
             :indent 3
             ;; We have to set the group value here because otherwise the
             ;; checklists are not set correctly. This is because the group
             ;; value is nil by default which leads to resetting of all
             ;; checklists. See bug#75171
             :value group-value
             group-items))

    (widget-insert "\n\n")

    (when-let (stat (ical-form-event-get event 'STATUS))
      (unless (eq stat 'none)
        (widget-insert
         (propertize "Status: " 'face 'ical-form-field-names)
         (symbol-name stat) "\n\n")))

    (when-let (org (ical-form-event-get event 'ORGANIZER))
      (widget-insert
       (propertize "Organizer: " 'face 'ical-form-field-names)
       org "\n\n"))


    (widget-create
     'editable-field
     :field-key 'url
     :keymap ical-form-field-map
     :format
     (concat
      (propertize "URL: " 'face 'ical-form-field-names)
      "%v\n\n")
     (or (ical-form-event-get event 'URL) ""))

    (widget-create
     'text
     :field-key 'notes
     :format "%v" ; Text after the field!
     :keymap ical-form-text-map
     :value-face 'ical-form-notes-field
     (ical-form--html-content-maybe
      (or (ical-form-event-get  event 'DESCRIPTION) "")))

    (widget-setup)

    (let ((widgets (ical-form--get-widgets)))
      ;; Call every hide-show notification so we have the correct initial
      ;; state.
      (cl-loop for wid in widgets
               for notify = (widget-get wid :notify)
               when (eq notify #'ical-form--hs-action)
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

    (add-hook 'post-command-hook #'ical-form--avoid-point-max nil t)

    (when (ical-form-event-get event 'X-EMACS-READ-ONLY)
      (ical-form--make-inactive))))

(defun ical-form--avoid-point-max ()
  "Keep point from being at point-max unless buffer is empty."
  (when (and (> (point-max) (point-min)) (eobp))
    (backward-char)))

(defun ical-form-data ()
  "Return event data of current event."
  (let* ((widgets (ical-form--get-widgets))
         (title-wid (ical-form--find-widget 'title widgets)))
    (widget-get title-wid :event-data)))

(defun ical-form-duplicate ()
  "Duplicate current event.
Should be called on an event-details buffer. Make the event
editable and remove ID information so that the event will be
treated as new when saved."
  (interactive)
  (let* ((data (ical-form-data)))
    (setf (alist-get 'UID data) nil
          (alist-get 'X-EMACS-READ-ONLY data) nil)

    ;; If calendar is read-only, we'll an error will be issued when trying to
    ;; save. So might as well not change it.

    ;; reactivate form
    (ical-form--make-inactive t)))

(defun ical-form-create-event (start end &optional all-day time-zone-id) ;
  "Return an event alist.
START and END are the start and end time for the event. If
ALL-DAY is non-nil, the event should be for the whole day.
TIME-ZONE-ID specifies the timezone."
  (list (cons 'DTSTART
              (ical-form--format-ical-date start all-day time-zone-id))
        (cons 'DTEND
              (ical-form--format-ical-date end all-day time-zone-id))))

(provide 'ical-form)
;;; ical-form.el ends here
