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
(require 'shr)

(defcustom ical-form-event-updated-hook nil
  "Hook called when an event is updated successfully.
Takes two arguments, the first is the old event data and the
second is the new event data."
  :type 'hook
  :group 'ical-form)

(defconst ical-form--html-tag-rx
  ;; [[:space:]] does not match newline in Emacs regexps, so this
  ;; spells out the whitespace characters explicitly.
  "</?[a-zA-Z][-a-zA-Z0-9]*\\(?:[ \t\n\r][^<>]*\\)?/?>"
  "Regexp matching generic HTML/XML tag syntax.
Deliberately doesn't check the tag name against a list of known
HTML elements: no purely syntactic or structural check can
distinguish a real tag from a stray bracketed word like \"<TBD>\"
anyway (rendering strips both the same way), so this just gates
against obviously-plain text before bothering to parse/render at
all -- `ical-form--html-content-maybe' is what actually decides
whether rendering did anything worth keeping.")

(defun ical-form--looks-like-html-p (content)
  "Return non-nil if CONTENT contains anything that looks like a tag."
  (string-match-p ical-form--html-tag-rx content))

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

(defface ical-form-type-tag
  '((t
     :inherit shadow
     :weight bold
     :height 0.8))
  "Face used for the \"REMINDER\"/\"EVENT\" type tag."
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
(defvar-local ical-form--inhibit-auto-time-update nil)


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
          ;; `parse-time-string' leaves the ZONE slot nil for a bare
          ;; wall-clock string like "20260810T083000" (no trailing Z),
          ;; which makes `encode-time' assume system-local time. A
          ;; TZID names a specific zone instead, so it must be plugged
          ;; into that slot -- otherwise a TZID'd time is silently
          ;; misinterpreted as being in whatever zone Emacs is
          ;; currently running in.
          (list (encode-time
                 (if time-zone-id
                     (append (butlast parsed-date) (list time-zone-id))
                   parsed-date))
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
         ;; DATE is an absolute instant; formatting it without pinning
         ;; ZONE to TIME-ZONE-ID would print the wall-clock time in
         ;; whatever zone Emacs is currently running in, while still
         ;; tagging it with TIME-ZONE-ID's TZID param -- a mismatched
         ;; pair that decodes to the wrong instant on read-back.
         (time-string (format-time-string
                       format date (or time-zone-id (and (not all-day) t)))))
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
  (when-let* ((prop-val (alist-get prop event)))
    (let* ((parse-quote-string (lambda (x) (list (intern
                                                  (downcase (cadr x))))))
           (trans
            (or (alist-get
                 prop
                 `((DTSTART . ical-form--parse-ical-date)
                   (DTEND . ical-form--parse-ical-date)
                   (DTSTAMP . ical-form--parse-ical-date)
                   (DUE . ical-form--parse-ical-date)
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

(defun ical-form-reminder-p (event)
  "Return non-nil if EVENT is a reminder (VTODO) rather than an event.
A reminder has a DUE date instead of a DTSTART/DTEND range, so its
absence of DTSTART -- which every event has, even a blank one created
via `ical-form-create-event' -- is what distinguishes the two."
  (and event (not (alist-get 'DTSTART event))))

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
         (reminder-p (ical-form-reminder-p old-data))
         (tz (ical-form--value 'timezone widgets))
         (all-day (ical-form--value 'all-day widgets))
         (start (ical-form--parse-datetime
                 (if all-day
                     "00:00"
                   (ical-form--value 'start-time widgets))
                 (ical-form--value 'start-date widgets)))
         ;; Reminders have no end-date/end-time widgets -- a due date is a
         ;; single instant, not a range.
         (end (unless reminder-p
                (ical-form--parse-datetime
                 (if all-day
                     "23:59:59"
                   (ical-form--value 'end-time widgets))
                 (ical-form--value 'end-date widgets))))
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
            (DESCRIPTION nil ,(ical-form--notes-value widgets))
            ;; Reminders have no availability, but have a completion status
            ;; instead, which events don't expose as editable.
            ,@(if reminder-p
                  `((STATUS nil ,(if (ical-form--value 'completed widgets)
                                     "COMPLETED"
                                   "NEEDS-ACTION")))
                `((X-EMACS-AVAILABILITY
                   nil
                   ,(upcase (symbol-name
                             (ical-form--value 'availability widgets))))))))
         (new-event (null old-id)))
    (when (ical-form-event-get old-data 'X-EMACS-READ-ONLY)
      (user-error "Event is not editable.?"))

    (setq new-data
          (append
           new-data
           (if reminder-p
               (list (cons 'DUE (ical-form--format-ical-date start all-day tz)))
             (list (cons 'DTSTART (ical-form--format-ical-date start all-day tz))
                   (cons 'DTEND (ical-form--format-ical-date end all-day tz))))))


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
  (interactive (list (widget-at)))
  (let* ((widgets (ical-form--get-widgets))
         ;; Reminders have no end-date/end-time widgets to pick, so ignore
         ;; any request to target them.
         (for-end-date (and (ical-form--find-widget 'end-date widgets)
                            (or current-prefix-arg
                                (and widget
                                     (member (widget-get widget :field-key)
                                             '(end-time end-date))))))
         (ktime (if for-end-date 'end-time 'start-time))
         (kdate (if for-end-date 'end-date 'start-date)))
    (if (widget-get (ical-form--find-widget ktime widgets) :inactive)
        (ical-form-read-only)
      (let* ((all-day-p  (ical-form--value 'all-day widgets))
             ;; Define these two to make sure they are bound for
             ;; `org-read-date'
             org-time-was-given
             org-end-time-was-given
             (new-time (org-read-date
                        (not all-day-p)
                        t
                        nil
                        (if for-end-date "End" "Start")
                        (ical-form--parse-datetime
                         (if all-day-p
                             (if for-end-date "23:59" "00:00")
                           (ical-form--value ktime widgets))
                         (ical-form--value kdate widgets)))))
        (save-excursion
          (let ((ical-form--inhibit-auto-time-update t))
            (widget-value-set (ical-form--find-widget kdate widgets)
                              (format-time-string "%F" new-time))
            (when (and (not all-day-p)
                       org-time-was-given)
              (widget-value-set (ical-form--find-widget ktime widgets)
                                (ical-form--format-time new-time))))

          (unless for-end-date
            (ical-form--update-end-time))

          (when (and (not all-day-p)
                     (not for-end-date)
                     org-time-was-given
                     org-end-time-was-given)
            (widget-value-set (ical-form--find-widget 'end-time widgets)
                              org-end-time-was-given)))))))

(defun ical-form--update-end-time (&rest _)
  "Update end time and date to maintain previous duration.
Ignores arguments."
  (unless ical-form--inhibit-auto-time-update
    (let* ((widgets (ical-form--get-widgets))
           (start-time-wid (ical-form--find-widget 'start-time widgets))
           (start-date-wid (ical-form--find-widget 'start-date widgets))
           (end-time-wid (ical-form--find-widget 'end-time widgets))
           (end-date-wid (ical-form--find-widget 'end-date widgets))
           (all-day-p  (ical-form--value 'all-day widgets))
           (tz (ical-form--value 'timezone widgets))
           (new-start-time
            (ignore-errors (ical-form--parse-datetime
                            (if all-day-p
                                "00:00"
                              (widget-value start-time-wid))
                            (widget-value start-date-wid)
                            tz)))
           (old-start-time (widget-get start-time-wid :prev-time))
           (end-time (ignore-errors
                       (ical-form--parse-datetime
                        (if all-day-p
                            "23:59"
                          (widget-value end-time-wid))
                        (widget-value end-date-wid)
                        tz)))
           new-end-time)
      (when (and end-time new-start-time)
        (setq new-end-time (time-add new-start-time
                                     (time-subtract
                                      end-time
                                      old-start-time)))
        (save-excursion
          (widget-value-set
           end-date-wid
           (format-time-string "%F" new-end-time tz))
          (widget-value-set
           end-time-wid
           (ical-form--format-time new-end-time tz))
          (widget-put start-time-wid :prev-time new-start-time))))))

(defun ical-form-open (event calendars timezones &optional update-fn)
  "Open a buffer to display the details of EVENT.
CALENDARS and TIMEZONES should be a list of calendars, and
timezones to use in the form.

UPDATE-FN, if provided, is used to set
`ical-form-update-event-function' locally in the new buffer."
  (let ((buf (generate-new-buffer
              (if (ical-form-reminder-p event)
                  "*calendar reminder*"
                "*calendar event*"))))
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
Also make it untabbable if hidden, and keep point from landing
inside it. An `invisible' overlay only hides WIDGET from
*display* -- `cursor-intangible-mode' decides where point may
rest via the `cursor-intangible' *text property* (checked with
`get-pos-property', which ignores overlays), so without also
setting that property, ordinary cursor motion (not just
`\\[widget-forward]', which already skips it via `:tab-order')
can still land point inside the hidden widget, letting its RET
binding fire even though nothing is visibly there to press."
  (ical-form--widget-overlay
   widget
   :hidden visible
   'evaporate t
   'priority 101
   'invisible (not visible))

  (when-let* ((from (widget-get widget :from))
              (to (widget-get widget :to)))
    (if visible
        (remove-text-properties from to '(cursor-intangible nil))
      (put-text-property from to 'cursor-intangible t)))

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
              'modification-hooks '(ical-form-read-only)
              'insert-in-front-hooks '(ical-form-read-only)
              'insert-behind-hooks '(ical-form-read-only)))))

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
             (tz (widget-value widget))
             (ical-form--inhibit-auto-time-update t))
        (save-excursion
          ;; Change only start time, the end time is changed automatically
          ;; `ical-form--update-end-time'
          ;; Start with end-time
          (cl-loop for (date-wid . time-wid) in '((end-date . end-time)
                                                  (start-date . start-time))
                   for time-widget = (ical-form--find-widget time-wid widgets)
                   for date-widget = (ical-form--find-widget date-wid widgets)
                   for old-time-utc = (ical-form--parse-datetime
                                       (widget-value time-widget)
                                       (widget-value date-widget)
                                       old-tz)
                   do
                   (widget-value-set
                    time-widget
                    (ical-form--format-time old-time-utc tz)))
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
                      for wid = (ical-form--find-widget wid-id widgets)
                      when wid
                      do
                      (ical-form--show-hide-widget
                       wid (cl-some #'identity vis))))))

(defvar ical-form--notes-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shr-browse-url)
    (define-key map [mouse-2] #'shr-browse-url)
    map)
  "Keymap for link text in a rendered HTML notes preview.
Only activates the link (RET, mouse-2, and mouse-1 via the
pre-existing `follow-link' property); unlike `shr-map' it doesn't
also bind ordinary letters to shr commands, which would hijack
keystrokes meant for the field itself -- see
`ical-form--html-content-maybe'.")

(defvar ical-form--notes-image-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'shr-browse-image)
    (define-key map [mouse-2] #'shr-browse-image)
    map)
  "Like `ical-form--notes-link-map', for image-placeholder text.")

(defun ical-form--collapse-whitespace (string)
  "Collapse runs of whitespace in STRING to a single space, trimmed.
HTML treats runs of whitespace, including newlines, as
insignificant, so shr's rendering of otherwise-plain text can
differ from the raw source purely in whitespace. Comparing
collapsed forms avoids mistaking that for a meaningful change."
  ;; [[:space:]] does not match newline in Emacs regexps, so this
  ;; spells out the whitespace characters explicitly.
  (string-trim (replace-regexp-in-string "[ \t\n\r\f]+" " " string)))

(defun ical-form--html-content-maybe (content)
  "Render CONTENT as HTML using shr, if that would change anything.
Return a cons (RENDERED-P . TEXT). RENDERED-P is non-nil if TEXT
is a shr rendering of CONTENT that actually differs from it
(ignoring whitespace-only differences); it is nil, and TEXT is
just CONTENT, if CONTENT doesn't look like HTML to begin with (see
`ical-form--looks-like-html-p'), if Emacs has no libxml support to
parse it, or if shr's rendering turns out to be the same as
CONTENT anyway (nothing to gain from treating it as HTML, so it
stays a plain editable field)."
  ;; Inspired by `notmuch-show--insert-part-text/html-shr'
  (if (and (libxml-available-p)
           (ical-form--looks-like-html-p content))
      (let ((rendered
             (with-temp-buffer
               (let ((shr-width (or (ignore-errors (window-body-width))
                                    shr-width))
                     (shr-inhibit-images t))
                 (shr-insert-document
                  (with-temp-buffer
                    (insert content)
                    (libxml-parse-html-region (point-min) (point-max)))))
               (buffer-substring (point-min) (point-max)))))
        ;; shr tags links/images with a `keymap' text property (shr-map /
        ;; shr-image-map) so they're clickable in a normal shr buffer. A
        ;; character's `keymap' property takes priority over a widget
        ;; field's `local-map', so left in place it hijacks ordinary
        ;; keystrokes typed into the field -- e.g. `a' silently becomes
        ;; `shr-show-alt-text' instead of reaching the field's read-only
        ;; guard or self-insert. Swap it for a minimal keymap that only
        ;; lets RET/mouse-2/mouse-1-click-follows-link activate the
        ;; link/image URL, so following a link still works without
        ;; shr-map's other bindings hijacking the rest of the field.
        (let ((pos 0) (len (length rendered)))
          (while (< pos len)
            (let ((next (next-single-property-change pos 'keymap rendered len)))
              (when (get-text-property pos 'keymap rendered)
                (put-text-property
                 pos next 'keymap
                 (if (get-text-property pos 'shr-url rendered)
                     ical-form--notes-link-map
                   ical-form--notes-image-map)
                 rendered))
              (setq pos next))))
        (if (equal (ical-form--collapse-whitespace rendered)
                   (ical-form--collapse-whitespace content))
            (cons nil content)
          (cons t rendered)))
    (cons nil content)))

(defun ical-form--notes-read-only (&rest _junk)
  "Ignoring the arguments, signal an error.
Used as a `modification-hooks' entry on the notes/description
field while it is showing a rendered HTML preview rather than
its raw source."
  (unless inhibit-read-only
    (error
     "Showing a rendered preview; use `ical-form-toggle-notes-source' to edit")))

(defun ical-form--notes-value (widgets)
  "Return the current value of the notes/description field in WIDGETS.
If the field is currently showing a rendered HTML preview, this
returns the underlying raw source instead of the rendered text,
so that saving an untouched HTML description never overwrites it
with a lossy flattened copy."
  (let ((wid (ical-form--find-widget 'notes widgets)))
    (if (and (widget-get wid :html-rendered)
             (not (widget-get wid :editing-raw)))
        (widget-get wid :raw-value)
      (widget-value wid))))

(defun ical-form--widget-resync-field-bounds (widget)
  "Repoint field WIDGET's stale :from/:to markers at its live bounds.
`widget-value-set' deletes and reinserts the field's text at the old
:from marker; since that marker's insertion type is t, the insertion
pushes it to the far side of the new text instead of leaving it at
the field's start, corrupting it into pointing past the field (often
right next to :to). Any overlay later built from :from/:to -- such as
the read-only overlay guarding a rendered HTML preview -- ends up
covering only a sliver of the field, leaving the rest silently
editable. Reset both markers from the field's own tracked bounds,
which `widget-value-set' keeps correct, to fix them back up."
  (when-let* ((from (widget-get widget :from))
              (to (widget-get widget :to))
              (field-from (widget-field-start widget))
              (field-to (widget-field-end widget)))
    (set-marker from field-from)
    (set-marker to field-to)))

(defun ical-form-toggle-notes-source (widget)
  "Toggle the notes/description WIDGET between rendered and raw source.
WIDGET must have been created with :html-rendered non-nil.
Preserves point and the buffer's modified state: toggling back and
forth without otherwise editing the field should not move the cursor
or dirty the buffer, since `widget-value-set' unconditionally does
both."
  (let ((modified (buffer-modified-p)))
    (save-excursion
      (if (widget-get widget :editing-raw)
          ;; Currently showing the editable raw source; switch to a rendered,
          ;; read-only preview, folding in whatever the user just edited --
          ;; unless it no longer contains anything worth rendering, in which
          ;; case just stay a plain editable field.
          (let* ((raw (widget-value widget))
                 (rendered (ical-form--html-content-maybe raw)))
            (widget-put widget :raw-value raw)
            (widget-put widget :html-rendered (car rendered))
            (widget-value-set widget (cdr rendered))
            (ical-form--widget-resync-field-bounds widget)
            (widget-put widget :editing-raw nil)
            (if (car rendered)
                (ical-form--widget-overlay
                 widget :inactive nil
                 'evaporate t 'priority 100
                 'modification-hooks '(ical-form--notes-read-only)
                 'insert-in-front-hooks '(ical-form--notes-read-only)
                 'insert-behind-hooks '(ical-form--notes-read-only))
              (ical-form--widget-overlay widget :inactive t)))
        ;; Currently showing the rendered, read-only preview; switch to editing
        ;; the raw source.
        (ical-form--widget-overlay widget :inactive t)
        (widget-value-set widget (widget-get widget :raw-value))
        (ical-form--widget-resync-field-bounds widget)
        (widget-put widget :editing-raw t)))
    (widget-setup)
    (set-buffer-modified-p modified)))

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
        (format
         "\\<ical-form-mode-map>%s details. \
Save `\\[ical-form-save]', \
abort `\\[ical-form-kill]'."
         (if (ical-form-reminder-p event) "Reminder" "Event"))))
      (set-buffer-modified-p nil))))

(defun ical-form--widget-group-value-create (widget)
  "Create function for groups.
This simply applies the `cursor-intangible' function to indent
characters."
  (let ((args (widget-get widget :args))
	(value (widget-get widget :value))
	arg answer children)
    (while args
      (setq arg (car args)
	    args (cdr args)
	    answer (widget-match-inline arg value)
	    value (cdr answer))
      (and (widget--should-indent-p)
	   (widget-get widget :indent)
           (insert
            (ical-form--make-intangible
             (make-string (widget-get widget :indent) ?\s))))
      (push (cond ((null answer)
		   (widget-create-child widget arg))
                  ((widget-inline-p arg t)
		   (widget-create-child-value widget arg (car answer)))
		  (t
		   (widget-create-child-value widget arg (car (car answer)))))
	    children))
    (widget-put widget :children (nreverse children))))

(defun ical-form--make-intangible (&rest args)
  (let ((result ""))
    (dotimes (i (length args) result)
      (let ((txt (pop args)))
        (when (eq (mod i 2) 0)
          (add-text-properties 0 (length txt)
                               '(cursor-intangible t
                                                   rear-nonsticky t
                                                   front-sticky t)
                               txt))
        (setq result (concat result txt))))))

(defun ical-form--create-form (event)
  "Create form in current buffer corresponding to EVENT."
  (let* ((cal-id (ical-form-event-get event 'X-EMACS-CALID))
         (read-only-p (ical-form-event-get event 'X-EMACS-READ-ONLY))
         (reminder-p (ical-form-reminder-p event))
         ;; A reminder's date is DUE, a single instant, not a DTSTART/DTEND
         ;; range.
         (dt-start (ical-form-event-get event (if reminder-p 'DUE 'DTSTART) t))
         (timezones ical-form--timezones)
         (calendars ical-form--calendars)
         (timezone (or (alist-get 'TZID (cdr dt-start))
                       (car-safe ical-form--default-timezone)))
         ;; A reminder with no DUE date has no date/time to show, so default
         ;; it to all-day rather than falling back to the current time.
         (all-day-p (if (car dt-start)
                        (alist-get 'ALL-DAY-P (cdr dt-start))
                      t))
         (end (unless reminder-p (ical-form-event-get event 'DTEND)))
         (NL (ical-form--make-intangible "\n"))
         (SPC (ical-form--make-intangible " "))
         (NL2 (concat NL NL)))
    (widget-insert NL2)

    (widget-insert
     (ical-form--make-intangible
      (propertize (if reminder-p "REMINDER" "EVENT")
                  'face 'ical-form-type-tag)
      NL))

    (widget-create 'editable-field
                   :field-key 'title
                   :event-data event
                   :keymap ical-form-field-map
                   :value-face 'ical-form-title-field
                   :format (concat "%v" NL)
                   (or (ical-form-event-get event 'SUMMARY) ""))

    (let* ((cal-type (if reminder-p "reminder" "event"))
           (cal-of-type-p (lambda (x) (equal (plist-get x :type) cal-type)))
           (options (cl-loop
                     for x in calendars
                     when (and (funcall cal-of-type-p x)
                              (or (plist-get x :editable)
                                  (equal (plist-get x :id)
                                         cal-id)))
                     collect
                     `(item :tag ,(plist-get x :title)
                            :value ,(plist-get x :id)
                            :format "%t"
                            :editable ,(plist-get x :editable)))))
      (apply
       #'widget-create
       'menu-choice
       :field-key 'calendar-id
       :format (ical-form--make-intangible
                (propertize "Calendar: " 'face 'ical-form-field-names)
                "%[%v%]"
                "\n\n")
       :value (or cal-id
                  (plist-get
                   (cl-find-if
                    (lambda (x) (and (funcall cal-of-type-p x)
                                     (plist-get x :default)))
                    calendars)
                   :id)
                  (plist-get
                   (cl-find-if
                    (lambda (x) (and (funcall cal-of-type-p x)
                                     (plist-get x :editable)))
                    calendars)
                   :id))
       options))

    (widget-create 'editable-field
                   :field-key 'start-date
                   :keymap ical-form-field-map
                   :notify #'ical-form--update-end-time
                   :format (concat SPC "%v" SPC)
                   :size 10
                   (and (car dt-start)
                        (format-time-string "%F" (car dt-start))))

    (widget-create 'editable-field
                   :field-key 'start-time
                   :keymap ical-form-field-map
                   :format (ical-form--make-intangible
                            " " "%v" " ")
                   :notify #'ical-form--update-end-time
                   :prev-time (car dt-start)
                   :size 6
                   (if (car dt-start)
                       (ical-form--format-time (car dt-start) timezone)
                     ""))

    ;; Reminders have no end -- a due date is a single instant, not a range.
    (unless reminder-p
      (widget-create 'editable-field
                     :field-key 'end-date
                     :keymap ical-form-field-map
                     :format (ical-form--make-intangible
                              "  --   " "%v" " ")
                     :size 10
                     (format-time-string "%F" end))

      (widget-create 'editable-field
                     :field-key 'end-time
                     :keymap ical-form-field-map
                     :format (concat
                              SPC
                              "%v"
                              (ical-form--make-intangible "   "))
                     :size 6
                     (ical-form--format-time end timezone)))

    (widget-create 'checkbox
                   :field-key 'all-day
                   :format (concat
                            SPC
                            "%[%v%]"
                            (ical-form--make-intangible " All day")
                            NL2)
                   :notify #'ical-form--hs-action
                   :hs (ical-form--checkbox-hs
                        nil
                        (if reminder-p
                            '(start-time timezone)
                          '(start-time end-time timezone)))
                   all-day-p)
    (let* ((options (mapcar
                     (lambda (x)
                       `(item :tag ,(format "%s (%s)"
                                            (car x)
                                            (plist-get (cdr x) :abbrev))
                              :value ,(car x)
                              :format "%t"
                              :details x))
                     timezones)))
      (apply
       #'widget-create
       'menu-choice
       :field-key 'timezone
       :notify #'ical-form--timezone-widget-notify
       :format (ical-form--make-intangible
                (propertize "Timezone: "
                            'face 'ical-form-field-names)
                "%[%v%]" "\n\n")
       :value timezone
       :old-value timezone
       options))

    ;; Reminders have no availability, but have a completion status instead.
    (if reminder-p
        (widget-create
         'checkbox
         :field-key 'completed
         :format (concat "%[%v%]"
                         (ical-form--make-intangible " Completed")
                         NL2)
         (eq (ical-form-event-get event 'STATUS) 'completed))
      (widget-create
       'radio-button-choice
       :field-key 'availability
       :entry-format (concat "%b" SPC "%v" SPC)
       :format (concat "%v" NL2)
       :value (or
               (ical-form-event-get event 'X-EMACS-AVAILABILITY) 'busy)
       `(item :format ,(ical-form--make-intangible "Tentative")
              :value tentative)
       `(item :format ,(ical-form--make-intangible "Free")
              :value free)
       `(item :format ,(ical-form--make-intangible "Busy")
              :value busy)
       `(item :format ,(ical-form--make-intangible "Unavailable")
              :value unavailable)))

    (widget-create
     'editable-field
     :field-key 'location
     :keymap ical-form-field-map
     :format
     (ical-form--make-intangible
      (propertize "Location: " 'face 'ical-form-field-names)
      "%v" "\n")
     (or (ical-form-event-get event 'LOCATION) ""))

    (let* ((recur (cdr (ical-form-event-get event 'RRULE t)))
           (group-items
            (list
             `(editable-field
               :field-key recurrence-interval
               :value-to-external ical-form--parse-integer-field
               :keymap ical-form-field-map
               :format ,(ical-form--make-intangible
                         "every " "%v" " ")
               :size 5
               ,(or (when-let* ((interval
                                (alist-get 'INTERVAL recur)))
                      (format "%d" interval))
                    "1"))

             `(radio-button-choice
               :field-key recurrence-freq
               :entry-format ,(concat "%b" SPC "%v" SPC)
               :format ,(concat "%v" NL)
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
               (item :format ,(ical-form--make-intangible "Day")
                     :value daily)
               (item :format ,(ical-form--make-intangible "Week")
                     :value weekly)
               (item :format ,(ical-form--make-intangible "Month")
                     :value monthly)
               (item :format ,(ical-form--make-intangible "Year")
                     :value yearly))

             (append
              `(checklist
                :field-key recurrence-byday
                :indent 3
                :entry-format ,(concat "%b" SPC "%v")
                :format ,(ical-form--make-intangible "on " "%v" "\n")
                :value ,(cl-loop for day in (alist-get 'BYDAY recur)
                                 collect (car day)))
              (cl-loop
               with lst = '("SUNDAY" "MONDAY" "TUESDAY"
                            "WEDNESDAY" "THURSDAY" "FRIDAY"
                            "SATURDAY")
               for w in lst
               collect `(item :format
                              ,(ical-form--make-intangible
                                (concat (capitalize (substring w 0 3))
                                        " "))
                              ,(intern (substring w 0 2)))))

             `(editable-field
               :field-key recurrence-bymonthday
               :value-to-external ical-form--parse-integer-list-field
               :keymap ical-form-field-map
               :format ,(ical-form--make-intangible
                         "on days of month [-31 to 31]: "
                         "%v"
                         "\n")
               :size 10
               ,(or (when-let* ((mdays (alist-get 'BYMONTHDAY recur)))
                      (string-join (cl-loop for i in mdays
                                            collect (number-to-string i))
                                   ", "))
                    ""))
             (append
              `(checklist
                :field-key recurrence-bymonth
                :format ,(ical-form--make-intangible "on " "%v" "\n")
                :value ,(alist-get 'BYMONTH recur))
              (cl-loop
               with lst = '("JAN" "FEB" "MAR" "APR"
                            "MAY" "JUN" "JUL" "AUG"
                            "SEP" "OCT" "NOV" "DEC")
               for w in lst
               for idx from 1
               collect `(item :format
                              ,(ical-form--make-intangible "%t ")
                              :tag ,w
                              ,idx)))
             `(editable-field
               :field-key recurrence-byweekno
               :value-to-external ical-form--parse-integer-list-field
               :keymap ical-form-field-map
               :format "on weeks of year [-53 to 53]: %v\n"
               :size 10
               ,(or (when-let* ((mdays (alist-get 'BYWEEKNO recur)))
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
               ,(or (when-let* ((mdays (alist-get 'BYYEARDAY recur)))
                      (string-join (cl-loop for i in mdays
                                            collect (number-to-string i))
                                   ", "))
                    ""))
             `(radio-button-choice
               :field-key recurrence-end-rule
               :do-not-save t
               :entry-format ,(concat "%b" SPC "%v" SPC)
               :format "%v"
               :hs ((on . recurrence-until)
                    (after . recurrence-count))
               :notify ical-form--hs-action
               :value ,(or (and (alist-get 'UNTIL recur) 'on)
                           (and (alist-get 'COUNT recur) 'after))
               (item :format ,(ical-form--make-intangible "Until")
                     :value on)
               (item :format ,(ical-form--make-intangible "After")
                     :value after))

             `(editable-field
               :field-key recurrence-until
               :value-to-external ical-form--parse-date-field
               :keymap ical-form-field-map
               ;; additional space is needed, otherwise :from and :to of the widget
               ;; change as text is added to it
               :format ,(concat SPC "%v" SPC)
               :size 10
               ,(or (when-let* ((end-date (alist-get 'UNTIL recur)))
                      (format-time-string "%F" end-date))
                    ""))

             `(editable-field
               :field-key recurrence-count
               :value-to-external ical-form--parse-integer-field
               :keymap ical-form-field-map
               :format ,(concat SPC "%v"
                                (ical-form--make-intangible " occurrences"))
               :size 5
               ,(or (when-let* ((occurrence-count
                                (alist-get 'COUNT recur)))
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
       :format (concat "%[%v%]"
                       (ical-form--make-intangible " Repeat "))
       :notify #'ical-form--hs-action
       :hs (ical-form--checkbox-hs 'recurrence)
       recur)

      (apply #'widget-create
             'group
             :format (ical-form--make-intangible
                      (propertize ":" 'display "")
                      "%v")
             :field-key 'recurrence
             :value-create #'ical-form--widget-group-value-create
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
                                     (if-let* ((prev-until (ical-form-event-get
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

    (widget-insert NL2)

    ;; Reminders show their completion status as the editable "Completed"
    ;; checkbox above instead of this read-only line.
    (unless reminder-p
      (when-let* ((stat (ical-form-event-get event 'STATUS)))
        (unless (eq stat 'none)
          (widget-insert
           (ical-form--make-intangible
            (concat (propertize "Status: "
                                'face 'ical-form-field-names)
                    (symbol-name stat)
                    NL2))))))

    (when-let* ((org (ical-form-event-get event 'ORGANIZER)))
      (widget-insert
       (ical-form--make-intangible
        (concat (propertize "Organizer: "
                            'face 'ical-form-field-names)
                org))
       NL2))


    (widget-create
     'editable-field
     :field-key 'url
     :keymap ical-form-field-map
     :format
     (ical-form--make-intangible
      (propertize "URL: " 'face 'ical-form-field-names)
      "%v" "\n\n")
     (or (ical-form-event-get event 'URL) ""))

    (let* ((raw-notes (or (ical-form-event-get event 'DESCRIPTION) ""))
           (rendered (ical-form--html-content-maybe raw-notes))
           (html-rendered (car rendered))
           notes-wid)
      ;; A read-only event always shows the rendered form, with no source to
      ;; toggle to -- there's nothing to edit either way.
      (when (and html-rendered (not read-only-p))
        (widget-create
         'push-button
         :notify (lambda (&rest _)
                   (ical-form-toggle-notes-source notes-wid))
         "Toggle rendered/source")
        (widget-insert "\n"))
      (setq notes-wid
            (widget-create
             'text
             :field-key 'notes
             :format "%v" ; Text after the field!
             :keymap ical-form-text-map
             :value-face 'ical-form-notes-field
             :html-rendered html-rendered
             :raw-value raw-notes
             (cdr rendered))))

    (insert (propertize "\n" 'cursor-intangible t
                        'rear-nonsticky nil
                        'front-sticky t))

    (widget-setup)

    (let ((widgets (ical-form--get-widgets)))
      ;; Call every hide-show notification so we have the correct initial
      ;; state.
      (cl-loop for wid in widgets
               for notify = (widget-get wid :notify)
               when (eq notify #'ical-form--hs-action)
               do (funcall notify wid))

      ;; If the notes field is showing a rendered HTML preview, make it
      ;; read-only until toggled to editing its raw source (see
      ;; `ical-form-toggle-notes-source'), so that saving an untouched
      ;; HTML description never overwrites it with the flattened preview.
      (when-let* ((notes-wid (ical-form--find-widget 'notes widgets))
                  ((widget-get notes-wid :html-rendered)))
        (ical-form--widget-overlay
         notes-wid :inactive nil
         'evaporate t 'priority 100
         'modification-hooks '(ical-form--notes-read-only)
         'insert-in-front-hooks '(ical-form--notes-read-only)
         'insert-behind-hooks '(ical-form--notes-read-only)))

      ;; This causes all lists of radio buttons to skip text when tabbing,
      ;; instead just going through the buttons
      (cl-loop for wid in widgets
               when (eq (widget-type wid) 'radio-button-choice)
               do
               (cl-loop for child in (widget-get wid :children)
                        do (widget-put child :tab-order -1))))
    (cursor-intangible-mode)

    (goto-char (point-min))
    (widget-move 1) ;; Go to next widget (should be title)
    (widget-end-of-line) ;; Go to end of line

    ;; (add-hook 'post-command-hook #'ical-form--avoid-point-max nil t)

    (when read-only-p
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

(defun ical-form-create-reminder (&optional due all-day time-zone-id)
  "Return a reminder alist for a new reminder.
DUE, if given, is the reminder's due date/time; a reminder created
with no DUE simply has no due date, same as one fetched with none
set (see `ical-form-reminder-p'). If ALL-DAY is non-nil, DUE is a
whole-day due date. TIME-ZONE-ID specifies DUE's timezone.

Always includes a NEEDS-ACTION STATUS -- a real reminder always has
one, and unlike DTSTART this makes the returned alist non-nil even
with no DUE, which `ical-form-reminder-p' needs to tell a blank
reminder template apart from a blank event one."
  (append
   (list (cons 'STATUS (list nil "NEEDS-ACTION")))
   (when due
     (list (cons 'DUE (ical-form--format-ical-date due all-day time-zone-id))))))

(provide 'ical-form)
;;; ical-form.el ends here
