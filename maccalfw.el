;;; maccalfw.el --- Calendar view for Mac Calendars -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Al Haji-Ali

;; Author: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Created: 2023
;; Version: 0.2
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

;; We declare it so that it can be used in `ical-form'
(require 'calfw)
(require 'ical-form)

(defvar maccalfw-modify-future-events-p 'ask
  "If non-nil, modifying events with recurrences applies to future events.
Special value \\='ask, prompts the user.")

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
  (let* ((dt-start (ical-form-event-get event 'DTSTART t))
         (start (decode-time (car dt-start)))
         (end (decode-time (ical-form-event-get event 'DTEND)))
         (all-day-p (alist-get 'ALL-DAY-P (cdr dt-start)))
         (args
          (list
           :start-date  (maccalfw--decode-date start)
           :start-time  (unless all-day-p
                          (maccalfw--decode-time start))
           :end-date    (when all-day-p
                          (maccalfw--decode-date end))
           :end-time    (unless all-day-p
                          (maccalfw--decode-time end))
           :title       (ical-form-event-get event 'SUMMARY)
           :location    (ical-form-event-get event 'LOCATION)
           :description (ical-form-event-get event 'DESCRIPTION))))
    (when (and (alist-get 'status (cl-struct-slot-info 'cfw:event))
               (alist-get 'data (cl-struct-slot-info 'cfw:event)))
      (setq args
            (append args (list
                          :status (ical-form-event-get event 'STATUS)
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
   :sorter (or (and (fboundp #'calfw-blocks-default-sorter)
                    #'calfw-blocks-default-sorter)
             #'string-lessp)))

(defun maccalfw-delete-event (ev)
  "Delete EVENT."
  (interactive
   (list (or (when-let (cfw-ev (get-text-property (point) 'cfw:event))
               (cfw:event-data cfw-ev))
             (error "No event at location"))))
  (or (prog1
            (maccalfw-remove-event
             (ical-form-event-get ev 'UID)
             (ical-form-event-get ev 'DTSTART)
             (if (ical-form-event-get ev 'RRULE)
               (maccalfw-modify-future-events-p)
             nil))
        (message "Event deleted")
        (cfw:refresh-calendar-buffer nil))
      (error "Deleting event failed")))

(defun maccalfw-modify-future-events-p (&optional prompt)
  "Return non-nil if modification should affect all future events.
Check the value of the variable
`maccalfw-modify-future-events-p', and potentially prompt
the user, displaying the message PROMPT."
  (if (eq maccalfw-modify-future-events-p 'ask)
      (let ((response
             (cadr
              (read-multiple-choice
               (format (or prompt "Which events to modify?")
                       (buffer-name))
               '((?f "future" "Modification applies to all future events.")
                 (?c "current" "Modification applies only to current event."))
               nil nil (and (not use-short-answers)
                            (and (fboundp 'use-dialog-box-p)
                                 (not (use-dialog-box-p))))))))
        (equal response "future"))
    maccalfw-modify-future-events-p))

(defun maccalfw-modify-event (old-data new-data)
  "Update or create an event.
Only `UID', 'DTSTART' and 'RRULE' are used from OLD-DATA.
NEW-DATA can contain only changed fields. If `UID' is missing or
nil, a new event is created instead."
  ;; if old event has a recurrence, check with use if all future events
  ;; should be editied or just the current one
  (let ((future
         (and (or (ical-form-event-get old-data 'RRULE)
                  (ical-form-event-get new-data 'RRULE))
              ;; If changing recurrence rule, then we should modify all
              ;; future events. Otherwise, we should ask the user
              (or (ical-form-event-get new-data 'RRULE)
                  (maccalfw-modify-future-events-p)))))
    (maccalfw-update-event
     (ical-form-event-get old-data 'UID)
     new-data
     (ical-form-event-get old-data 'DTSTART)
     future)))

;; TODO:
;; (defun maccalfw-delete-event (event)
;;   "Delete calfw EVENT."
;;   (interactive
;;    (list (or (get-text-property (point) 'cfw:event)
;;              (error "No event at location"))))
;;   (ical-form-remove-event (cfw:event-data event)))

(defun maccalfw-new-event (event-data)
  "Create an events-details buffer for a new event.
EVENT-DATA contains the initial event information."
  (interactive
   (list
    (let (start end all-day ev)
      (when (derived-mode-p 'cfw:calendar-mode)
        (if-let ((event (and current-prefix-arg
                             (get-text-property (point) 'cfw:event)))
                 (old-event-data (cfw:event-data event)))
            (setq ev
            (cl-loop for item in old-event-data
                           if (member (car item) '( ; Keep those fields
                                             DTSTART DTEND
                                             SUMMARY
                                             LOCATION
                                             X-AVAILABILITY
                                             URL
                                             DESCRIPTION))
                           collect item))
          (when (and (fboundp 'calfw-blocks-region-to-time)
                     (eq (cfw:component-view (cfw:cp-get-component))
                         'block-week))
            (cl-destructuring-bind (e-start e-end e-all-day)
                (calfw-blocks-region-to-time)
              (setq start e-start
                    end e-end
                    all-day e-all-day)))))
      (or ev
          (let ((start (or start (current-time))))
            (ical-form-create-event
             start (or end (time-add start 3600)) all-day))))))
  (ical-form-open event-data
                  (maccalfw-get-calendars)
                  (maccalfw-timezones)))

(defun maccalfw-goto-event-details (event)
  "Open event details for the calfw EVENT."
  (interactive
   (list (or (get-text-property (point) 'cfw:event)
             (error "No event at location"))))
  (ical-form-open (cfw:event-data event)
                  (maccalfw-get-calendars)
                  (maccalfw-timezones)))


(defun maccalfw-mouse-down-disable-dbl-click (event)
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

(make-obsolete 'maccalfw-event-new-event'maccalfw-new-event "0.2")
(make-obsolete 'maccalfw-event-goto-details
               'maccalfw-goto-event-details "0.2")
(make-obsolete 'maccalfw-event-delete-event 'maccalfw-delete-event  "0.2")
(make-obsolete 'maccalfw-event-mouse-down
               'maccalfw-mouse-down-disable-dbl-click "0.2")
(make-obsolete 'maccalfw-event-open 'ical-form-open "0.2")
(make-obsolete-variable 'maccalfw-event-save-hook
                        'ical-form-event-updated-hook  "0.2")

(provide 'maccalfw)
;;; maccalfw.el ends here
