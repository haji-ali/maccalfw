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
(require 'ical-form)

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
  (let* ((dt-start (ical-form--get event 'DTSTART t))
         (start (decode-time (car dt-start)))
         (end (decode-time (ical-form--get event 'DTEND)))
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
           :title       (ical-form--get event 'SUMMARY)
           :location    (ical-form--get event 'LOCATION)
           :description (ical-form--get event 'DESCRIPTION))))
    (when (and (alist-get 'status (cl-struct-slot-info 'cfw:event))
               (alist-get 'data (cl-struct-slot-info 'cfw:event)))
      (setq args
            (append args (list
                          :status (ical-form--get event 'STATUS)
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


;; TODO: maccalfw-delete-event should be here, but at the moment
;; it uses ical-form to check if we should modify future events
;; this should be abstract somehow.

(defun maccalfw-delete-event (event)
  "Delete calfw EVENT."
  (interactive
   (list (or (get-text-property (point) 'cfw:event)
             (error "No event at location"))))
  (or (prog1
          (let ((ev (cfw:event-data event)))
            (maccalfw-remove-event
             (ical-form--get ev 'UID)
             (ical-form--get ev 'DTSTART)
             (if (ical-form--get ev 'RRULE)
                 (ical-form-modify-future-events-p)
               nil)))
        (message "Event deleted")
        (cfw:refresh-calendar-buffer nil))
      (error "Deleting event failed")))

(defun maccalfw-event-new-event (event-data)
  "Create an events-details buffer for a new event.
EVENT-DATA contains the initial event information."
  ;; TODO: Needs updating to ical form
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
            (cl-loop for item in old-event-data
                     if (member (car item) '(;
                                             DTSTART DTEND
                                             SUMMARY
                                             LOCATION
                                             X-AVAILABILITY
                                             URL
                                             DESCRIPTION))
                     collect item)
          (cl-destructuring-bind (start end all-day)
              (calfw-blocks-region-to-time)
            (list (cons 'DTSTART (ical-form--format-ical-date start all-day))
                  (cons 'DTEND (ical-form--format-ical-date (or end (time-add start 3600))
                                                            all-day)))))
      (list (cons 'DTSTART (ical-form--format-ical-date (current-time)))
            (cons 'DTEND (ical-form--format-ical-date (time-add (current-time)
                                                                3600)))))))
  (ical-form-open event-data))

(defun maccalfw-event-goto-details (event)
  "Open event details for the calfw EVENT."
  (interactive
   (list (or (get-text-property (point) 'cfw:event)
             (error "No event at location"))))
  (ical-form-open (cfw:event-data event)))

(provide 'maccalfw)
;;; maccalfw.el ends here
