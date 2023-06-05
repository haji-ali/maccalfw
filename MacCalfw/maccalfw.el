;;; maccalfw.el --- calendar view for Mac Calendars -*- lexical-binding: t; -*-

;;Copyright (C) 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
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

;; A bridge from Mac Calendar to calfw.
;; The API and interfaces have not been confirmed yet.

;;; Installation:

;; Here is a minimum sample code:
;; (require 'maccalfw)
;; To open a calendar buffer, execute the following function.
;; (cfw:open-maccal-calendar 'all)

;; Executing the following command, this program clears caches to refresh the ICS data.
;; (cfw:maccal-data-cache-clear-all)

;;; Code:

(require 'calfw)

(defun maccalfw--decode-date (date)
  (list (decoded-time-month date)
        (decoded-time-day date)
        (decoded-time-year date)))

(defun maccalfw--encode-date (date)
  (encode-time (list 0 0 0
                     (nth 1 date)
                     (nth 0 date)
                     (nth 2 date))))

(defun maccalfw--decode-time (date)
  (list (decoded-time-hour date)
        (decoded-time-minute date)))


(defun maccalfw-convert-event (event)
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
     :description (plist-get event :summary))))

(defun maccalfw-convert-to-calfw (events-list)
  (cl-loop for e in events-list
        for event = (maccalfw-convert-event e)
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

(defvar maccalfw-data-cache nil "a list of (cal-id . ics-data)")

(defun maccalfw-data-cache-clear (cal-id)
  (setq maccalfw-data-cache
        (cl-loop for i in maccalfw-data-cache
              for (u . d) = i
              unless (equal u cal-id)
              collect i)))

(defun maccalfw-data-cache-clear-all ()
  (interactive)
  (setq maccalfw-data-cache nil))

(defun maccalfw-to-calendar (cal-id begin end)
  (cl-loop for event in
        (maccalfw-convert-to-calfw
            (maccalfw-fetch-events cal-id
                                   (maccalfw--encode-date begin)
                                   ;; end is not included, added an extra day
                                   (time-add
                                    (maccalfw--encode-date end)
                                    ;; seconds in a day
                                    86400)))
        if (and (listp event)
                (equal 'periods (car event)))
        collect
        (cons
         'periods
         (cl-loop for evt in (cadr event)
               collect evt))
        else
        collect event))

(defun maccalfw-create-source (name cal-id color)
    (make-cfw:source
     :name name
     :color color
     ;; TODO: Better update somehow
   :update (lambda () (maccalfw-data-cache-clear cal-id))
     :data (lambda (begin end)
           (maccalfw-to-calendar cal-id begin end))))

(defun cfw:open-maccalfw-calendar (&optional calendars)
  "Simple calendar interface. This command displays all CALENDARS
obtained using `maccalfw-get-calendars' or all of them if it is 'all."
  (interactive (list 'all))
  (module-load (locate-library (expand-file-name
                                "~/Work/maccalfw/MacCalfw/.build/debug/libmaccalfw.dylib")
                               t))

  (when (eq calendars 'all)
    (setq calendars (maccalfw-get-calendars)))
  (save-excursion
    (let ((cp (cfw:create-calendar-component-buffer
               :view 'week
               :contents-sources
               (mapcar
                (lambda (x)
                  (maccalfw-create-source (plist-get x :title)
                                          (plist-get x :id)
                                          (plist-get x :color)))
                calendars))))
      (switch-to-buffer (cfw:cp-get-buffer cp)))))

;; (progn (eval-current-buffer) (cfw:open-ical-calendar "./ics/test.ics"))

(provide 'maccalfw)
;;; maccalfw.el ends here
