;;; maccalfw.el --- calendar view for ical format

;; Copyright (C) 2011  SAKURAI Masashi

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

(defun maccalfw-convert-event (event)
  (let ((start (decode-time (plist-get event :start)))
        (end (decode-time (plist-get event :end)))
        (all-day-p (plist-get event :all-day-p)))
    (make-cfw:event
     :start-date  (list (decoded-time-month start)
                        (decoded-time-day start)
                        (decoded-time-year start))
     :start-time  (unless all-day-p
                    (list (decoded-time-hour start)
                          (decoded-time-minute start)))
     :end-date    (list (decoded-time-month end)
                        (decoded-time-day end)
                        (decoded-time-year end))
     :end-time    (unless all-day-p
                    (list (decoded-time-hour end)
                          (decoded-time-minute end)))
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

(defvar maccalfw-data-cache nil "a list of (url . ics-data)")

(defun maccalfw-data-cache-clear (url)
  (setq maccalfw-data-cache
        (cl-loop for i in maccalfw-data-cache
              for (u . d) = i
              unless (equal u url)
              collect i)))

(defun maccalfw-data-cache-clear-all ()
  (interactive)
  (setq maccalfw-data-cache nil))

(defun maccalfw-to-calendar (cal-id begin end)
  (cl-loop for event in
        (maccalfw-convert-to-calfw
         (maccalfw-fetch-events cal-id begin end))
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
  (lexical-let ((url url))
    (make-cfw:source
     :name name
     :color color
     ;; TODO: Better update somehow
     :update (lambda () (maccalfw-data-cache-clear url))
     :data (lambda (begin end)
             (maccalfw-to-calendar cal-id begin end)))))

(defun cfw:open-maccalfw-calendar (calendars)
  "Simple calendar interface. This command displays all CALENDARS
obtained using `maccalfw-get-calendars' or all of them if it is 'all."
  (interactive)
  (module-load (locate-library (expand-file-name
                                "~/Work/maccalfw/MacCalfw/.build/debug/libmaccalfw.dylib")
                               t))

  (when (eq cal-ids 'all)
    (setq cal-ids
          (maccalfw-get-calendars)))
  (save-excursion
    (let ((cp (cfw:create-calendar-component-buffer
               :view 'month
               :contents-sources
               (mapcar
                (lambda (x)
                  (maccalfw-create-source (plist-get :title x)
                                          (plist-get :id x)
                                          (plist-get :color x)))
                calendars))))
      (switch-to-buffer (cfw:cp-get-buffer cp)))))

;; (progn (eval-current-buffer) (cfw:open-ical-calendar "./ics/test.ics"))

(provide 'maccalfw)
;;; maccalfw.el ends here
