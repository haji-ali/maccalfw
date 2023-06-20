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

;;; Code:

(require 'calfw)
(require 'calfw-blocks)

(defun maccalfw--decode-date (date)
  (list (decoded-time-month date)
        (decoded-time-day date)
        (decoded-time-year date)))

(defun maccalfw--encode-date (date &optional end-of-day)
  (encode-time (append
                (if end-of-day
                    (list 59 59 23)
                  (list 0 0 0))
                (list (nth 1 date)
                     (nth 0 date)
                      (nth 2 date)))))

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

(defun maccalfw-to-calendar (cal-id begin end)
  (cl-loop for event in
        (maccalfw-convert-to-calfw
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

(defun maccalfw-create-source (name cal-id color)
  (make-cfw:source
   :name name
   :color color
   ;; TODO: Better update somehow
   :update #'ignore
   ;;:hidden nil
   :data (lambda (begin end)
           (maccalfw-to-calendar cal-id begin end))))

(defun maccalfw-get-calendars-by-name (names)
  (--filter
   (member (plist-get it :title) names)
   (maccalfw-get-calendars)))

(defun maccalfw--load-module (force)
  (unless (and (not force) (fboundp #'maccalfw-get-calendars))
    (unless module-file-suffix
      (error "Jinx: Dynamic modules are not supported"))
    (let* ((mod-name (file-name-with-extension
                      "MacCalfw/.build/release/libmaccalfw/maccalfw"
                      module-file-suffix))
           (mod-file (locate-library mod-name t)))
      (unless (mod-file and (not (eq force 'compile)))
        (let* ((swift (or (getenv "SWIFTC")
                          (executable-find "swiftc")
                          (error "Jinx: No swift compiler found")))
               (default-directory (file-name-directory
                                   (or (locate-library c-name t)
                                       (error "Jinx: %s not found" c-name))))
               (command
                `(,swift "build" "-c" "release" "-Xswiftc"
                         "-I/opt/homebrew/include/" ;; TODO: Better way of
                         ;; doing this?
                         "-o" ,mod-name ,c-name
                         ,@(split-string-and-unquote
                            (condition-case nil
                                (car (process-lines "pkg-config" "--cflags" "--libs" "enchant-2"))
                              (error "-I/usr/include/enchant-2 -lenchant-2"))))))
          (with-current-buffer (get-buffer-create "*jinx module compilation*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (compilation-mode)
              (insert (string-join command " ") "\n")
              (if (equal 0 (apply #'call-process (car command) nil (current-buffer) t (cdr command)))
                  (insert (message "Jinx: %s compiled successfully" mod-name))
                (let ((msg (format "Jinx: Compilation of %s failed" mod-name)))
                  (insert msg)
                  (pop-to-buffer (current-buffer))
                  (error msg)))))
          (setq mod-file (expand-file-name mod-name))))
      (module-load mod-file))))

(defun maccalfw (&optional calendars)
  "Simple calendar interface. This command displays all CALENDARS
obtained using `maccalfw-get-calendars' or all of them if it is 'all."
  (interactive (list 'all))
  (module-load (locate-library (expand-file-name
                                "~/Work/maccalfw/test.dylib")
                               t))
  (when (eq calendars 'all)
    (setq calendars (maccalfw-get-calendars)))
  (save-excursion
    (let ((cp (cfw:create-calendar-component-buffer
               :view 'block-week
               :contents-sources
               (mapcar
                (lambda (x)
                  (maccalfw-create-source (plist-get x :title)
                                          (plist-get x :id)
                                          (plist-get x :color)))
                calendars))))
      (switch-to-buffer (cfw:cp-get-buffer cp)))))

;; (cfw:open-maccalfw-calendar
;;  (cfw:get-calendars-by-name '("iCloud Family")))

(provide 'maccalfw)
;;; maccalfw.el ends here
