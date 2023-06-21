(require 'time-date)

(module-load (locate-library (expand-file-name
                              "./libmaccalfw.dylib")
                             t))

(condition-case err
    (maccalfw-test '(:hello "World" :test "testing" :id "MyID"))
  (error
   (message "an error occured \"%s\"!" (error-message-string err))))


;; (require 'maccalfw)
;; (require 'calfw)

;; (let ((calendars (maccalfw-get-calendars))
;;       (start (encode-time (make-decoded-time
;;                            :second 0 :minute 0 :hour 0
;;                            :day 31 :month 05 :year 2023)
;;                        ))
;;       (end (current-time))
;;       cal-id
;;       events-list)
;;   (setq cal-id
;;         (plist-get
;;    (cl-find-if (lambda (x) (equal (plist-get x :title) "Work"))
;;                calendars)
;;    :id))

;;   (setq events-list (maccalfw-fetch-events
;;                      cal-id
;;                      start end))

;;   (print (car events-list))

;;   (dolist (event events-list)
;;     (message "Title: %s" (plist-get event :title))
;;     (message "Start date: %s"
;;              (format-time-string "%F %T" (plist-get event :start)))
;;     (message "End date: %s"
;;              (format-time-string "%F %T" (plist-get event :end)))
;;     (message "All day: %S" (plist-get event :all-day-p))
;;     (message "--------------------")
;;     )
;;   )
