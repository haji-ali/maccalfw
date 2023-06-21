(require 'time-date)

(module-load (locate-library (expand-file-name
                              "./libmaccalfw.dylib")
                             t))

(condition-case err
    (maccalfw--test '(:hello "World" :test "testing" :id "MyID" :wrong))
  (error
   (message "an error occured \"%s\"!" (error-message-string err))))


;; (require 'maccalfw)
;; (require 'calfw)

(let ((calendars (maccalfw-get-calendars))
      (start
       (encode-time (list 0 0 0 19 6 2023)))
      (end
       (encode-time (list 0 0 0 20 6 2023)))
      cal-home-id
      cal-work-id
      events-list
      event)
  (setq cal-home-id
        (plist-get
         (cl-find-if (lambda (x) (equal (plist-get x :title) "Home"))
                     calendars)
         :id))
  (setq cal-work-id
        (plist-get
         (cl-find-if (lambda (x) (equal (plist-get x :title) "Work"))
                     calendars)
         :id))

  (message "Cal id: %S" cal-home-id)
  (setq events-list (maccalfw-fetch-events
                     cal-home-id
                     start end))
  (setq event (car events-list))

  (plist-put event :title "New one")
  (plist-put event :notes "Completely new notes")
  (plist-put event :url "http://relaxng.org/ns/structure/1.0")
  (plist-put event :id nil)

  (print event)
  ;;(setq ret-val (maccalfw-update-event event))
  ;;(message "Return value: %S" ret-val)
  )

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
