(require 'time-date)

(module-load (locate-library (expand-file-name
                              "./.build/debug/libmaccalfw.dylib")
                             t))

;; (message "%S" (maccalfw-fetch-events "Hello" 0 0))

;; (message "Events: %S" (maccalfw-fetch-events
;;                        "Work"
;;                        (make-time)
;;                        ))
;; (current-time)

(let ((calendars (maccalfw-get-calendars))
      (start (encode-time (make-decoded-time
                           :second 0 :minute 0 :hour 0
                           :day 31 :month 05 :year 2023)
                          ))
      (end (current-time))
      cal-id
      events-list)
  (message "Calendars: %S" calendars)

  (setq cal-id
        (plist-get
   (cl-find-if (lambda (x) (equal (plist-get x :title) "Work"))
               calendars)
   :id))

  (message "Cal ID: %s" cal-id)

  (setq events-list (maccalfw-fetch-events
                     cal-id
                     start end))

  (message "%S" events-list)
  )
