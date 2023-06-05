(require 'time-date)

(add-to-list load-path
             (expand-file-name "~/.config/emacs/.elocal/straight/repos/emacs-calfw/"))

(add-to-list load-path
             (expand-file-name "~/.config/emacs/init.d/"))

(module-load (locate-library (expand-file-name
                              "./.build/debug/libmaccalfw.dylib")
                             t))

(require 'maccalfw)
(require 'calfw)

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
  (setq cal-id
        (plist-get
   (cl-find-if (lambda (x) (equal (plist-get x :title) "Work"))
               calendars)
   :id))

  (setq events-list (maccalfw-fetch-events
                     cal-id
                     start end))

  (print (car events-list))

  (dolist (event events-list)
    (message "Title: %s" (plist-get event :title))
    (message "Start date: %s"
             (format-time-string "%F %T" (plist-get event :start)))
    (message "End date: %s"
             (format-time-string "%F %T" (plist-get event :end)))
    (message "All day: %S" (plist-get event :all-day-p))
    (message "--------------------")
    )
  )
