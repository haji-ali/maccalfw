(require 'time-date)

(module-load (locate-library (expand-file-name "./libmaccalfw.dylib") t))

(condition-case err
    (maccalfw--test '(:hello "World" :test "testing" :id "MyID" :wrong))
  (error
   (message "an error occured \"%s\"!" (error-message-string err))))

(let ((calendars (maccalfw-get-calendars))
      (start
       (encode-time (list 0 0 0 19 6 2023)))
      (end
       (encode-time (list 0 0 0 20 6 2023)))
      cal-home-id
      cal-work-id
      events-list
      event)
  (setq cal-work-id
        (plist-get
         (cl-find-if (lambda (x) (equal (plist-get x :title) "Calendar"))
                     calendars)
         :id))

  (message "Cal id: %S" cal-home-id)
  (setq events-list (maccalfw-fetch-events
                     cal-work-id
                     start end))
  (setq event (car events-list))

  (setq ret-val events-list)
  (message "Return value: %S" (length events-list ))
  (message "Return value: %S" ret-val))
