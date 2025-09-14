(require 'time-date)

(module-load (locate-library (expand-file-name "./libmaccalfw.dylib") t))
;; (module-load (locate-library (file-name-with-extension "libmaccalfw"
;;                                                        module-file-suffix)
;;                              t))

;; (message "%S" (cl-find-if
;;                (lambda (x) (plist-get x :default))
;;                (maccalfw-timezones)))

;; (condition-case err
;;     (maccalfw--test '(:hello "World" :test "testing" :id "MyID" :wrong))
;;   (error
;;    (message "an error occured \"%s\"!" (error-message-string err))))

;; (let ((calendars (maccalfw-get-calendars))
;;       (start
;;        (encode-time (list 0 0 0 19 6 2023)))
;;       (end
;;        (encode-time (list 0 0 0 20 6 2023)))
;;       cal-home-id
;;       cal-work-id
;;       events-list
;;       event)
;;   (setq cal-work-id
;;         (plist-get
;;          (cl-find-if (lambda (x) (equal (plist-get x :title) "Calendar"))
;;                      calendars)
;;          :id))

;;   (message "Cal id: %S" cal-home-id)
;;   (setq events-list (maccalfw-fetch-events
;;                      cal-work-id
;;                      start end))
;;   (setq event (car events-list))

;;   (setq ret-val events-list)
;;   (message "Return value: %S" (length events-list ))
;;   (message "Return value: %S" ret-val))

;; (defun maccalfw-get-calendars-by-name (names)
;;   "Return the calendar IDs with NAMES."
;;   (cl-remove-if-not
;;    (lambda (x) (member (plist-get x :title) names))
;;    (maccalfw-get-calendars)))

;; (let* ((calendars (maccalfw-get-calendars-by-name '("Calendar")))
;;        (start     (encode-time (list 0 0 0 20 6 2023)))
;;        (end       (encode-time (list 0 0 10 20 6 2023)))
;;        (cal-work-id (plist-get (car calendars) :id))
;;        events-list event)

;;   (setq events-list (maccalfw-fetch-events
;;                      cal-work-id
;;                      start end))
;;   ;; (setq event (car events-list))
;;   (message "%S" (plist-get (car events-list) :time-zone))
;;   )
;; (maccalfw--load-module 'compile)
