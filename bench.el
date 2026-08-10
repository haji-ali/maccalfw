;;; bench.el --- Benchmark maccalfw calendar operations -*- lexical-binding: t; -*-

;; Measures per-call latency of maccalfw-get-calendars, maccalfw-timezones,
;; and maccalfw-fetch-events, to compare the old in-process dynamic-module
;; backend against the new maccalq subprocess backend.
;;
;; Usage (from this checkout's directory):
;;   emacs -Q --batch -l bench.el
;;
;; Run once in this checkout (whichever backend it has -- old `main` or
;; new `cli-executable`) and once in the other, then compare the two
;; reports. The backend is auto-detected from which functions are bound
;; after `require`.

;;; Code:

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path default-directory)
  ;; calfw is only referenced inside maccalfw.el's function bodies, not
  ;; at load time, so a stub `provide' is enough if it's not installed
  ;; in this batch Emacs (-Q loads no packages).
  (unless (require 'calfw nil t)
    (provide 'calfw))
  (require 'ical-form)
  (require 'maccalfw))

(defun bench--stats (times)
  "Return a plist of stats for TIMES, a list of durations in seconds."
  (let* ((n (length times))
         (sorted (sort (copy-sequence times) #'<))
         (sum (apply #'+ times))
         (mean (/ sum n))
         (variance (/ (apply #'+ (mapcar (lambda (x) (expt (- x mean) 2)) times))
                     n)))
    (list :n n :mean mean :median (nth (/ n 2) sorted)
          :min (car sorted) :max (car (last sorted))
          :stddev (sqrt variance))))

(defun bench--ms (seconds) (format "%.1fms" (* seconds 1000)))

(defun bench--run (label n thunk)
  "Call THUNK N times (plus one untimed warmup call), print stats."
  (funcall thunk)
  (let (times)
    (dotimes (_ n)
      (let ((start (float-time)))
        (funcall thunk)
        (push (- (float-time) start) times)))
    (let ((stats (bench--stats (nreverse times))))
      (message "%-30s n=%-3d mean=%-8s median=%-8s min=%-8s max=%-8s stddev=%s"
               label (plist-get stats :n)
               (bench--ms (plist-get stats :mean))
               (bench--ms (plist-get stats :median))
               (bench--ms (plist-get stats :min))
               (bench--ms (plist-get stats :max))
               (bench--ms (plist-get stats :stddev)))
      stats)))

(let ((setup-start (float-time)))
  (cond
   ((fboundp 'maccalfw--cli-ensure)
    (message "=== Backend: new CLI subprocess (%s) ===" default-directory)
    (maccalfw--cli-ensure))
   ((fboundp 'maccalfw--load-module)
    (message "=== Backend: old in-process dynamic module (%s) ==="
             default-directory)
    (maccalfw--load-module))
   (t (error "Neither maccalfw--cli-ensure nor maccalfw--load-module is bound")))
  (message "Backend ready in %s (not counted in per-call stats below)"
           (bench--ms (- (float-time) setup-start))))

(let ((start (current-time))
      (end (time-add (current-time) (* 7 24 60 60))))
  (bench--run "maccalfw-get-calendars" 20 #'maccalfw-get-calendars)
  (bench--run "maccalfw-timezones" 20 #'maccalfw-timezones)
  (bench--run "maccalfw-fetch-events (1 week, all cals)" 20
              (lambda () (maccalfw-fetch-events nil start end))))

(message "Done.")

;;; bench.el ends here
