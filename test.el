;;; test.el --- Tests for maccalfw.el -*- lexical-binding: t; -*-

;; These exercise maccalfw.el's CLI-calling plumbing (argument
;; building, stdin delivery, response parsing, error handling, the
;; calendars/timezones translation to their pre-existing plist/alist
;; shape) against tests/fake-maccalq.sh, a stand-in for maccalq. None
;; of this needs EventKit or a Swift build.
;;
;; Run via `make test' or
;; `emacs -Q --batch --load test.el -f ert-run-tests-batch-and-exit'.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; calfw's functions are only referenced inside maccalfw.el's function
;; bodies, not at load time, so a stub `provide' satisfies its
;; `require' without needing the real package installed.
(unless (featurep 'calfw)
  (provide 'calfw))

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path default-directory)
  (require 'maccalfw))

(defvar test-maccalfw--fake-cli
  (expand-file-name
   "tests/fake-maccalq.sh"
   (file-name-directory (or load-file-name buffer-file-name))))

(defmacro test-maccalfw--with-fake-mode (mode &rest body)
  "Run BODY with `maccalfw--cli-executable' set to the fake CLI.
The fake CLI selects its canned response from the FAKE_MODE
environment variable, set to MODE here."
  (declare (indent 1))
  `(let ((maccalfw--cli-executable test-maccalfw--fake-cli)
         (process-environment
          (cons (concat "FAKE_MODE=" ,mode) process-environment)))
     ,@body))

;;; Argument building

(ert-deftest test-maccalfw-cli-build-args-simple ()
  (should (equal '("events" "--start" "s" "--end" "e")
                 (maccalfw--cli-build-args "events" '(:start "s" :end "e")))))

(ert-deftest test-maccalfw-cli-build-args-nil-omitted ()
  (should (equal '("event" "--id" "x")
                 (maccalfw--cli-build-args "event" '(:id "x" :start nil)))))

(ert-deftest test-maccalfw-cli-build-args-boolean-flag ()
  (should (equal '("remove-event" "--id" "x" "--future")
                 (maccalfw--cli-build-args
                  "remove-event" '(:id "x" :future t)))))

(ert-deftest test-maccalfw-cli-build-args-repeated-flag ()
  (should (equal '("events" "--calendar" "a" "--calendar" "b")
                 (maccalfw--cli-build-args "events" '(:calendar ("a" "b"))))))

(ert-deftest test-maccalfw-cli-build-args-stringifies-non-strings ()
  (should (equal '("events" "--format" "elisp")
                 (maccalfw--cli-build-args "events" '(:format elisp)))))

;;; Low-level process invocation

(ert-deftest test-maccalfw-cli-run-passes-args ()
  (test-maccalfw--with-fake-mode "echo-args"
    (should (equal '(0 . "((ARGS nil \"events --start s --end e\"))")
                   (maccalfw--cli-run
                    test-maccalfw--fake-cli
                    '("events" "--start" "s" "--end" "e") nil)))))

(ert-deftest test-maccalfw-cli-run-delivers-stdin ()
  (test-maccalfw--with-fake-mode "echo-stdin"
    (should (equal '(0 . "((STDIN nil \"say \\\"hi\\\" \\\\ bye\"))")
                   (maccalfw--cli-run
                    test-maccalfw--fake-cli '("update-event")
                    "say \"hi\" \\ bye")))))

;;; High-level call + error handling

(ert-deftest test-maccalfw-cli-call-parses-response ()
  (test-maccalfw--with-fake-mode "echo-args"
    (should (equal '((ARGS nil "events --foo bar --format elisp"))
                   (maccalfw--cli-call "events" '(:foo "bar"))))))

(ert-deftest test-maccalfw-cli-call-not-authorized ()
  (test-maccalfw--with-fake-mode "not-authorized"
    (let ((err (should-error (maccalfw--cli-call "calendars")
                             :type 'maccalfw-not-authorized)))
      (should (equal "Calendar access was not granted" (cadr err))))))

(ert-deftest test-maccalfw-cli-call-general-error ()
  (test-maccalfw--with-fake-mode "general-error"
    (let ((err (should-error (maccalfw--cli-call "calendars")
                             :type 'maccalfw-error)))
      (should (equal "Something went wrong" (cadr err))))))

;;; calendars/timezones translation to the pre-existing shape

(ert-deftest test-maccalfw-get-calendars-shape ()
  (test-maccalfw--with-fake-mode "calendars"
    (let ((cals (maccalfw-get-calendars)))
      (should (= 2 (length cals)))
      (should (equal "Home" (plist-get (nth 0 cals) :title)))
      (should (eq t (plist-get (nth 0 cals) :editable)))
      (should (eq t (plist-get (nth 0 cals) :default)))
      (should (null (plist-get (nth 1 cals) :editable)))
      (should (null (plist-get (nth 1 cals) :default))))))

(ert-deftest test-maccalfw-timezones-shape ()
  (test-maccalfw--with-fake-mode "timezones"
    (let ((tzs (maccalfw-timezones)))
      (should (= 2 (length tzs)))
      (should (equal "America/New_York" (car (nth 0 tzs))))
      (should (equal -18000 (plist-get (cdr (nth 0 tzs)) :offset)))
      (should (eq t (plist-get (cdr (nth 0 tzs)) :default)))
      (should (null (plist-get (cdr (nth 1 tzs)) :default))))))

;;; fetch-events

(ert-deftest test-maccalfw-fetch-events-calendar-id-normalization ()
  (test-maccalfw--with-fake-mode "echo-args"
    (let ((start (encode-time (list 0 0 9 1 1 2026 nil nil 0)))
          (end (encode-time (list 0 0 10 1 1 2026 nil nil 0))))
      (should (equal
               '((ARGS nil "events --start 2026-01-01T09:00:00Z --end 2026-01-01T10:00:00Z --format elisp"))
               (maccalfw-fetch-events nil start end)))
      (should (equal
               '((ARGS nil "events --start 2026-01-01T09:00:00Z --end 2026-01-01T10:00:00Z --calendar cal-1 --format elisp"))
               (maccalfw-fetch-events "cal-1" start end)))
      (should (equal
               '((ARGS nil "events --start 2026-01-01T09:00:00Z --end 2026-01-01T10:00:00Z --calendar cal-1 --calendar cal-2 --format elisp"))
               (maccalfw-fetch-events '("cal-1" "cal-2") start end))))))

(ert-deftest test-maccalfw-fetch-events-response-shape ()
  (test-maccalfw--with-fake-mode "events"
    (should (equal
             '(((UID nil "evt-1") (SUMMARY nil "Party")
                (DTSTART nil "20260101T090000Z") (DTEND nil "20260101T100000Z")))
             (maccalfw-fetch-events nil (current-time) (current-time))))))

;;; update-event / remove-event

(ert-deftest test-maccalfw-update-event-stdin-round-trip ()
  (test-maccalfw--with-fake-mode "echo-stdin"
    (let* ((changed-data '((SUMMARY nil "New title")
                           (DESCRIPTION nil "line one\nline two \"quoted\"")))
           (response (maccalfw-update-event nil changed-data))
           (stdin (nth 2 (assoc 'STDIN response))))
      (should (equal changed-data (car (read-from-string stdin)))))))

(ert-deftest test-maccalfw-remove-event-returns-t ()
  (test-maccalfw--with-fake-mode "remove-ok"
    (should (eq t (maccalfw-remove-event "evt-1")))))

;;; Combined-fetch caching (maccalfw--fetch-events-cached)
;;
;; calfw queries every configured calendar as a separate source, so
;; without this, N calendars means N maccalq subprocess calls per
;; redraw. These mock maccalfw-fetch-events directly (not the CLI) to
;; test the caching/splitting logic that sits above it.

(defun test-maccalfw--fake-event (cal-id uid)
  "A minimal ical-form-shaped event alist tagged with CAL-ID."
  `((UID nil ,uid) (X-EMACS-CALID nil ,cal-id)))

(ert-deftest test-maccalfw-fetch-events-cached-dedupes-same-range ()
  (let ((maccalfw--events-cache nil)
        (call-count 0))
    (cl-letf (((symbol-function 'maccalfw-fetch-events)
               (lambda (cal-ids &rest _)
                 (setq call-count (1+ call-count))
                 (mapcar (lambda (id) (test-maccalfw--fake-event id id)) cal-ids))))
      (let ((begin '(1 1 2026)) (end '(1 7 2026)))
        ;; Three "sources" querying the same range, as calfw does.
        (maccalfw--fetch-events-cached '("a" "b" "c") begin end)
        (maccalfw--fetch-events-cached '("a" "b" "c") begin end)
        (maccalfw--fetch-events-cached '("a" "b" "c") begin end)
        (should (= 1 call-count))))))

(ert-deftest test-maccalfw-fetch-events-cached-splits-by-calendar ()
  (let ((maccalfw--events-cache nil))
    (cl-letf (((symbol-function 'maccalfw-fetch-events)
               (lambda (cal-ids &rest _)
                 (mapcar (lambda (id) (test-maccalfw--fake-event id id)) cal-ids))))
      (let ((by-cal (maccalfw--fetch-events-cached '("a" "b") '(1 1 2026) '(1 7 2026))))
        (should (equal (list (test-maccalfw--fake-event "a" "a")) (gethash "a" by-cal)))
        (should (equal (list (test-maccalfw--fake-event "b" "b")) (gethash "b" by-cal)))
        (should (null (gethash "c" by-cal)))))))

(ert-deftest test-maccalfw-fetch-events-cached-different-range-refetches ()
  (let ((maccalfw--events-cache nil)
        (call-count 0))
    (cl-letf (((symbol-function 'maccalfw-fetch-events)
               (lambda (&rest _) (setq call-count (1+ call-count)) nil)))
      (maccalfw--fetch-events-cached '("a") '(1 1 2026) '(1 7 2026))
      (maccalfw--fetch-events-cached '("a") '(1 8 2026) '(1 14 2026))
      (should (= 2 call-count)))))

(ert-deftest test-maccalfw-fetch-events-cached-different-cal-ids-refetches ()
  (let ((maccalfw--events-cache nil)
        (call-count 0))
    (cl-letf (((symbol-function 'maccalfw-fetch-events)
               (lambda (&rest _) (setq call-count (1+ call-count)) nil)))
      (maccalfw--fetch-events-cached '("a") '(1 1 2026) '(1 7 2026))
      (maccalfw--fetch-events-cached '("a" "b") '(1 1 2026) '(1 7 2026))
      (should (= 2 call-count)))))

(ert-deftest test-maccalfw-fetch-events-cached-ttl-expires ()
  (let ((maccalfw--events-cache nil)
        (call-count 0))
    (cl-letf (((symbol-function 'maccalfw-fetch-events)
               (lambda (&rest _) (setq call-count (1+ call-count)) nil)))
      (maccalfw--fetch-events-cached '("a") '(1 1 2026) '(1 7 2026))
      ;; Simulate the cache entry having aged past the TTL.
      (setf (nth 1 maccalfw--events-cache)
            (- (float-time) maccalfw--events-cache-ttl 1))
      (maccalfw--fetch-events-cached '("a") '(1 1 2026) '(1 7 2026))
      (should (= 2 call-count)))))

(ert-deftest test-maccalfw-invalidate-events-cache-forces-refetch ()
  (let ((maccalfw--events-cache nil)
        (call-count 0))
    (cl-letf (((symbol-function 'maccalfw-fetch-events)
               (lambda (&rest _) (setq call-count (1+ call-count)) nil)))
      (maccalfw--fetch-events-cached '("a") '(1 1 2026) '(1 7 2026))
      (maccalfw--invalidate-events-cache)
      (maccalfw--fetch-events-cached '("a") '(1 1 2026) '(1 7 2026))
      (should (= 2 call-count)))))

(ert-deftest test-maccalfw-remove-event-invalidates-cache ()
  (test-maccalfw--with-fake-mode "remove-ok"
    (setq maccalfw--events-cache (list "sentinel" (float-time) (make-hash-table)))
    (maccalfw-remove-event "evt-1")
    (should (null maccalfw--events-cache))))

(ert-deftest test-maccalfw-update-event-invalidates-cache ()
  (test-maccalfw--with-fake-mode "remove-ok"
    (setq maccalfw--events-cache (list "sentinel" (float-time) (make-hash-table)))
    (maccalfw-update-event nil '((SUMMARY nil "x")))
    (should (null maccalfw--events-cache))))

(provide 'test)
;;; test.el ends here
