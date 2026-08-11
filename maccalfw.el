;;; maccalfw.el --- Calendar view for Mac Calendars -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Al Haji-Ali

;; Author: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Created: 2023
;; Version: 0.2
;; Package-Requires: ((emacs "29.1") (calfw "2.0") (ical-form "0.2"))
;; Homepage: https://github.com/haji-ali/maccalfw
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

;; Bridge from Mac Calendar to calfw.

;;; Installation:

;; Here is a minimum sample code:
;; (require 'maccalfw)
;; To open a calendar buffer, execute the following function.
;; (maccalfw-open 'all)

;;; Code:

(require 'calfw)
(require 'ical-form)
(require 'cl-lib)

(defvar maccalfw-modify-future-events-p 'ask
  "If non-nil, modifying events with recurrences applies to future events.
Special value \\='ask, prompts the user.")

(define-error 'maccalfw-error "maccalfw error")
(define-error 'maccalfw-not-authorized
              "Calendar/Reminders access not authorized" 'maccalfw-error)

(defvar maccalfw--cli-executable nil
  "Cached path to the built `maccalq' executable.
Set this directly to point at an alternative binary, e.g. in tests.")

(defun maccalfw--cli-bin-path (swift)
  "Return the directory SWIFT's release build output goes to."
  (with-temp-buffer
    (unless (equal 0 (call-process swift nil t nil
                                   "build" "-c" "release" "--show-bin-path"))
      (error "maccalfw: Failed to determine swift build output path"))
    (string-trim (buffer-string))))

(defun maccalfw--cli-build (&optional force)
  "Build maccalq via Swift Package Manager, returning its path.
Skip building if an up-to-date binary already exists and FORCE is
nil."
  (let* ((swift (or (getenv "SWIFT")
                    (executable-find "swift")
                    (error "maccalfw: No swift compiler found")))
         (default-directory (file-name-directory (locate-library "maccalfw")))
         (exe (expand-file-name
               "maccalq" (maccalfw--cli-bin-path swift))))
    (when (or force (not (file-executable-p exe)))
      (with-current-buffer (get-buffer-create "*maccalq build*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (compilation-mode)
          (insert (format "%s build -c release\n" swift))
          (if (equal 0 (call-process swift nil (current-buffer) t
                                     "build" "-c" "release"))
              (message "maccalfw: maccalq built successfully")
            ;; Don't assume a window/frame is available to pop the
            ;; output up in -- this can run during package
            ;; installation, before any frame exists, and popping up a
            ;; buffer would itself fail there, masking this error with
            ;; a confusing one about no window system being available.
            ;; Put the actual build output in the error text instead;
            ;; *maccalq build* still has it for later inspection.
            (error "maccalfw: Building maccalq failed:\n%s"
                   (buffer-string))))))
    exe))

(defun maccalfw--cli-ensure (&optional force)
  "Return the path to a built `maccalq', building it if needed."
  (when (or force (not maccalfw--cli-executable))
    (setq maccalfw--cli-executable (maccalfw--cli-build force)))
  maccalfw--cli-executable)

(defun maccalfw--cli-arg-string (value)
  "Convert VALUE to a string suitable as a single CLI argument."
  (if (stringp value) value (format "%s" value)))

(defun maccalfw--cli-build-args (command options)
  "Build a maccalq argv list for COMMAND with OPTIONS.
OPTIONS is a plist. A value of t means a boolean flag with no
value (e.g. :future t -> \"--future\"). A list value repeats the
flag once per element. nil values are omitted. Any other value
becomes a single \"--key value\" pair (converted to a string via
`maccalfw--cli-arg-string' if it isn't one already)."
  (let (args)
    (cl-loop for (key value) on options by #'cddr
             for flag = (concat "--" (string-remove-prefix
                                      ":" (symbol-name key)))
             do (cond
                 ((null value))
                 ((eq value t) (push flag args))
                 ((listp value)
                  (dolist (v value)
                    (push flag args)
                    (push (maccalfw--cli-arg-string v) args)))
                 (t (push flag args)
                    (push (maccalfw--cli-arg-string value) args))))
    (cons command (nreverse args))))

(defun maccalfw--cli-run (exe args input)
  "Run EXE with ARGS, writing INPUT (a string, or nil) to its stdin.
Return (EXIT-CODE . OUTPUT-STRING). maccalq writes its result to
stdout on success and a plain-text message to stderr on failure,
never both, so it's safe to capture them into the same string."
  (with-temp-buffer
    (when input (insert input))
    (let ((exit-code
           (apply #'call-process-region
                  (point-min) (point-max) exe
                  t (list t t) nil
                  args)))
      (cons exit-code (buffer-string)))))

(defun maccalfw--cli-read-response (output)
  "Read OUTPUT (a string) as a single elisp sexp."
  (car (read-from-string output)))

(defun maccalfw--cli-call (command &optional options input)
  "Run maccalq COMMAND with OPTIONS, returning its parsed result.
OPTIONS is a plist of CLI flags, see `maccalfw--cli-build-args'.
INPUT, if non-nil, is a string written as the CLI's stdin (used
by update-event to pass the changed event data).

Signals `maccalfw-not-authorized' on exit code 2 (access not
granted) or `maccalfw-error' on any other failure, using maccalq's
stderr message text."
  (let* ((exe (maccalfw--cli-ensure))
         (args (maccalfw--cli-build-args
                command (append options (list :format 'elisp))))
         (result (maccalfw--cli-run exe args input)))
    (if (equal 0 (car result))
        (maccalfw--cli-read-response (cdr result))
      (signal (if (equal (car result) 2)
                 'maccalfw-not-authorized
               'maccalfw-error)
              (list (string-trim
                     (string-remove-prefix "maccalq: " (cdr result))))))))

(defun maccalfw--iso8601 (time)
  "Format Emacs TIME as a UTC ISO8601 string for maccalq."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t))

(defun maccalfw--cli-triples-to-plist (triples &optional bool-keys)
  "Convert a list of (NAME PARAMS VALUE) TRIPLES to a plist.
Keys become lowercase keyword symbols (TITLE -> :title). Keys in
BOOL-KEYS are converted from \"yes\"/absent to t/nil instead of
being kept as strings."
  (cl-loop for (name _params value) in triples
           for key = (intern (concat ":" (downcase (symbol-name name))))
           append (list key (if (memq key bool-keys)
                               (equal value "yes")
                             value))))

(defun maccalfw-get-calendars (&optional type)
  "Return Mac calendars as plists (:id :title :color :editable :default).
TYPE is `event' (the default), `reminder', or `all'. When TYPE is
`all' and only one of Calendar/Reminders access is granted, the
calendars maccalq could still list are returned and the
authorization failure is reported with `message' instead of
aborting the whole call."
  (delq nil
        (mapcar
         (lambda (triples)
           (if (and (null (cdr triples)) (eq (caar triples) 'MESSAGE))
               (progn (message "maccalfw: %s" (nth 2 (car triples))) nil)
             (maccalfw--cli-triples-to-plist triples '(:editable :default))))
         (maccalfw--cli-call
          "calendars" (list :type (symbol-name (or type 'event)))))))

(defun maccalfw-timezones ()
  "Return system timezones as an alist of (ID . PLIST).
PLIST has :name, :abbrev, :offset (an integer, seconds from GMT),
and :default when applicable."
  (mapcar
   (lambda (triples)
     (let* ((plist (maccalfw--cli-triples-to-plist triples '(:default)))
            (id (plist-get plist :id)))
       (cons id (cl-loop for (k v) on plist by #'cddr
                         unless (eq k :id)
                         append (list k (if (eq k :offset)
                                           (string-to-number v)
                                         v))))))
   (maccalfw--cli-call "timezones")))

(defun maccalfw-fetch-events (calendar-id start-time end-time)
  "Return events between START-TIME and END-TIME.
CALENDAR-ID may be nil (all calendars), a single calendar ID
string, or a list of calendar IDs."
  (maccalfw--cli-call
   "events"
   (list :start (maccalfw--iso8601 start-time)
         :end (maccalfw--iso8601 end-time)
         :calendar (cond ((null calendar-id) nil)
                        ((stringp calendar-id) (list calendar-id))
                        (t calendar-id)))))

(defun maccalfw-fetch-reminders (calendar-id &optional include-completed)
  "Return reminders (VTODOs) from CALENDAR-ID.
CALENDAR-ID may be nil (all reminder calendars), a single calendar ID
string, or a list of calendar IDs.  Unlike `maccalfw-fetch-events',
this isn't bounded by a date range -- reminders may have no due date
at all -- so it returns every reminder in scope.  Completed reminders
are omitted unless INCLUDE-COMPLETED is non-nil."
  (maccalfw--cli-call
   "reminders"
   (list :calendar (cond ((null calendar-id) nil)
                        ((stringp calendar-id) (list calendar-id))
                        (t calendar-id))
         :include-completed (and include-completed t))))

(defun maccalfw-update-event (id changed-data &optional start future)
  "Update or create an event, returning the saved event's data.
ID is the event identifier, or nil to create a new event.
CHANGED-DATA is an alist of the fields to change, in `ical-form'
format. START disambiguates recurring event instances sharing an
ID. If FUTURE is non-nil, all future occurrences are updated."
  (prog1
      (maccalfw--cli-call
       "update-event"
       (list :id id
             :start (and start (maccalfw--iso8601 start))
             :future (and future t))
       (prin1-to-string changed-data))
    (maccalfw--invalidate-items-cache)))

(defun maccalfw-remove-event (id &optional start future)
  "Remove event ID, returning t on success.
START disambiguates recurring event instances sharing an ID. If
FUTURE is non-nil, all future occurrences are removed."
  (maccalfw--cli-call
   "remove-event"
   (list :id id
         :start (and start (maccalfw--iso8601 start))
         :future (and future t)))
  (maccalfw--invalidate-items-cache)
  t)

(defun maccalfw--decode-date (time)
  "Return a calendar date from encoded TIME.
The return value is (month day year)."
  (list (decoded-time-month time)
        (decoded-time-day time)
        (decoded-time-year time)))

(defun maccalfw--encode-date (date &optional end-of-day)
  "Encode a calendar DATE.
DATE is of the format (month day year). If END-OF-DAY is nil, the
time is midnight, otherwise it is a second before midnight of the
next day."
  (encode-time (append
                (if end-of-day
                    (list 59 59 23)
                  (list 0 0 0))
                (list (nth 1 date)
                      (nth 0 date)
                      (nth 2 date)))))

(defun maccalfw--decode-time (time)
  "Return a calendar time from encoded TIME."
  (list (decoded-time-hour time)
        (decoded-time-minute time)))

(defun maccalfw--convert-event (event)
  "Convert an EVENT to a calfw event.
The event is returned `maccalfw-fetch-events'."
  (let* ((dt-start (ical-form-event-get event 'DTSTART t))
         (start (decode-time (car dt-start)))
         (end (decode-time (ical-form-event-get event 'DTEND)))
         (all-day-p (alist-get 'ALL-DAY-P (cdr dt-start)))
         (args
          (list
           :start-date  (maccalfw--decode-date start)
           :start-time  (unless all-day-p
                          (maccalfw--decode-time start))
           :end-date    (maccalfw--decode-date end)
           :end-time    (unless all-day-p
                          (maccalfw--decode-time end))
           :title       (ical-form-event-get event 'SUMMARY)
           :location    (ical-form-event-get event 'LOCATION)
           :description (ical-form-event-get event 'DESCRIPTION))))
    (when (and (alist-get 'status (cl-struct-slot-info 'calfw-event))
               (alist-get 'data (cl-struct-slot-info 'calfw-event)))
      (setq args
            (append args (list
                          :status (ical-form-event-get event 'STATUS)
                          :data        event))))
    (apply #'make-calfw-event args)))

(defun maccalfw--convert-reminder (reminder)
  "Convert REMINDER to a calfw event.
REMINDER is one of the items returned by `maccalfw-fetch-reminders'.
Unlike an event, a reminder has no inherent duration, so it is always
converted to a single-instant span (same start and end): all-day on
its DUE date if DUE has no time-of-day, at the DUE time itself
otherwise (rendered with a default block height by calfw-blocks, see
`calfw-blocks-default-event-length'). A reminder with no DUE at all is
shown today, all-day."
  (let* ((due (ical-form-event-get reminder 'DUE t))
         (all-day-p (if due (alist-get 'ALL-DAY-P (cdr due)) t))
         (start (decode-time (or (car due) (current-time))))
         (args
          (list
           :start-date  (maccalfw--decode-date start)
           :start-time  (unless all-day-p
                          (maccalfw--decode-time start))
           :end-date    (maccalfw--decode-date start)
           :end-time    (unless all-day-p
                          (maccalfw--decode-time start))
           :title       (ical-form-event-get reminder 'SUMMARY)
           :location    (ical-form-event-get reminder 'LOCATION)
           :description (ical-form-event-get reminder 'DESCRIPTION))))
    (when (and (alist-get 'status (cl-struct-slot-info 'calfw-event))
               (alist-get 'data (cl-struct-slot-info 'calfw-event)))
      (setq args
            (append args (list
                          :status (ical-form-event-get reminder 'STATUS)
                          :data        reminder))))
    (apply #'make-calfw-event args)))

(defun maccalfw--convert-to-calfw (events-list &optional converter)
  "Convert an EVENTS-LIST to calfw events using CONVERTER.
CONVERTER defaults to `maccalfw--convert-event'; pass
`maccalfw--convert-reminder' to convert reminders instead."
  (cl-loop with converter = (or converter #'maccalfw--convert-event)
           for e in events-list
           for event = (funcall converter e)
           if event
           if (not (or (calfw-event-start-time event)
                       (calfw-event-end-time event)))
           collect event into periods
           else
           collect event into contents
           else do
           (progn
             (message "Ignoring event \"%s\"" e)
             (message "Cannot handle this event, tag: %s" e))
           finally return `((periods ,periods) ,@contents)))

(defvar maccalfw--items-cache nil
  "Cache (KEY TIMESTAMP . ITEMS-BY-CALENDAR) of the most recent combined
`maccalfw-fetch-events'/`maccalfw-fetch-reminders' call. See
`maccalfw--fetch-items-cached'.")

(defconst maccalfw--items-cache-ttl 2.0
  "How long, in seconds, `maccalfw--items-cache' stays valid.
calfw queries every configured calendar as a separate source, but
they all query the same visible date range synchronously,
microseconds apart, on every redraw -- long enough to dedupe that
into one fetch call per TYPE instead of one per calendar; short
enough that staying on the same range for a while still notices
external changes (e.g. synced from another device) rather than
showing stale data indefinitely. Edits made through
`maccalfw-update-event'/`maccalfw-remove-event' also invalidate the
cache directly, so a refresh right after an edit is never stale
regardless of the TTL.")

(defun maccalfw--invalidate-items-cache ()
  "Discard `maccalfw--items-cache', forcing the next fetch to be fresh."
  (setq maccalfw--items-cache nil))

(defun maccalfw--fetch-items-cached (type cal-ids begin end)
  "Return a hash table of CAL-ID -> items of TYPE, from CAL-IDS.
TYPE is `event' or `reminder' (see `maccalfw-get-calendars'); it picks
`maccalfw-fetch-events' or `maccalfw-fetch-reminders'. CAL-IDS is the
full set of calendar IDs of TYPE being displayed together; they're all
fetched in a single call and the result cached briefly (see
`maccalfw--items-cache-ttl') and split by each item's X-EMACS-CALID,
rather than each calendar querying separately.

BEGIN/END bound the fetch for events; pass nil for reminders, which
aren't date-bounded server-side to begin with -- passing nil (rather
than the visible range) keeps the cache key, and hence the fetched
set, stable while paging between dates instead of missing a cache hit
on every navigation. `maccalfw--get-calendar-items' applies the actual
date-range filter itself, from the full set this returns."
  (let ((key (list type cal-ids begin end)))
    (unless (and maccalfw--items-cache
                (equal key (nth 0 maccalfw--items-cache))
                (< (- (float-time) (nth 1 maccalfw--items-cache))
                   maccalfw--items-cache-ttl))
      (let ((by-cal (make-hash-table :test #'equal))
            (items (if (eq type 'event)
                      (maccalfw-fetch-events
                       cal-ids
                       (maccalfw--encode-date begin)
                       (maccalfw--encode-date end t))
                    (maccalfw-fetch-reminders cal-ids))))
        (dolist (item items)
          (push item (gethash (ical-form-event-get item 'X-EMACS-CALID)
                              by-cal)))
        (maphash (lambda (k v) (puthash k (nreverse v) by-cal)) by-cal)
        (setq maccalfw--items-cache (list key (float-time) by-cal))))
    (nth 2 maccalfw--items-cache)))

(defun maccalfw--reminder-date (reminder)
  "Return the (month day year) date REMINDER is shown on.
A reminder due today or with no DUE date at all is shown today; one
due on another day is shown there instead, regardless of its
time-of-day."
  (maccalfw--decode-date
   (decode-time
    (or (ical-form-event-get reminder 'DUE) (current-time)))))

(defun maccalfw--get-calendar-items (type cal-ids cal-id begin end)
  "Return calfw items of TYPE for CAL-ID, in range BEGIN to END.
TYPE is `event' or `reminder' (see `maccalfw-get-calendars'); it picks
the converter (`maccalfw--convert-event'/`maccalfw--convert-reminder')
and, for a reminder, whether `maccalfw--reminder-date' -- rather than
DTSTART/DTEND -- decides membership in the range. BEGIN and END are
dates with the format (month day year). CAL-IDS is the full set of
calendar IDs of TYPE sharing the underlying fetch; see
`maccalfw--fetch-items-cached'."
  (let* ((all-items (gethash cal-id
                             (maccalfw--fetch-items-cached
                              type cal-ids
                              (and (eq type 'event) begin)
                              (and (eq type 'event) end))))
         (items
          (if (eq type 'event)
              all-items
            (let ((begin-time (maccalfw--encode-date begin))
                  (end-time (maccalfw--encode-date end t)))
              (cl-remove-if-not
               (lambda (reminder)
                 (let ((date-time (maccalfw--encode-date
                                    (maccalfw--reminder-date reminder))))
                   (and (not (time-less-p date-time begin-time))
                        (not (time-less-p end-time date-time)))))
               all-items))))
         (converter (if (eq type 'event)
                        #'maccalfw--convert-event
                      #'maccalfw--convert-reminder)))
    (cl-loop for event in (maccalfw--convert-to-calfw items converter)
             if (and (listp event)
                     (equal 'periods (car event)))
             collect
             (cons
              'periods
              (cl-loop for evt in (cadr event)
                       collect evt))
             else
             collect event)))

(defun maccalfw--create-source (all-cal-ids name cal-id color type)
  "Create a calfw-source out of a calendar.
CAL-ID is the ID of the calendar and its own TYPE (`event' or
`reminder') can be obtained with `maccalfw-get-calendars'. The
calendar's NAME and COLOR are set accordingly. ALL-CAL-IDS is the full
set of calendar IDs of TYPE being displayed together; see
`maccalfw--fetch-items-cached'."
  (make-calfw-source
   :name name
   :color color
   :update #'ignore
   :data (lambda (begin end)
           (maccalfw--get-calendar-items type all-cal-ids cal-id begin end))))

(defun maccalfw-get-calendars-by-name (names)
  "Return the calendar IDs with NAMES."
  (cl-remove-if-not
   (lambda (x) (member (plist-get x :title) names))
   (maccalfw-get-calendars)))

(defun maccalfw-open (&optional calendars)
  "Open a calfw calendar with CALENDARS from Apple's Calendar/Reminders.
This command displays any CALENDARS obtained using
`maccalfw-get-calendars' or all event calendars if it is \\='all.
CALENDARS may mix event and reminder calendars (e.g. from
`(maccalfw-get-calendars \\='all)'); each is dispatched to an events or
a reminders source by its own :type."
  (interactive (list 'all))
  (maccalfw--cli-ensure)
  (when (eq calendars 'all)
    (setq calendars (maccalfw-get-calendars)))
  (calfw-open-calendar-buffer
   :view (if (featurep 'calfw-blocks)
             'block-week
           'week)
   :contents-sources
   (cl-loop for type in '(event reminder)
            for cals = (cl-remove-if-not
                        (lambda (x) (equal (plist-get x :type) (symbol-name type)))
                        calendars)
            for ids = (mapcar (lambda (x) (plist-get x :id)) cals)
            append
            (mapcar
             (lambda (x)
               (maccalfw--create-source ids
                                        (plist-get x :title)
                                        (plist-get x :id)
                                        (plist-get x :color)
                                        type))
             cals))
   :sorter (or (and (fboundp 'calfw-blocks-default-sorter)
                    #'calfw-blocks-default-sorter)
             #'string-lessp)))

(defun maccalfw-delete-event (ev)
  "Delete event EV."
  (interactive
   (list (or (when-let* ((cfw-ev (get-text-property (point) 'cfw:event)))
               (calfw-event-data cfw-ev))
             (error "No event at location"))))
  (or (prog1
            (maccalfw-remove-event
             (ical-form-event-get ev 'UID)
             (ical-form-event-get ev 'DTSTART)
             (if (ical-form-event-get ev 'RRULE)
               (maccalfw-modify-future-events-p)
             nil))
        (message "Event deleted")
        (calfw-refresh-calendar-buffer nil))
      (error "Deleting event failed")))

(defun maccalfw-modify-future-events-p (&optional prompt)
  "Return non-nil if modification should affect all future events.
Check the value of the variable
`maccalfw-modify-future-events-p', and potentially prompt
the user, displaying the message PROMPT."
  (if (eq maccalfw-modify-future-events-p 'ask)
      (let ((response
             (cadr
              (read-multiple-choice
               (format (or prompt "Which events to modify?")
                       (buffer-name))
               '((?f "future" "Modification applies to all future events.")
                 (?c "current" "Modification applies only to current event."))
               nil nil (and (not use-short-answers)
                            (and (fboundp #'use-dialog-box-p)
                                 (not (use-dialog-box-p))))))))
        (equal response "future"))
    maccalfw-modify-future-events-p))

(defun maccalfw-modify-event (old-data new-data)
  "Update or create an event.
Only UID, DTSTART and RRULE are used from OLD-DATA. NEW-DATA can
contain only changed fields. If UID is missing or nil, a new
event is created instead."
  ;; if old event has a recurrence, check with use if all future events
  ;; should be editied or just the current one
  (let ((future
         (and (or (ical-form-event-get old-data 'RRULE)
                  (ical-form-event-get new-data 'RRULE))
              ;; If changing recurrence rule, then we should modify all
              ;; future events. Otherwise, we should ask the user
              (or (ical-form-event-get new-data 'RRULE)
                  (maccalfw-modify-future-events-p)))))
    (maccalfw-update-event
     (ical-form-event-get old-data 'UID)
     new-data
     (ical-form-event-get old-data 'DTSTART)
     future)))

(defun maccalfw-new-event (event-data)
  "Create an events-details buffer for a new event.
EVENT-DATA contains the initial event information."
  (interactive
   (list
    (let (start end all-day ev)
      (when (derived-mode-p 'calfw-calendar-mode)
        (if-let* ((event (and current-prefix-arg
                             (get-text-property (point) 'cfw:event)))
                 (old-event-data (calfw-event-data event)))
            (setq ev
            (cl-loop for item in old-event-data
                           if (member (car item) '( ; Keep those fields
                                             DTSTART DTEND
                                             SUMMARY
                                             LOCATION
                                             X-AVAILABILITY
                                             URL
                                             DESCRIPTION))
                           collect item))
          (when (and (fboundp 'calfw-blocks-region-to-time)
                     (eq (calfw-component-view (calfw-cp-get-component))
                         'block-week))
            (cl-destructuring-bind (e-start e-end e-all-day)
                (calfw-blocks-region-to-time)
              (setq start e-start
                    end e-end
                    all-day e-all-day)))))
      (or ev
          (let ((start (or start (current-time))))
            (ical-form-create-event
             start (or end (time-add start 3600)) all-day
             (car-safe (cl-find-if
                        (lambda (x) (plist-get (cdr x) :default))
                        (maccalfw-timezones)))))))))
  (ical-form-open event-data
                  (maccalfw-get-calendars)
                  (maccalfw-timezones)
                  #'maccalfw-modify-event))

(defun maccalfw-goto-event-details (event)
  "Open event details for the calfw EVENT."
  (interactive
   (list (or (get-text-property (point) 'cfw:event)
             (error "No event at location"))))
  (ical-form-open (calfw-event-data event)
                  (maccalfw-get-calendars)
                  (maccalfw-timezones)
                  #'maccalfw-modify-event))


(defun maccalfw-mouse-down-disable-dbl-click (event)
  "Call `mouse-drag-region' but disable double clicking.
Assigning this commend to [down-mouse-1] ensures the commands
assigned to [double-mouse-1] is called.
EVENT defaults to the event data."
  (interactive "e")
  (let (mouse-selection-click-count)
    (if (and (consp event)
             (nthcdr 2 event))
        (setcar (nthcdr 2 event) 1))
    (mouse-drag-region event)))

(define-obsolete-function-alias
  'maccalfw-event-new-event #'maccalfw-new-event "0.2")
(define-obsolete-function-alias
  'maccalfw-event-goto-details #'maccalfw-goto-event-details "0.2")
(define-obsolete-function-alias
  'maccalfw-event-delete-event #'maccalfw-delete-event  "0.2")
(define-obsolete-function-alias
  'maccalfw-event-mouse-down #'maccalfw-mouse-down-disable-dbl-click "0.2")
(define-obsolete-function-alias
  'maccalfw-event-open #'ical-form-open "0.2")
(define-obsolete-variable-alias
  'maccalfw-event-save-hook 'ical-form-event-updated-hook  "0.2")

(provide 'maccalfw)
;;; maccalfw.el ends here
