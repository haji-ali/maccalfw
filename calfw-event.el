;;; calfw-event.el --- An event viewer and editor  -*- lexical-binding: t; -*-

;; Copyright (C) 2023, Al Haji-Ali

;; Author: ml729
;; Maintainer: Al Haji-Ali <abdo.haji.ali at gmail.com>
;; Created: Author
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
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
;;
;;
;;; Commentary:
;;
;; This package provides several views for calfw that show events as blocks.
;;
;;; Code:


(defvar calfw-event-properties
  `((:all-day-p "all-day" ,(lambda (x) (and x "yes")))
    (:detached-p "detached" ,(lambda (x) (and x "yes")))
    ;; (:occurrence-date "occurance-date"
    ;;                   ,(lambda (x) (propertize (format-time-string "%c" x)
    ;;                                            'read-only t)))
    (:status "status" ,(lambda (x) (symbol-name x)))
    (:availability "availability" ,(lambda (x) (symbol-name x)))
    (:organzier "organzier" ,#'identity)
    (:url "URL" ,#'identity)
    (:time-zone "timezone" ,#'identity)))

(defvar-keymap calfw-event-mode-map
  :doc "Keymap for `calfw-event'."
  :full t
  :parent widget-keymap
  "C-c C-e" #'calfw-event-edit
  "C-c C-k" #'calfw-event-kill
  "C-c C-w" #'calfw-event-save
  "C-c C-s" #'calfw-event-date-field-pick)

(define-derived-mode calfw-event-mode fundamental-mode "Calendar Event"
  "Major mode for editing calendar events."
  :lighter " Calfw event"
  (use-local-map calfw-event-mode-map))

(defvar-local calfw-event--data nil)
(defvar-local calfw-event--widgets nil)

(defvar calfw-event--timezones (maccalfw-timezones))
(defvar calfw-event--default-timezone
  (cl-find-if
   (lambda (x) (plist-get (cdr x) :default))
   calfw-event--timezones))

(defun calfw-event-kill ()
  (interactive)
  ;; TODO: Check modified buffer
  (quit-window t))

(defun calfw-event-edit ()
  (interactive)

  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<calfw-event-mode-map>Event details. \
Save `\\[calfw-event-save]', \
abort `\\[calfw-event-kill]'.")))

(defun calfw-event-save ()
  (interactive)
  )

(defface calfw-event-notes-field
  '((t
     :inherit widget-field
     :box (:line-width (0 . 0))
     ))
  "Face used for editable fields."
  :version "28.1")

(defface calfw-event-field-names
  '((t
     :weight bold))
  "Face used for field names."
  :version "28.1")

(defun calfw-event--show-hide-widget (widget visible)
  (let* ((field-begin (plist-get (cdr widget) :from))
         (field-end (plist-get (cdr widget) :to))
         (inhibit-read-only t)
	 (inhibit-modification-hooks t))
    (add-text-properties field-begin field-end `(invisible ,visible))))

(defvar org-time-was-given)
(defvar org-end-time-was-given)

(defun calfw-event-date-field-pick ()
  (interactive)
  (let* ((start-time-wid (plist-get calfw-event--widgets :start-time))
         (start-date-wid (plist-get calfw-event--widgets :start-date))
         (end-time-wid (plist-get calfw-event--widgets :end-time))
         (end-date-wid (plist-get calfw-event--widgets :end-date))
         (timezone-wid (plist-get calfw-event--widgets :timezone))
         (all-day-wid (plist-get calfw-event--widgets :all-day))
         (start-time
          (calfw-event--parse-datetime
           (widget-value start-time-wid)
           (widget-value start-date-wid)))
         (end-time
          (calfw-event--parse-datetime
           (widget-value end-time-wid)
           (widget-value end-date-wid)))
         org-time-was-given
         org-end-time-was-given
         new-time
         new-end-time)
    (setq new-time (org-read-date
                    (not (widget-value all-day-wid))
                    t
                    nil
                    "Event"
                    start-time
                    nil nil))
    (widget-value-set start-date-wid
                      (format-time-string "%F" new-time))

    (when org-time-was-given
      ;; Update time as well
      (widget-value-set start-time-wid (calfw-event--format-time new-time))
      (when org-end-time-was-given
        ;; Update end time as well.. TODO: Check format
        (widget-value-set start-time-wid
                          org-end-time-was-given)))))

(defun calfw-event--format-time (time &optional timezone)
  ;; Assume time is in the default timezone
  (let ((tz (and timezone (alist-get timezone calfw-event--timezones
                                     nil nil #'equal))))
    (format-time-string
     "%R"
     (if tz
         (time-add
          time
          (-
           (plist-get tz :offset)
           (plist-get (cdr calfw-event--default-timezone) :offset)))
       time))))

(defun calfw-event--parse-datetime (date-str time-str &optional timezone)
  "Parse time and return the time in the default-time zone."
  (let ((tz (and timezone (alist-get timezone calfw-event--timezones
                                     nil nil #'equal)))
        (time (encode-time
               (parse-time-string (format "%s %s" date-str time-str)))))
    (if tz
        (time-add
         time
         (-
          (plist-get (cdr calfw-event--default-timezone) :offset)
          (plist-get (alist-get timezone calfw-event--timezones
                                nil nil #'equal)
                     :offset)))
      time)))

(defun calfw-event--timezone-widget-notify (widget &rest ignore)
  "Action for timezone action.
Assumes that WIDGET has an additional attributes `:old-value'
which is the old value of the timezone (will be updated in this
function).
"
  (let* ((old-tz (widget-get widget :old-value))
         (tz (widget-value widget)))
    (save-excursion
      (cl-loop for (date-wid . time-wid) in '((:start-date :start-time)
                                              (:end-date :end-time))
               for time-widget = (plist-get calfw-event--widgets time-wid)
               for date-widget = (plist-get calfw-event--widgets date-wid)
               do
               (widget-value-set
                time-widget
                (calfw-event--format-time
                 (calfw-event--parse-datetime
                  (widget-value date-widget)
                  (widget-value time-widget)
                  old-tz)
                 tz)))
      (widget-put widget :old-value tz))))

(defun calfw-event--all-day-notify (checkbox &rest _)
  (let ((checked (widget-value checkbox)))
    (calfw-event--show-hide-widget
     (plist-get calfw-event--widgets
                :end-date)
     (not checked))
    (mapc
     (lambda (x)
       (when-let (field (plist-get calfw-event--widgets x))
         (calfw-event--show-hide-widget field checked)))
     (list :start-time :end-time :timezone))))

(defun calfw-event--create-form (event)
  (let ((timezone (plist-get event :timezone)))
    (widget-insert "\n\n")

    (setq
     calfw-event--widgets
     (plist-put
      calfw-event--widgets
      :title
      (widget-create 'editable-field
                     :size 13
                     :keymap widget-field-keymap
                     :value-face 'info-title-1
                     :format " %v \n" ; Text after the field!
                     (plist-get event :title))))

    (let ((options (mapcar
                    (lambda (x)
                      `(item :tag ,(plist-get x :title)
                             :value ,(plist-get x :id)))
                    (maccalfw-get-calendars))))
      (setq
       calfw-event--widgets
       (plist-put
        calfw-event--widgets
        :calendar-id
        (apply
         'widget-create 'menu-choice
         :tag "Calendar"
         :format " %[%t%]: %v\n\n"
         :value (plist-get event :calendar-id)
         options))))

    (setq
     calfw-event--widgets
     (plist-put
      calfw-event--widgets
      :start-date
      (widget-create 'editable-field
                     :keymap widget-field-keymap
                     :format " %v "
                     :size 10
                     (format-time-string "%F"
                                         (plist-get event :start)))))

    (setq
     calfw-event--widgets (plist-put
                           calfw-event--widgets
                           :end-date
                           (widget-create 'editable-field
                                          :keymap widget-field-keymap
                                          :format "  --    %v   "
                                          :size 10
                                          (format-time-string "%F"
                                                              (plist-get event :end)))))
    (setq
     calfw-event--widgets
     (plist-put
      calfw-event--widgets
      :start-time
      (widget-create 'editable-field
                     :keymap widget-field-keymap
                     :format " %v -- "
                     :size 6
                     (calfw-event--format-time
                      (plist-get event :start)
                      timezone))))

    (setq
     calfw-event--widgets
     (plist-put
      calfw-event--widgets
      :end-time
      (widget-create 'editable-field
                     :keymap widget-field-keymap
                     :format " %v   "
                     :size 6
                     (calfw-event--format-time
                      (plist-get event :end)
                      timezone))))

    (setq
     calfw-event--widgets
     (plist-put
      calfw-event--widgets
      :all-day
      (widget-create 'checkbox
                     :format " %[%v%] All day\n\n"
                     :notify #'calfw-event--all-day-notify
                     (plist-get event :all-day-p))))
    (when timezone
      (let* ((options (mapcar
                       (lambda (x)
                         `(item :tag ,(format "%s (%s)"
                                              (car x)
                                              (plist-get (cdr x) :abbrev))
                                :value ,(car x)
                                :details x))
                       calfw-event--timezones)))
        (setq
         calfw-event--widgets
         (plist-put
          calfw-event--widgets
          :timezone
          (apply
           'widget-create 'menu-choice
           :notify #'calfw-event--timezone-widget-notify
           :tag "Timezone"
           :format " %[%t%]: %v\n\n"
           :value timezone
           :old-value timezone
           options)))))

    (calfw-event--all-day-notify
     (plist-get calfw-event--widgets :all-day))

    (setq
     calfw-event--widgets
     (plist-put
      calfw-event--widgets
      :url
      (widget-create 'editable-field
                     :keymap widget-field-keymap
                     :format
                     (concat
                      (propertize " Location: " 'face 'calfw-event-field-names)
                      "%v\n\n")
                     (or (plist-get event :location) ""))))


    (when-let (stat (plist-get event :status))
      (widget-insert
       (propertize " Status: " 'face 'calfw-event-field-names)
       (symbol-name stat) "\n\n"))

    (when-let (org (plist-get event :organizer))
      (widget-insert
       (propertize " Organizer: " 'face 'calfw-event-field-names)
       org "\n\n"))

    (setq
     calfw-event--widgets
     (plist-put
      calfw-event--widgets
      :availability
      (widget-create 'radio-button-choice
                     :entry-format "  %b %v "
                     :inline t
                     :format " %v\n\n"
                     :value (when-let (avail (plist-get event :availability))
                              (symbol-name avail))
                     '(item :format "%[Tentative%] " :value tentative)
                     '(item :format "%[Free%] " :value free)
                     '(item :format "%[Busy%] " :value busy)
                     '(item :format "%[Unavailable%] " :value unavailable))))

    (setq
     calfw-event--widgets
     (plist-put
      calfw-event--widgets
      :url
      (widget-create 'editable-field
                     :keymap widget-field-keymap
                     :format
                     (concat
                      (propertize " URL: " 'face 'calfw-event-field-names)
                      "%v\n\n")
                     (or (plist-get event :url) ""))))

    (setq
     calfw-event--widgets
     (plist-put
      calfw-event--widgets
      :notes
      (widget-create 'text
                     :format "%v" ; Text after the field!
                     :value-face 'calfw-event-notes-field
                     (or (plist-get event :notes) ""))))
    ;;(use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))
    calfw-event--widgets))

(defun calfw-event-open (event)
  (pop-to-buffer (generate-new-buffer "*calender event*"))
  (setq-local calfw-event--data event)
  (calfw-event-mode)
  (calfw-event--create-form event)

  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<calfw-event-mode-map>Event details. \
Edit `\\[calfw-event-edit]', \
abort `\\[calfw-event-kill]'."))
  (set-buffer-modified-p nil))

(defun calfw-event-goto-details ()
  (interactive)
  (if-let ((event (get-text-property (point) 'cfw:event)))
      (calfw-event-open
       (cfw:event-data event))
    (error "No event at location")))

(defun calfw-event-mouse-down (start-event)
  "Call `mouse-drag-region' but disable double clicking."
  (interactive "e")
  (let (mouse-selection-click-count)
    (if (and (consp start-event)
             (nthcdr 2 start-event))
        (setcar (nthcdr 2 start-event) 1))
    (mouse-drag-region start-event)))

(defun calfw-event-activate-widgets (active)
  ""
  (save-excursion
    (goto-char (point-min))
    ;; Surely there's a better way to find all the "top level" widgets
    ;; in a buffer, but I couldn't find it.
    (while (not (eobp))
      (when-let* ((widget (widget-at (point)))
                  ;;(parent (widget-get widget :parent))
                  ;;(active (widget-get parent :active))
                  )
        (unless (eq (widget-get widget :inactive)
                    (not active))
          (if active
              (widget-apply widget :activate)
            (widget-apply widget :deactivate))))
      (forward-char 1))))

(let ((keymap (make-sparse-keymap)))
  (define-key keymap [13] #'calfw-event-goto-details)
  (define-key keymap [double-mouse-1] #'calfw-event-goto-details)
  (define-key keymap [down-mouse-1] #'calfw-event-mouse-down)
  (setq calfw-blocks-event-keymap keymap))

(provide 'calfw-event)
;;; calfw-event.el ends here
