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
  "C-c C-w" #'calfw-event-save)

(defvar-keymap calfw-event-date-field-map
  :doc "Keymap for `calfw-event'."
  :full t
  :parent widget-field-keymap
  "C-c C-c" #'calfw-event-date-field-pick)

(define-derived-mode calfw-event-mode fundamental-mode "Calendar Event"
  "Major mode for editing calendar events."
  :lighter " Calfw event"
  (use-local-map calfw-event-mode-map))

(defvar-local calfw-event--data nil)
(defvar-local calfw-event--parent-buffer nil)

(defvar calfw-event--timezones (maccalfw-timezones))
(defvar calfw-event--default-timezone
  (cl-find-if
   (lambda (x) (plist-get (cdr x) :default))
   calfw-event--timezones))

(defun calfw-event-kill ()
  (interactive)
  (unless (and (buffer-modified-p)

               )
    (quit-window t)))

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

  ;; Code to get content
  ;; (progn
  ;;   (org-narrow-to-subtree)
  ;;   (buffer-substring (progn (forward-line 1) (point))
  ;;                     (progn (org-end-of-subtree t) (point))))
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

(defun calfw-event-date-field-pick (widget)
  (when (fboundp 'org-read-date)
    (widget-value-set widget (org-read-date t))))

(defun calfw-event--format-time (time timezone)
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

(defun calfw-event--parse-datetime (date-str
                                    time-str
                                    timezone)
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
Assumes that WIDGET has two additional attributes:
 `:datetime-widgets' is a list of cons of text fields each
representing date and time.
`:old-value' is the old value of the timezone (will be updated in
this function).
"
  (let* ((old-tz (widget-get widget :old-value))
         (wids (widget-get widget :datetime-widgets))
         (tz (widget-value widget)))
    (save-excursion
      (cl-loop for wid in wids
               do
               (widget-value-set
                (cdr wid)
                (calfw-event--format-time
                 (calfw-event--parse-datetime
                  (widget-value (car wid))
                  (widget-value (cdr wid))
                  old-tz)
                 tz)))
      (widget-put widget :old-value tz))))

(defun calfw-event--create-form (event)
  (interactive (list calfw-event--data))
  (let (start-date-wid
        end-date-wid
        start-time-wid
        end-time-wid
        (timezone (plist-get event :timezone))
        (all-day (plist-get event :all-day-p))
        all-day-toggle-widgets)
    (widget-insert "\n\n")

    (widget-create 'editable-field
                   :size 13
                   :keymap widget-field-keymap
                   :value-face 'info-title-1
                   :format " %v \n" ; Text after the field!
                   (plist-get event :title))

    (let ((options (mapcar
                    (lambda (x)
                      `(item :tag ,(plist-get x :title)
                             :value ,(plist-get x :id)))
                    (maccalfw-get-calendars))))
      (apply
       'widget-create 'menu-choice
       :tag "Calendar"
       :format " %[%t%]: %v\n\n"
       :value (plist-get event :calendar-id)
       options))

    (setq start-date-wid
          (widget-create 'editable-field
                         :keymap widget-field-keymap
                         :format " %v "
                         :size 10
                         ;; :keymap  'calfw-event-date-keymap
                         (format-time-string "%F"
                                             (plist-get event :start))))

    (setq end-date-wid
          (widget-create 'editable-field
                         :keymap widget-field-keymap
                         :format "  --    %v   "
                         :size 10
                         ;; :keymap  'calfw-event-date-keymap
                         (format-time-string "%F"
                                             (plist-get event :end))))
    (setq start-time-wid
          (widget-create 'editable-field
                         :keymap widget-field-keymap
                         :format " %v -- "
                         :size 6
                         (calfw-event--format-time
                          (plist-get event :start)
                          timezone)))

    (setq end-time-wid
          (widget-create 'editable-field
                         :keymap widget-field-keymap
                         :format " %v   "
                         :size 6
                         (calfw-event--format-time
                          (plist-get event :end)
                          timezone)))

    (let* ((fn-action (lambda (checkbox &rest _)
                        (let ((checked (widget-value checkbox))
                              (toggle (widget-get checkbox :toggle-widgets)))
                          (mapc
                           (lambda (x)
                             (calfw-event--show-hide-widget x (not checked)))
                           (car toggle))
                          (mapc
                           (lambda (x)
                             (calfw-event--show-hide-widget x checked))
                           (cdr toggle))))))
      (setq all-day-toggle-widgets
            (cons (list end-date-wid)
                  (list start-time-wid end-time-wid)))
      (funcall fn-action
               (widget-create 'checkbox
                              :format " %[%v%] All day\n\n"
                              :toggle-widgets all-day-toggle-widgets
                              :notify fn-action
                              (plist-get event :all-day-p))))

    (when timezone
      (let* ((options (mapcar
                       (lambda (x)
                         `(item :tag ,(format "%s (%s)"
                                              (car x)
                                              (plist-get (cdr x) :abbrev))
                                :value ,(car x)
                                :details x))
                       calfw-event--timezones))
             (timezone-wid (apply
                            'widget-create 'menu-choice
                            :datetime-widgets
                            (list (cons start-date-wid
                                        start-time-wid)
                                  (cons end-date-wid
                                        end-time-wid))
                            :notify #'calfw-event--timezone-widget-notify
                            :tag "Timezone"
                            :format " %[%t%]: %v\n\n"
                            :value timezone
                            :old-value timezone
                            options)))
        (setcdr
         all-day-toggle-widgets
         (cons timezone-wid
               (cdr all-day-toggle-widgets)))))


    (when-let (stat (plist-get event :status))
      (widget-insert
       (propertize " Status: " 'face 'calfw-event-field-names)
       (symbol-name stat) "\n\n"))

    (when-let (org (plist-get event :organizer))
      (widget-insert
       (propertize " Organizer: " 'face 'calfw-event-field-names)
       org "\n\n"))

    (widget-create 'radio-button-choice
                   ;;:tag "Availability"
                   :entry-format "  %b %v "
                   :inline t
                   :format " %v\n\n"
                   :value (when-let (avail (plist-get event :availability))
                            (symbol-name avail))
                   ;; '(item :tag "This option" :value "This" )
                   ;; '(choice-item "That option")
                   ;; '(editable-field :menu-tag "No option" "Thus option")
                   '(item :format "%[Tentative%] " :value tentative)
                   '(item :format "%[Free%] " :value free)
                   '(item :format "%[Busy%] " :value busy)
                   '(item :format "%[Unavailable%] " :value unavailable))

    (widget-create 'editable-field
                   :keymap widget-field-keymap
                   :format
                   (concat
                    (propertize " URL: " 'face 'calfw-event-field-names)
                    "%v\n\n")
                   (or (plist-get event :url) ""))

    (widget-create 'text
                   :format "%v" ; Text after the field!
                   :value-face 'calfw-event-notes-field
                   (or (plist-get event :notes) ""))
    ;;(use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))))

(defun calfw-event-open (event)
  (setq-local calfw-event--parent-buffer (current-buffer))

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
