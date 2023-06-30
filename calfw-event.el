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

(define-derived-mode calfw-event-mode fundamental-mode "Calendar Event"
  "Major mode for editing calendar events."
  :lighter " Calfw"

  (define-key calfw-event-mode-map "\C-c\C-e" #'calfw-event-edit)
  (define-key calfw-event-mode-map "\C-c\C-k" #'calfw-event-kill)
  (define-key calfw-event-mode-map "\C-c\C-w" #'calfw-event-save))

(defvar-local calfw-event--data nil)

(defun calfw-event-kill ()
  (interactive)
  (kill-current-buffer))

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

(defun calfw-event--show-hide-widget (widget visible)
  (let* ((field-begin (plist-get (cdr widget) :from))
         (field-end (plist-get (cdr widget) :to))
         (inhibit-read-only t)
	 (inhibit-modification-hooks t))
    (add-text-properties field-begin field-end `(invisible ,visible))))

(defun calfw-event-open (source event)
  (pop-to-buffer (generate-new-buffer "*calender event*"))
  (calfw-event-mode)
  (widget-insert "\n\n")
  (widget-create 'editable-field
                 :size 13
                 :value-face 'info-title-1
                 :format " %v " ; Text after the field!
                 (plist-get event :title))
  (insert "\n ")
  (let ((options (mapcar
                  (lambda (x)
                    `(item :tag ,(plist-get x :title)
                           :value ,(plist-get x :id)))
                  (maccalfw-get-calendars))))
    (apply
     'widget-create 'menu-choice
     :tag "Calendar"
     :value (plist-get event :calendar-id)
     options))
  (widget-insert " \n\n")

  (widget-create 'editable-field
                 :format " %v "
                 :size 10
                 (format-time-string "%F" (plist-get event :start)))

  (let* ((all-day (plist-get event :all-day-p))
         (all-day-wid (list
                       (widget-create 'editable-field
                                      :format "  --    %v   "
                                      :size 10
                                      :inactive (not all-day)
                                      (format-time-string "%F" (plist-get event :end)))))
         (interval-wid (list (widget-create 'editable-field
                                            :format " %v -- "
                                            :size 6
                                            :inactive all-day
                                            (format-time-string "%R" (plist-get event :start)))
                             (widget-create 'editable-field
                                            :format " %v   "
                                            :size 6
                                            :inactive all-day
                                            (format-time-string "%R" (plist-get event :end)))))
         (fn-action (lambda (checkbox &rest _)
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
    (funcall fn-action
             (widget-create 'checkbox
                            :toggle-widgets (cons all-day-wid interval-wid)
                            :notify fn-action
                            (plist-get event :all-day-p)
                            ;; :notify
                            ;; TODO: Need to remove time
                            ))
    (widget-insert " All day"))

  (widget-insert "\n\n ")



  (when-let (timezone (plist-get event :timezone))
    (let ((options (mapcar
                    (lambda (x)
                      `(item :tag ,(format "%s (%s)"
                                           (plist-get x :id)
                                           (plist-get x :abbrev))
                             :value ,(plist-get x :id)))
                    (maccalfw-timezones))))
      (apply
       'widget-create 'menu-choice
       :tag "Timezone"
       :value timezone
       options))
    (widget-insert "\n\n"))


  (widget-create 'radio-button-choice
                 ;;:tag "Availability"
                 :entry-format "  %b %v"
                 :inline t
                 :value (when-let (avail (plist-get event :availability))
                          (symbol-name avail))
                 ;; '(item :tag "This option" :value "This" )
                 ;; '(choice-item "That option")
                 ;; '(editable-field :menu-tag "No option" "Thus option")
                 '(item :format "%[Tentative%] " :value tentative)
                 '(item :format "%[Free%] " :value free)
                 '(item :format "%[Busy%] " :value busy)
                 '(item :format "%[Unavailable%] " :value unavailable))

  (widget-insert "\n\n")

  (when-let (stat (plist-get event :status))
    (widget-insert " Status: " (symbol-name stat))
    (widget-insert "\n\n"))

  (when-let (org (plist-get event :organizer))
    (widget-insert " Organizer: " org)
    (widget-insert "\n\n"))

  (widget-create 'editable-field
                 :format " URL: %v"
                 (or (plist-get event :url) ""))
  (widget-create 'text
                 :format "\n\n%v" ; Text after the field!
                 :value-face 'calfw-event-notes-field
                 (or (plist-get event :notes) ""))
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min))
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
       (get-text-property (point) 'cfw:source)
       event)
    (error "No event at location")))

(defun calfw-event-mouse-down (start-event)
  "Call `mouse-drag-region' but disable double clicking."
  (interactive "e")
  (let (mouse-selection-click-count)
    (if (and (consp start-event)
             (nthcdr 2 start-event))
        (setcar (nthcdr 2 start-event) 1))
    (mouse-drag-region start-event)))

(let ((keymap (make-sparse-keymap)))
  (define-key keymap [13] #'calfw-event-goto-details)
  (define-key keymap [double-mouse-1] #'calfw-event-goto-details)
  (define-key keymap [down-mouse-1] #'calfw-event-mouse-down)
  (setq calfw-blocks-event-keymap keymap))

(provide 'calfw-event)
;;; calfw-event.el ends here
