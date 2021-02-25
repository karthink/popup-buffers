;;; popup-buffers.el --- Designate buffers as popups to summon or dismiss easily. -*- lexical-binding: t -*-

;; Copyright (C) 2021  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthik.chikmagalur@gmail.com>
;; URL: https://github.com/karthink/popup-buffers
;; Version: 0.20
;; Package-Requires: ((emacs "25.3"))

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A hacked together solution to handle annoying popups in Emacs. Only works
;; well in conjunction with some system to handle window creation and
;; placement, like shackle.el. This plugin summons windows defined by
;; the user as "popups" by simply calling `display-buffer'.
;;
;; COMMANDS:
;;
;; popup-buffers-toggle-latest : Toggle latest popup
;; popup-buffers-cycle         : Cycle through all popups, or close all open popups
;; popup-buffers-open-latest   : Open latest popup
;; popup-buffers-close-latest  : Close latest popup
;;
;; CUSTOMIZATION:
;;
;; `popup-buffers-reference-buffers': A list of major modes or regexps whose
;; corresponding buffer major-modes or regexps (respectively) should be treated
;; as popups.
;;
;; `popup-buffers-mode-line': String or sexp to show in the mode-line of
;; popup-buffers. Setting this to NIL removes the mode-line entirely from
;; popup-buffers.
;;
;; TODO: Add popup list maintenance to `make-frame-finish-functions',
;; (add-hook 'after-make-frame-functions 'popup-buffers-update-popups)
;;
;; by Karthik Chikmagalur <karthik.chikmagalur@gmail.com>

;;; Code:

(require 'cl-lib)

(defgroup popup-buffers nil
  "Provide functions for easy access to popup windows"
  :group 'convenience)

(defcustom popup-buffers-reference-buffers '("\\*Messages\\*$")
  "List of buffers to treat as popups.
Each entry in the list can be a regexp (string) to match buffer
names against, or a `major-mode' (symbol) to match buffer
major-modes against.

Example:

'(\"\\*Messages\\*\"
  \"Output\\*$\"
  help-mode
  compilation-mode)

Will match against the Messages buffer, any buffer ending in Output*, and all help and compilation buffers."
  :type '(restricted-sexp :match-alternatives (stringp symbolp))
  :group 'popup-buffers)

(defcustom popup-buffers-mode-line '(:eval (propertize " POP" 'face 'mode-line-emphasis))
  "String or sexp to show in the mode-line of popup-buffers. Can
be a quoted list or function. Setting this to NIL removes the
mode-line entirely from popup-buffers."
  :group 'popup-buffers
  :type '(choice (string :tag "Literal text")
                 (sexp :tag "General `mode-line-format' entry")))

(defcustom popup-buffers-mode-line-position 0
  "Position in mode-line to place `popup-buffers-mode-line'."
  :type 'integer)

(defcustom popup-buffers-display-control t
  "Whether popup-buffers should control the placement of popup windows. 
Choices are:
'user: The default. Only control placement of explicitly marked popups.
 nil : Do not control popup placement.
 t   : Control placement of all popups."
  :group 'popup-buffers
  :type '(choice 'user t nil))

(defcustom popup-buffers-display-function #'popup-buffers-select-popup-at-bottom
  "Function to use to display popup-buffers. Note that this is
only invoked when `popup-buffers-display-control' is
non-nil.

This function accepts two arguments, a buffer and an action alist
and displays the buffer. See (info \"(elisp) Buffer Display
Action Alists\") for details on the alist."
  :group 'popup-buffers
  :type 'function)

(defvar popup-buffers-reference-names nil
  "List of buffer names whose windows are treated as popups.")

(defvar popup-buffers-reference-modes nil
 "List of buffer major-modes whose buffers are treated as popups.")

(defvar popup-buffers-open-popup-alist nil
  "Alist of currently live (window . buffer)s that are treated as popups.")

(defvar popup-buffers-buried-popup-alist nil
  "Alist of currently buried (window . buffer)s that are treated as popups.")

(defvar-local popup-buffers-popup-status nil
  "Identifies a buffer as a popup by its buffer-local value.
  Valid values are 'popup, 'raised, 'user-popup or nil.

'popup     : This is a popup buffer specified in `popup-buffers-reference-buffers'.
'raised    : This is a POPUP buffer raised to regular status by the user.
'user-popup: This is a regular buffer lowered to popup status by the user.")

;;;###autoload
(defun popup-buffers-select-popup-at-bottom (buffer &optional _alist)
  "Display and switch to a popup-buffer at the bottom of the screen."
  (let ((window (display-buffer-in-side-window
                 buffer
                 '((window-height . (lambda (win)
                                      (fit-window-to-buffer
                                       win
                                       (floor (frame-height) 3))))
                   (side . bottom)
                   (slot . 1)))))
    (select-window window)))

(defun popup-buffers-popup-p (buf)
  "Predicate to test if buffer BUF meets the criteria listed in
`popup-buffers-reference-buffers'."
  (or (seq-some (lambda (buf-regexp)
               (string-match-p buf-regexp (buffer-name buf)))
             popup-buffers-reference-names)
      (member (buffer-local-value 'major-mode buf) popup-buffers-reference-modes)))

(defun popup-buffers-display-control-p (buf &optional _act)
  "Predicate to test if display of buffer BUF needs to be handled
by popup-buffer. This is intended to be used in
`display-buffer-alist'."
  (let ((buffer (if (bufferp buf) buf (get-buffer buf))))
    (pcase popup-buffers-display-control 
      ('user
       (with-current-buffer buffer
         (eq popup-buffers-popup-status 'user-popup)))
      ('t (with-current-buffer buffer
            (memq popup-buffers-popup-status '(popup user-popup)))))))

(defun popup-buffers-find-popups (test-buffer-list)
  "Return an alist of (window . buffer) corresponding to
popup-buffers in the list of buffers TEST-BUFFER-LIST."
  (let* (open-popups)
    (dolist (b test-buffer-list open-popups)
      (let ((popup-status (buffer-local-value 'popup-buffers-popup-status b)))
        (when (and (not (minibufferp b))
                   (not (eq popup-status 'raised))
                   (or (member popup-status '(popup user-popup))
                       (popup-buffers-popup-p b)))
          (with-current-buffer b
            (setq popup-buffers-popup-status (or popup-status
                                                 'popup)))
          (push (cons (get-buffer-window b) b)
                open-popups))))))

;;;###autoload
(defun popup-buffers-update-popups ()
  "Update the list of currently open popups. Meant to be added to
`window-configuration-change-hook'."
  (let* ((open-buffers (mapcar #'window-buffer (window-list)))
         (open-popups (popup-buffers-find-popups open-buffers))
         (closed-popups (cl-set-difference popup-buffers-open-popup-alist
                                           open-popups
                         :test (lambda (arg1 arg2) (eql (cdr arg1) (cdr arg2))))))
         (setq popup-buffers-open-popup-alist (nreverse open-popups))
         (setq popup-buffers-buried-popup-alist
               (append closed-popups
                       popup-buffers-buried-popup-alist)))
  ;; Mode line update
  (cl-loop for (_ . buf) in popup-buffers-open-popup-alist do
             (with-current-buffer buf
               (setq mode-line-format (popup-buffers-modified-mode-line)))))

;;;###autoload
(defun popup-buffers-find-buried-popups ()
  "Update the list of currently buried popups. Meant to be run
when starting `popup-buffers-mode'."
  (setq popup-buffers-buried-popup-alist
        (popup-buffers-find-popups
            (cl-set-difference (buffer-list)
                               (mapcar #'window-buffer
                                       (window-list))))))

(defun popup-buffers-close-latest ()
  "Close the last opened popup."
  (interactive "P")
  (if (null popup-buffers-open-popup-alist)
      (message (if popup-buffers-mode
                   "No open popups!"
                 "popup-buffers-mode not active!"))
    (cl-destructuring-bind ((win . buf) . rest) popup-buffers-open-popup-alist
      (when (and (window-valid-p win) (window-parent win))
        ;;only close window when window has a parent:
        (when (not (seq-some
                    (lambda (item) (eq buf (cdr item)))
                    popup-buffers-buried-popup-alist))
          ;; buffer doesn't already exist in the buried popup list
          (push (cons win buf) popup-buffers-buried-popup-alist)
          (pop popup-buffers-open-popup-alist))
        (with-selected-window win
          (bury-buffer buf)
          (delete-window win))))))

(defun popup-buffers-open-latest ()
  "Open the last closed popup."
  (interactive)
  (if (null popup-buffers-buried-popup-alist)
      (message (if popup-buffers-mode
                   "No buried popups!"
                 "popup-buffers-mode not active!"))
    (let* ((new-popup (pop popup-buffers-buried-popup-alist))
           (buf (cdr new-popup)))
      (if (buffer-live-p buf)
          (progn (display-buffer buf))
        (popup-buffers-open-latest)))))

;;;###autoload
(defun popup-buffers-modified-mode-line ()
  "Return modified mode-line string."
  (when popup-buffers-mode-line
    (if (member popup-buffers-mode-line mode-line-format)
        mode-line-format
      (append (cl-subseq mode-line-format 0 popup-buffers-mode-line-position)
              (cons popup-buffers-mode-line (nthcdr popup-buffers-mode-line-position
                                                            mode-line-format))))))

(defun popup-buffers-bury-all ()
  "Bury all open popup buffers."
  (while popup-buffers-open-popup-alist
    (popup-buffers-close-latest)))

(defun popup-buffers-raise-all ()
  "Open all popup buffers.
Note that buffers that are displayed in the same 'position' on
the screen by `display-buffer' will not all be displayed."
  (while popup-buffers-buried-popup-alist
    (popup-buffers-open-latest)))

;;;###autoload
(defun popup-buffers-toggle-latest (&optional arg)
  "Toggle visibility of the last opened popup window.

With prefix arg C-u, toggle visibility of the next popup windows
while keeping the current one (Note: This is currently bugged.)

With a double prefix arg C-u C-u, toggle all popup-windows. Note
that only one buffer can be show in one 'slot', so it will
display as many windows as it can."
  (interactive "p")
  (if popup-buffers-open-popup-alist
      (pcase arg
        (4 (popup-buffers-open-latest))
        (16 (popup-buffers-bury-all))
        (_ (popup-buffers-close-latest)))
    (if (equal arg 16)
        (popup-buffers-raise-all)
      (popup-buffers-open-latest))))

;;;###autoload
(defun popup-buffers-cycle (&optional _arg)
  "Cycle visibility of popup windows one at a time.

TODO: With a prefix argument ARG, cycle in the opposite
direction."
  (interactive "p")
  (if (null popup-buffers-open-popup-alist)
      (popup-buffers-open-latest)
    (if (null popup-buffers-buried-popup-alist)
        (popup-buffers-bury-all) ; starting new cycle, so bury everything first.
      ;; cycle through buffers
      (popup-buffers-close-latest)
      (let ((bufs popup-buffers-buried-popup-alist))
        (setq popup-buffers-buried-popup-alist
              (append (cdr bufs) (cons (car bufs) nil))))
      (popup-buffers-open-latest))))

;;;###autoload
(defun popup-buffers-raise-popup (&optional buffer)
  "Raise a popup to regular status.
If BUFFER is not specified,raise the current buffer."
  (interactive)
  (when-let* ((buf (get-buffer (or buffer (current-buffer))))
              (popup-status (buffer-local-value 'popup-buffers-popup-status buf)))
    (with-current-buffer buf
      (setq popup-buffers-popup-status (and (popup-buffers-popup-p buf) 'raised))
      (setq mode-line-format (default-value 'mode-line-format)))
    (delete-window (get-buffer-window buf))
    (pop-to-buffer buf)))

;;;###autoload
(defun popup-buffers-lower-to-popup (&optional buffer)
  "Turn a regular window into a popup."
  (interactive)
  (let ((buf (get-buffer (or buffer (current-buffer)))))
    (with-current-buffer buf
      (setq popup-buffers-popup-status (if (popup-buffers-popup-p buf)
                                           'popup
                                         'user-popup))
      (delete-window (get-buffer-window buf t))
      (pop-to-buffer buf))
    (popup-buffers-update-popups)))

;;;###autoload
(defun popup-buffers-toggle-state (&optional buffer)
  "Turn a popup into a regular window or vice-versa."
  (interactive)
  (let* ((buf (get-buffer (or buffer (current-buffer))))
         (popup-status (buffer-local-value 'popup-buffers-popup-status buf)))
    (pcase popup-status 
      ((or 'popup 'user-popup) (popup-buffers-raise-popup buf))
      (_ (popup-buffers-lower-to-popup buf)))))

;;;###autoload
(define-minor-mode popup-buffers-mode
  "Toggle Popup Buffers mode. When enabled, treat certain buffer
windows as popups, a class of window that can be summoned or
dismissed with a command. See the customization options for
details on how to designate buffer types as popups."
  :global t
  :version "0.20"
  :lighter ""
  :group 'popup-buffers
  :keymap (let ((map (make-sparse-keymap))) map)
  (if popup-buffers-mode
      ;; Turning the mode ON
      (progn
        (setq popup-buffers-reference-names
              (cl-remove-if-not #'stringp popup-buffers-reference-buffers)
              popup-buffers-reference-modes
              (cl-remove-if-not #'symbolp popup-buffers-reference-buffers))
        (popup-buffers-find-buried-popups)
        (popup-buffers-update-popups)
        (add-hook 'window-configuration-change-hook 'popup-buffers-update-popups)
        (add-to-list 'display-buffer-alist
                     `(popup-buffers-display-control-p
                       (,popup-buffers-display-function))))
    ;; Turning the mode OFF
    (setq popup-buffers-buried-popup-alist nil
          popup-buffers-open-popup-alist nil)
    (remove-hook 'window-configuration-change-hook 'popup-buffers-update-popups)
    (setq display-buffer-alist
          (delete `(popup-buffers-display-control-p
                    (,popup-buffers-display-function))
                display-buffer-alist))))

(provide 'popup-buffers)
;;; popup-buffers ends here
