;;; shiwake-mode.el --- My major mode for editing ledger files. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Maintainer: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2021/04/09
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'ledger-mode)
(require 'shiwake-insert-interest)
(require 'shiwake-tags)

(defconst shiwake-date-template
  "
# =================== %Y/%m/%d ===================\n")

(defconst shiwake-account-template
  "
#                     %s
# --------------------------------------------------\n\n")

(defun shiwake-date ()
  (interactive)
  (insert (format-time-string shiwake-date-template)))

(defun shiwake-account (account)
  (interactive "MAccount: ")
  (insert (format shiwake-account-template account)))

(defun shiwake-skip-account-backward ()
  (save-excursion
    (skip-chars-backward "^[:space:]\n()")))

(defun shiwake-skip-account-forward ()
  (save-excursion
    (skip-chars-forward "^[:space:]\n()")))

(defun shiwake-bounds-of-account-at-point ()
  (let* ((cur (point))
         (b (shiwake-skip-account-backward))
         (e (shiwake-skip-account-forward)))
    (cons (+ cur b) (+ cur e))))

(defun shiwake-account-at-point ()
  (let ((bounds (shiwake-bounds-of-account-at-point)))
    (buffer-substring (car bounds) (cdr bounds))))

(defun shiwake-read-account ()
  (interactive)
  (save-excursion
    (let* ((bounds (shiwake-bounds-of-account-at-point))
           (beg (car bounds))
           (end (cdr bounds))
           (res (completing-read "Account: "
                                 (ledger-accounts-list)
                                 nil
                                 nil
                                 (shiwake-account-at-point))))
      (delete-region beg end)
      (insert res))))

(defun shiwake-replace-account (new)
  "Rename account under point to NEW."
  (interactive
   (let* ((new-name (ledger-read-account-with-prompt "New account")))
     (list new-name)))
  (save-excursion
    (goto-char (pos-bol))
    (if (re-search-forward ledger-account-name-or-directive-regex (line-end-position) t)
        (replace-match new 'fixedcase 'literal nil 1)
      (insert new)
      (indent-for-tab-command)
      )
    (when ledger-post-auto-align
      (ledger-post-align-postings (point-min) (point-max)))))

(defun shiwake-insert-payee (new)
  "Insert payee NEW."
  (interactive
   (let* ((new-name (ledger-read-payee-with-prompt "New Payee")))
     (list new-name)))
  (save-excursion
    (let ((cur-pos (point)))
      (goto-char (pos-bol))
      (if (re-search-forward ledger-payee-name-or-directive-regex (line-end-position) t)
          (replace-match new 'fixedcase 'literal nil 1)
        (goto-char cur-pos)
        (insert new)
        (indent-for-tab-command)
        )
      (when ledger-post-auto-align
        (ledger-post-align-postings (point-min) (point-max))))
    ))

(defun shiwake-toggle-pending-current-transaction ()
  "Set the transaction at point using pending."
  (interactive)
  (ledger-toggle-current-transaction 'pending))

(defun shiwake-mode-clean-buffer ()
  "Indent, remove multiple line feeds the buffer."
  (interactive)
  (let ((start (point-min-marker))
        (end (point-max-marker))
        (distance-in-xact (- (point) (ledger-navigate-beginning-of-xact))))
    (let ((target (buffer-substring (line-beginning-position) (line-end-position))))
      (goto-char start)
      (untabify start end)
      (ledger-post-align-postings start end)
      (ledger-mode-remove-extra-lines)
      (goto-char start)
      (search-forward target)
      (beginning-of-line)
      (forward-char distance-in-xact))))

(defun shiwake-mode-replace-first-date-in-current-line (new-date)
  "カーソルのある行に含まれる最初の YYYY/MM/DD を NEW-DATE に置換.
NEW-DATE は YYYY/MM/DD 形式の文字列.日付がなければ何もしない."
  (interactive
   (list
    (format-time-string "%Y/%m/%d"
                        (org-read-date nil t nil "日付を選択:"))))
  (save-excursion
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (goto-char line-start)
      (when (re-search-forward "\\([0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}\\)" line-end t)
        (replace-match new-date nil nil nil 1)))))

(defface shiwake-mode-font-tag-face
  '((t :inherit font-lock-comment-face :inverse-video t))
  "Default face for tag.")

(defface shiwake-mode-meta-key-face
  '((t (:foreground "#83a598" :weight bold)))
  "Face for Ledger metadata keys like key: in comments.")

(defface shiwake-mode-meta-value-face
  '((t (:foreground "#b8bb26")))
  "Face for Ledger metadata values like key: VALUE in comments.")

(defvar shiwake-font-lock-keywords
  (append
   '((" \\(:[^:\n]*:\\([^:\n]*:\\)*\\)" (1 'shiwake-mode-font-tag-face))
     (";[ \t]*\\([[:alnum:]_@#%+.-]+\\):" (1 'shiwake-mode-meta-key-face))
     (";[ \t]*\\([[:alnum:]_@#%+.-]+\\):" (1 'shiwake-mode-meta-key-face))

     )
   ledger-font-lock-keywords
   ))

(defvar shiwake-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-h") #'shiwake-account)
    (define-key map (kbd "C-c C-l") #'shiwake-mode-clean-buffer)
    (define-key map (kbd "C-c C-m") #'shiwake-insert-payee)
    (define-key map (kbd "C-c C-n") #'shiwake-replace-account)
    (define-key map (kbd "C-c C-j") #'shiwake-mode-replace-first-date-in-current-line)
    (define-key map (kbd "C-c ,") #'shiwake-insert-interest-transaction)
    map))

(define-derived-mode shiwake-mode ledger-mode "Shiwake"
  "Shiwake-mode is a my major mode for editing ledger data.

\\{shiwake-mode-map}"
  (setq font-lock-defaults '(shiwake-font-lock-keywords t)))

(provide 'shiwake-mode)

;;; shiwake-mode.el ends here
