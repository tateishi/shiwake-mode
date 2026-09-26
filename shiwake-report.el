;;; shiwake-report.el --- shiwake report. -*- lexical-binding: t; -*-

;; Copyright (C) 2026 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Maintainer: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2026/03/12
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

(require 'org)
(require 'ledger-report)

(defun shiwake-report-month-format-specifier ()
  "Substitute current month.

FORMAT is yyyy-mm."
  (with-current-buffer (or ledger-report-buffer-name (current-buffer))
    (let* ((month (or ledger-report-current-month (ledger-report--current-month)))
           (year (car month))
           (month-index (cdr month)))
      (format "%04d-%02d" year month-index))))

;;; レポートの表示幅を調整する
(defcustom shiwake-column-adjust-ratio nil
  "ウィンドウ幅に対するレポートの編集幅の倍率.
nilなら補正しない。"
  :type '(choice (const :tag "補正しない" nil)
                 (number :tag "倍率"))
  :group 'ledger)

(defun shiwake-adjust-columns (args)
  "ARGS の--columns に対する幅をウィンドウ幅をもとに計算する."

  (if (not shiwake-column-adjust-ratio)
      args
    (if-let* ((pos (cl-position "--columns" args :test #'string=))
              (width (nth (1+ pos) args)))
        (progn
          (setf (nth (1+ pos) args)
                (number-to-string
                 (truncate (* shiwake-column-adjust-ratio (string-to-number width)))))
          args)
      args)))

(advice-add 'ledger-report--compute-extra-args
            :filter-return
            #'shiwake-adjust-columns)

;;; 指定日付の行へ移動、なければ直前の行へ
(defun shiwake-report-goto-nearest-date (date)
  "DATEの行へ移動、なければ直前の行へ移動する."
  (when (derived-mode-p 'ledger-report-mode)
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" nil t))
        (when (string-greaterp (match-string 1) date)
          (setq found t)
          (beginning-of-line)
          (unless (bobp)
            (forward-line -1))))
      (when found
        (recenter)))))

(defun shiwake-report-goto-nearest-today ()
  "今日の行へ移動、なければ直前の行へ移動する."
  (interactive)
  (shiwake-report-goto-nearest-date (format-time-string "%Y-%m-%d")))

(defun shiwake-report-goto-date ()
  "指定日付の行へ移動、なければ直前の行へ移動する."
  (interactive)
  (let ((date (org-read-date nil nil nil "日付: ")))
    (shiwake-report-goto-nearest-date date)))

(provide 'shiwake-report)

;;; shiwake-report.el ends here
