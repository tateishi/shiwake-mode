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
(defun shiwake-report-compute-extra-args-advice (orig-fun report-cmd)
  "レポートの表示幅を調整する.

ORIG-FUN に引数 REPORT-CMD としてを呼び出し、結果を加工して
レポートの表示幅を調整する."
  (message "advice arg: %S" report-cmd)
  (let ((args (funcall orig-fun report-cmd)))
    (message "advice orig-fun called: args: %S" args)

    (if-let ((pos (cl-position "--columns" args :test #'string=))
             (width (nth (1+ pos) args)))
        (progn
          (setf (nth (1+ pos) args)
                (number-to-string
                 (truncate (* 0.8 (string-to-number width)))))
          (message "advice after edit: args: %S" args)
          args
          )
      args)))

;;; レポート表示幅調整用adviceをつける
(advice-add 'ledger-report--compute-extra-args
            :around
            #'shiwake-report-compute-extra-args-advice)

;;; レポート表示幅調整用adviceを外す
;; (advice-remove 'ledger-report--compute-extra-args
;;                #'shiwake-report-compute-extra-args-advice)

;;; advice が設定されているか判定
;; (advice-member-p #'shiwake-report-compute-extra-args-advice
;;                  'ledger-report--compute-extra-args)

(provide 'shiwake-report)

;;; shiwake-report.el ends here
