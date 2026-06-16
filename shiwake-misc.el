;;; shiwake-misc.el --- shiwake-mode misc code. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TATEISHI Tadatoshi

;;; Commentary:
;;; Code:

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

(provide 'shiwake-misc)

;;; shiwake-misc.el ends here
