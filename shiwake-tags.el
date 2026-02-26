;;; shiwake-tags.el --- PoC 用コード -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(require 'org)
(require 'ledger-mode)

(defconst shiwake-tag-directive-regex
  (concat "^tag[ \t]+\\(.*?\\)[ \t]*$"))

(defcustom shiwake-tagnames-file nil
  "The path to an optional file in which all tag names are used or declared.
This file will then be used as a source for tag name
completions instead of the current file.
See ledger's \"tag\" directive."
  :type '(choice (const :tag "Use current buffer for completion" nil)
                 file)
  :group 'ledger
  :safe #'string-or-null-p)

(defun shiwake-tagnames-in-buffer ()
  "Scan buffer and return list of all tagnames."
  (let ((origin (point))
        tagnames-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward shiwake-tag-directive-regex nil t)
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (push (or (match-string-no-properties 1) (match-string-no-properties 2))
                tagnames-list))))
    (sort (delete-dups tagnames-list) #'string-lessp)))

(defun shiwake-tagnames-list ()
  "Return a list of all known tag names as strings.
Looks in `ledger-tagnames-file' if set, otherwise the current buffer."
  (if shiwake-tagnames-file
      (let ((f shiwake-tagnames-file))
        (with-temp-buffer
          (insert-file-contents f)
          (shiwake-tagnames-in-buffer)))
    (shiwake-tagnames-in-buffer)))

(defun shiwake-read-tag-with-prompt (prompt)
  "Read a tag from the minibuffer with PROMPT."
  (ledger-completing-read-with-default prompt '() (shiwake-tagnames-list)))

(defun shiwake-report-tagname-format-specifier ()
  "Return a valid meta-data tag name (shiwake version)."
  (message "shiwake-report-tagname-format-specifier.")
  (shiwake-read-tag-with-prompt "TAG"))

(provide 'shiwake-tags)
;;; shiwake-tags.el ends here
