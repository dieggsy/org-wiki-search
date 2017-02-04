;;; wiki-search.el --- Advanced wikipedia lookup -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: http://github.com/therockmandolinist/emacs-hacker-typer
;; Git-Repository: git://github.com/therockmandolinist/emacs-hacker-typer.git
;; Created: 2016-01-20
;; Version: 0.1.0
;; Keywords: reference search wiki
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (deferred "0.5.0") (request-deferred "0.3.0") (org "8.2.10") (toc-org "20170131.558"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides a customizable implementation of hackertyper.com in
;; Emacs for your amusement. It opens a buffer in which typing any letter
;; inserts a piece of a specified or randomly chosen script. It can also pull
;; up a picture of "hackerman" on command.

;;; Code:

(require 'request-deferred)
(require 'deferred)
(require 'cl-lib)
(require 'org)

(defvar wiki-search--stack '()
  "Search history. Internal.")

;;;###autoload
(defun wiki-search (arg &optional search-term back forward action)
  "Search wikipedia."
  (interactive "p")
  (let ((input (if search-term
                   search-term
                 (read-string
                  (concat "Search wiki ["
                          (if (region-active-p)
                              (buffer-substring (region-beginning) (region-end))
                            (thing-at-point 'word)) "]: ")
                  nil nil
                  (thing-at-point 'word)))))


    (cond
     ((eq arg 1)
      (deferred:$
        (request-deferred
         "https://en.wikipedia.org/w/api.php"
         :type "GET"
         :parser 'json-read
         :params `(("action" . "query")
                   ("prop" . "extracts")
                   ("format" . "json")
                   ("redirects" . "")
                   ("exintro" . "")
                   ("explaintext" . "")
                   ("titles" . ,input)))
        (deferred:nextc it
          (lambda (response)
            (let* ((json-data (request-response-data response))
                   (title (cdr (wiki-search--assoc-find-key 'to json-data)))
                   (content (cdr (wiki-search--assoc-find-key 'extract json-data))))

              (when (get-buffer "*wiki*")
                (kill-buffer "*wiki*"))
              (get-buffer-create "*wiki*")
              (with-current-buffer "*wiki*"
                (org-mode)
                (evil-motion-state)
                (insert (concat "#+TITLE: " input "\n"))
                (insert content)
                (insert (format "\n[[elisp:(wiki-search\"4\"\"%s\")][Read more]]" input))
                (goto-char (point-min))
                (while (re-search-forward "\n" nil t)
                  (replace-match "\n\n"))
                (fill-region (point-min) (point-max))
                (define-key (current-local-map) (kbd "q") #'quit-window))
              (display-buffer "*wiki*" (when search-term '(display-buffer-same-window . nil))))))))
     ((or (eq arg 4) (eq (string-to-number arg) 4))
      (deferred:$
        (request-deferred
         "https://en.wikipedia.org/w/api.php"
         :type "GET"
         :parser 'json-read
         :params `(("action" . "query")
                   ("prop" . "revisions")
                   ("rvprop" . "content")
                   ("format" . "json")
                   ("redirects" . "")
                   ("titles" . ,input)))
        (deferred:nextc it
          (lambda (response)
            (let* ((json-data (request-response-data response))
                   (title (cdr (wiki-search--assoc-find-key 'title json-data)))
                   (content (cdr (wiki-search--assoc-find-key '* json-data))))
              (when (get-buffer "*wiki*")
                (kill-buffer "*wiki*"))
              (get-buffer-create "*wiki*")
              (with-current-buffer "*wiki*"
                (let ((inhibit-redisplay (when search-term t)))
                  (insert content)
                  (goto-char (point-min))
                  (insert (concat title "\n\n"))
                  (call-process-region (point-min)
                                       (point-max)
                                       "pandoc"
                                       t t nil
                                       "-f" "mediawiki" "-t" "org")
                  (goto-char (point-min))
                  (insert "#+TITLE: ")
                  (dolist (reps '(("\n\\*\\*" . "\n*")
                                  ("\\[\\[file:\\([^]]+?\\)\\]\\]" . "[[elisp:(wiki-search 4 \"\\1\")][\\1]]" )
                                  ("\\[\\[file:\\([^]]+?\\)\\]\\[\\([^]]+?\\)\\]\\]" . "[[elisp:(wiki-search\"\\1\")][\\2]]")
                                  ("\\[\\[\\(Category:[^]]+?\\)\\]\\[\\([^]]+?\\)\\]\\]" . "[[https://en.wikipedia.org/wiki/\\1][\\2]]")
                                  ("elisp:(wiki-search \"\\(.*?\\.\\(jpg\\|png\\)\\)\")" . "https://en.wikipedia.org/wiki/file:\\1")
                                  ("-  " . "- ")))
                    (goto-char (point-min))
                    (while (re-search-forward (car reps) nil t)
                      (replace-match (cdr reps))))
                  (org-mode)
                  (goto-char (point-min))
                  (org-insert-heading)
                  (insert "Contents")
                  (org-set-tags-to ":toc:TOC_3:")
                  (toc-org-insert-toc)
                  (evil-motion-state)
                  (outline-show-all)
                  (fill-region (point-min) (point-max))
                  (org-global-cycle t)
                  (define-key (current-local-map) (kbd "q") #'quit-window)
                  (read-only-mode)))
              (display-buffer "*wiki*" (when search-term '(display-buffer-same-window . nil)))))))))))

;;; utils

(defun wiki-search--assoc-find-key (key tree)
  "Return cons with given key in nested assoc list."
  (cond ((consp tree)
         (cl-destructuring-bind (x . y)  tree
           (if (eql x key) tree
             (or (wiki-search--assoc-find-key key x) (wiki-search--assoc-find-key key y)))))
        ((vectorp tree)
         (wiki-search--assoc-find-key key (elt tree 0)))))

(provide 'wiki-search)

;;; wiki-search.el ends here


