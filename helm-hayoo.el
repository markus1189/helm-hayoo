;;; helm-hayoo.el --- Source and configured helm for searching hayoo

;; Copyright (C) 2014 Markus Hauck

;; Author: Markus Hauck <markus1189@gmail.com>
;; Maintainer: Markus Hauck <markus1189@gmail.com>
;; Keywords: helm
;; Version: 0.0.2
;; Package-requires: ((helm "1.6.0") (json "1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a helm source `helm-source-hayoo' and a
;; configured helm `helm-hayoo' to search hayoo online.
;;

;;; Code:

(require 'helm)
(require 'json)

(defgroup helm-hayoo nil
  "Helm source for hayoo."
  :group 'helm)

(defcustom helm-hayoo-query-url
  "http://holumbus.fh-wedel.de/hayoo/hayoo.json?query=%s"
  "Url used to query hayoo, must have a `%s' placeholder."
  :type 'string)

(defcustom helm-hayoo-sort-imports t
  "If non-nil, sort imports after adding a new one."
  :type 'boolean)

(defcustom helm-hayoo-align-imports t
  "If non-nil, align imports after adding a new one."
  :type 'boolean)

(defun helm-hayoo-make-query (query)
  "Url encode and return a valid query for QUERY to hayoo."
  (format helm-hayoo-query-url (url-encode-url query)))

(defun helm-hayoo-search ()
  "Search hayoo for current `helm-pattern'."
  (mapcar (lambda (result) (cons (helm-hayoo-format-result result) result))
          (append (assoc-default 'functions (helm-hayoo-do-search helm-pattern)) nil)))

(defun helm-hayoo-do-search (query)
  "Retrieve json response for search QUERY from hayoo."
  (with-current-buffer
         (url-retrieve-synchronously
          (helm-hayoo-make-query query))
       (json-read-object)))

(defun helm-hayoo-format-result (result)
  "Format json parsed response RESULT for display in helm."
  (let ((package (assoc-default 'package result))
        (signature (assoc-default 'signature result))
        (name (assoc-default 'name result))
        (module (assoc-default 'module result)))
    (format "(%s) %s %s :: %s" package module name signature)))

(defun helm-hayoo-action-insert-name (item)
  "Insert name of ITEM at point."
  (insert (assoc-default 'name item))
  (message (helm-hayoo-format-result item)))

(defun helm-hayoo-action-browse-haddock (item)
  "Browse haddock for ITEM."
  (browse-url (assoc-default 'uri item)))

(defun helm-hayoo-action-kill-name (item)
  "Kill the name of ITEM."
  (kill-new (assoc-default 'name item)))

(defun helm-hayoo-action-import (item)
  "Insert a haskell import statement for ITEM."
  (if (not (equal 'haskell-mode major-mode))
      (message "Can't import if not in haskell-mode buffer.")
    (save-excursion
      (goto-char (point-min))
      (haskell-navigate-imports)
      (insert (concat (helm-hayoo-format-item-for-import item) "\n"))
      (if helm-hayoo-sort-imports (haskell-sort-imports))
      (if helm-hayoo-align-imports (haskell-align-imports)))))

(defun helm-hayoo-format-item-for-import (item)
  "Format json parsed item ITEM for usage as a haskell import statement."
  (let ((module (assoc-default 'module item))
        (name (assoc-default 'name item)))
    (format "import %s (%s)" module name)))

(defun helm-hayoo-matcher-name (candidate)
  "Try to match `helm-pattern' in the name of CANDIDATE."
  (string-match-p helm-pattern candidate))

(defvar helm-hayoo-item-matcher '(helm-hayoo-matcher-name
                                  (lambda (c) t))
  "List of functions that are called by helm to determine if candidates match.")

(defvar helm-source-hayoo
  `((name . "Hayoo")
    (volatile)
    (requires-pattern . 2)
    (match . ,helm-hayoo-item-matcher)
    (action . (("Insert name" . helm-hayoo-action-insert-name)
               ("Kill name" . helm-hayoo-action-kill-name)
               ("Browse haddock" . helm-hayoo-action-browse-haddock)
               ("Import this" . helm-hayoo-action-import)))
    (candidates . helm-hayoo-search))
  "Helm source for searching hayoo.")

;;;###autoload
(defun helm-hayoo ()
  "Preconfigured helm to search hayoo."
  (interactive)
  (helm :sources '(helm-source-hayoo)))

(provide 'helm-hayoo)
;;; helm-hayoo.el ends here
