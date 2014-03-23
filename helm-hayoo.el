;;; helm-hayoo.el --- Source and configured helm for searching hayoo.

;; Copyright (C) 2014 Markus Hauck

;; Author: Markus Hauck <markus1189@gmail.com>
;; Maintainer: Markus Hauck <markus1189@gmail.com>
;; Keywords: helm
;; Version: 0.0.1
;; Package-requires: ((helm "20140322") (json "1.4"))

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

(defvar helm-hayoo-query-url
  "http://holumbus.fh-wedel.de/hayoo/hayoo.json?query=%s"
  "Url used to query hayoo, must have a `%s' placeholder.")

(defun helm-hayoo-make-query (query)
  "Returns a valid query for hayoo, searching for QUERY, which is
passed beforehand to `url-encode-url'."
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
  "Format a json response for display in helm."
  (let ((package (assoc-default 'package result))
        (signature (assoc-default 'signature result))
        (name (assoc-default 'name result))
        (module (assoc-default 'module result)))
    (format "(%s) %s %s :: %s" package module name signature)))

(defvar helm-source-hayoo
  '((name . "Hayoo")
    (volatile)
    (requires-pattern . 2)
    (action . (("Insert name" . (lambda (e)
                                  (progn (insert (assoc-default 'name e))
                                         (message (helm-hayoo-format-result e)))))
               ("Kill name" . (lambda (e) (kill-new (assoc-default 'name e))))
               ("Browse haddock" . (lambda (e) (browse-url (assoc-default 'uri e))))))
    (candidates . helm-hayoo-search)
    (delayed . 0.5))
  "Helm source for searching hayoo.")

(defun helm-hayoo ()
  "Preconfigured helm to search hajoo."
  (interactive)
  (helm :sources '(helm-source-hayoo)))

(provide 'helm-hayoo)
;;; helm-hayoo.el ends here
