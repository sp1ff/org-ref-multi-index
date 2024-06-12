;;; org-ref-multi-index-tests.el --- ERT tests for org-ref-multi-index  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit test suite for org-ref-multi-index.

;;; Code:

(require 'ert)
(require 'ox)

(require 'org-ref-multi-index)

(ert-deftest org-ref-multi-index-tests-smoke ()
  "`org-ref-multi-index' smoke tests."

  (let* ((split (org-ref-multi-index--split "index|tag"))
         (index (car split))
         (tag (cdr split)))
    (should (equal index "index"))
    (should (equal tag "tag")))

  (let* ((split (org-ref-multi-index--split "tag"))
         (index (car split))
         (tag (cdr split)))
    (should (eq index 'default))
    (should (equal tag "tag")))

  (let* ((split (org-ref-multi-index--split ""))
         (index (car split))
         (tag (cdr split)))
    (should (eq index 'default))
    (should (equal tag "")))

  (let* ((result
          (condition-case nil
              (org-ref-multi-index--split "index|tag|bad")
            (error nil))))
    (should (not result))))

(ert-deftest org-ref-multi-index-tests-index ()
  "Tests for `org-ref-multi-index-show-index'."
  (let ((buffer (find-file (concat (getenv "srcdir") "/data/multi-index-testing.org")))
        (sentence-end-double-space nil))
    (with-current-buffer buffer (org-ref-multi-index-show-index))
    (let ((text (with-current-buffer (get-buffer "*index*") (buffer-string))))
      (should
       (equal
        text
        "#+TITLE: Index

* F
flapdoodle [[elisp:(progn (switch-to-buffer \"multi-index-testing.org\") (goto-char 86) (org-cycle '(64)))][Here is a new term: Flapdoodle mindex:flapdoodle. ]] 

")))))

(ert-deftest org-ref-multi-index-tests-publish ()
  "Tests for `org-ref-multi-index-proc'."
  (let ((buffer (find-file (concat (getenv "srcdir") "/data/multi-index-testing.org"))))
    (add-to-list 'org-export-before-parsing-functions #'org-ref-multi-index-proc)
    (with-current-buffer buffer (org-org-export-as-org))
    (let ((text (with-current-buffer (get-buffer "*Org ORG Export*") (buffer-string))))
      ;; `text' will have an initial line reading like "# Created ..."-- strip that
      (let ((text (string-join (cdr (string-split text "[\n]")) "\n")))
        (should
         (equal
          text
          "#+options: num:nil ^:{}
#+title: Multi-index Testing
Here is a new term: Flapdoodle <<default-flapdoodle-0>>. We now introduce a new function: =foo()=  <<fn-foo-0>>.

New para.

Here's a second function =bar()= <<fn-bar-0>>, and here <<fn-foo-1>>is another link to an existing index entry.
* Concept Index

flapdoodle: [[default-flapdoodle-0][flapdoodle-0]]

* Function Index

bar: [[fn-bar-0][bar-0]]

foo: [[fn-foo-0][foo-0]], [[fn-foo-1][foo-1]]
"))))))

(provide 'org-ref-multi-index-tests)

;;; org-ref-multi-index-tests.el ends here
