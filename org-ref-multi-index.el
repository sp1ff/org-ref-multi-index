;;; org-ref-multi-index.el --- Multiple indicies in org-ref   -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: text
;; URL: https://github.com/sp1ff/org-ref-multi-index

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

;; This package adds support for multiple indicies to org-ref.  The Org
;; document must be using the `imakeidx` Latex package.

;;; Code:
(require 'cl-lib)
(require 'ol)
(require 'org-element)

(defconst org-ref-multi-index-version "0.0.1")

(defun org-ref-multi-index--split (pth)
  "Split PTH into it's index & label.

mindex:A|B means label B in index A. mindex:B means label B in the
default index."
  (let ((split (string-split pth "|")))
    (cond
     ((= 1 (length split))
      (cons 'default pth))
     ((= 2 (length split))
      (cons (nth 0 split) (nth 1 split)))
     (t
      (user-error "Unable to interpret %S as an org-ref-multi-index path" pth)))))


;;;###autoload
(defun org-ref-multi-index-show-index (&optional path)
  "Open an buffer with links to index entries.
PATH will be the empty string for the default index, or the index name."
  (interactive)

  (let ((path (if (> (length path) 0) path 'default))
	      (index-links '())
        (initial-letters '()))
    (org-element-map
	      (org-element-parse-buffer)
	      'link
      (lambda (link)
	      (let* ((plist (nth 1 link))
	             (ty (plist-get plist :type))
	             (pth (plist-get plist :path)))
	        (when (equal ty "mindex")
	          (let* ((split (org-ref-multi-index--split pth))
		               (index (car split))
		               (tag (cdr split)))
	            (when (equal path index)
		            (add-to-list
		             'index-links
		             (cons
		              tag
		              (format
		               "[[elisp:(progn (switch-to-buffer \"%s\") (goto-char %s) (org-cycle '(64)))][%s]] "
                   (current-buffer)
                   (plist-get plist :begin) ;; position of link
                   ;; grab a description
                   (save-excursion
                     (goto-char (plist-get plist :begin))
                     (if (thing-at-point 'sentence)
                         ;; get a sentence
                         (let ((s (thing-at-point 'sentence)))
                           (cl-loop for char in '("[" "]" "\n")
                                    do
                                    (setq s (replace-regexp-in-string
                                             (regexp-quote char) " " s)))
                           (concat s " "))
                       ;; or call it a link
                       "link")))))))))))

    ;; sort the links
    (setq index-links (cl-sort index-links 'string-lessp :key 'car))

    ;; now separate out into chunks first letters
    (dolist (link index-links)
      (push (substring (car link) 0 1) initial-letters))

    (setq initial-letters (reverse initial-letters))

    ;; now create the index
    (switch-to-buffer (get-buffer-create "*index*"))
    (org-mode)
    (erase-buffer)
    (insert "#+TITLE: Index\n\n")
    (dolist (letter initial-letters)
      (insert (format "* %s\n" (upcase letter)))
      ;; now process the links
      (while (and
              index-links
              (string= letter (substring (car (car index-links)) 0 1)))
        (let ((link (pop index-links)))
          (insert (format "%s %s\n\n" (car link) (cdr link))))))
    (switch-to-buffer "*index*")))

;;;###autoload
(defun org-ref-multi-index-proc (backend)
  "Preprocess multi-index entries.
Do nothing if BACKEND is \='latex.

Each entry is replaced by a radio target.  Any printmindex links are
replaced by the actual indices."
  (unless (eq 'latex backend)
    ;; `index-links' is a flat list of Org link elements, ordered by
    ;; location in the document. They have to be kept in this order so
    ;; that we replace them with radio targets in sequence.
    (let ((index-links
	         (reverse
	          (org-element-map
		            (org-element-parse-buffer)
		            'link
	            (lambda (lnk)
		            (when (string= "mindex" (org-element-property :type lnk))
		              lnk))))))
      ;; We can have multiple links to the same index entry scattered
      ;; throughout the document. For each index, first group the
      ;; links by tag, then sort them by tag.
      ;; `sorted-groups' will be an alist of alists. The outer alist
      ;; is indexed by, well, index. For each index, the associated
      ;; alist is indexed by strings naming the index entry (i.e. the
      ;; "tag"). The values themselves are Org link elements.
      (let* ((grouped-links
	            (seq-group-by
	             (lambda (link)
		             (let* ((plist (nth 1 link))
			                  (pth (plist-get plist :path))
			                  (index (car (org-ref-multi-index--split pth))))
		               index))
	             index-links)) ;; 👈 alist; keys are index
	           (sorted-groups)) ;; 👈 alist of alists; index -> tag -> links
	      (cl-loop for (index . links) in grouped-links do
                 (push
                  (cons
		               index
		               (seq-sort-by
			              #'car
			              #'string-lessp
			              (seq-group-by
			               (lambda (link)
			                 (cdr
			                  (org-ref-multi-index--split
				                 (org-element-property :path link))))
			               links)))
                  sorted-groups))
	      ;; Now, within each group of index entries in each "inner"
	      ;; alist, sort by location.
	      (cl-loop for (_index . alist) in sorted-groups do
		             (cl-loop for (key . links) in alist do
			                    (setf
			                     (alist-get key links nil nil 'equal)
			                     (sort
			                      links
			                      (lambda (a b)
			                        (< (org-element-property :begin a)
				                         (org-element-property :begin b)))))))
	      ;; Next, compute the radio targets with which we'll be
	      ;; replacing all "mindex" links. We'll again build an alist
	      ;; of alists
	      (let ((link-replacements))
	        (cl-loop for (index . alist) in sorted-groups do
		               (cl-loop for (key . links) in alist do
			                      (setf
			                       (alist-get
			                        key
			                        (alist-get index link-replacements nil nil 'equal)
			                        nil nil 'equal)
			                       (cl-loop for i from 0 for lnk in links collect
				                              (cons
				                               (org-element-property :begin lnk)
				                               (format "<<%s-%s-%s>>" index key i))))))
	        ;; Now we actually replace all the "mindex" links
	        (cl-loop for il in index-links do
		               (let* ((plist (nth 1 il))
			                    (pth (plist-get plist :path))
			                    (split (org-ref-multi-index--split pth))
			                    (index (car split))
			                    (tag (cdr split))
			                    (radio
			                     (alist-get
			                      (org-element-property :begin il)
			                      (alist-get
			                       tag
			                       (alist-get index link-replacements nil nil 'equal)
			                       nil nil 'equal)
			                      nil nil 'equal)))
		                 (cl--set-buffer-substring
		                  (org-element-property :begin il)
		                  (org-element-property :end il)
		                  radio)))
	        ;; With the export buffer thus prepared, and our indicies
	        ;; built in-memory, let's replace any "printmindex" links,
	        ;; re-parsing the buffer so positions are up-to-date.
	        (let ((tally 0))
	          (org-element-map
		            (org-element-parse-buffer)
		            'link
	            (lambda (link)
		            (when (string= "printmindex" (org-element-property :type link))
		              (let* ((path (org-element-property :path link))
			                   (key (if (eq 0 (length path)) 'default path))
			                   (sorted (alist-get key sorted-groups nil nil 'equal))
			                   (text
			                    (string-join
			                     (cl-loop for (tag . links) in sorted collect
				                            (format "%s: %s"
					                                  tag
					                                  (string-join
					                                   (cl-loop for i from 0 for _link in links collect
						                                          (format "[[%s-%s-%s][%s-%s]]"
							                                                key
							                                                tag
							                                                i
							                                                tag
							                                                i))
					                                   ", ")))
			                     "\n\n"))
			                   (begin (org-element-property :begin link))
			                   (end (org-element-property :end link)))
		                (cl--set-buffer-substring
		                 (+ tally begin)
		                 (+ tally end)
		                 text)
		                (setq tally (- (+ tally (length text)) (- end begin)))))))))))))

(org-link-set-parameters
 "mindex"
 :follow
 (lambda (path)
   (occur (cdr (org-ref-multi-index--split path))))
 :export
 (lambda (path _desc format)
   (cond
    ((eq format 'latex)
     (let* ((split (org-ref-multi-index--split path))
	          (index (car split))
	          (tag (cdr split)))
       (if (eq 'default index)
	         (format "\\index{%s}" tag)
         (format "\\index[%s]{%s}" index tag)))))))

(org-link-set-parameters
 "printmindex"
 :follow #'org-ref-multi-index-show-index
 :export
 (lambda (path _desc format)
   (cond
    ((eq format 'latex)
	   (if (eq 0 (length path))
			   (format "\\printindex")
		   (format "\\printindex[%s]" path))))))

(provide 'org-ref-multi-index)

;;; org-ref-multi-index.el ends here
