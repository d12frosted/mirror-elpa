;;; orgability.el --- reading list manager in `org-mode'

;; Copyright (c) 2018 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 25 Apr 2018

;; Keywords: org-mode
;; Homepage: https://github.com/d12frosted/orgability

;; Package-Version: 0.0.1
;; Package-Requires: ((org-cliplink "0.2") (org-brain "0.5") (org-brain "1136"))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(require 'org-brain)
(require 'org-cliplink)
(require 'org-cliplink)

(defvar orgability-file nil
  "File for storing reading list.")

(defvar orgability-add-to-top t
  "If non-nil, entries are added to the top of the `orgability-file'.")

(defvar orgability-todo-keyword "TODO"
  "If non-nil, entries are added with a todo keyword.")

(defvar orgability-active-timestamp nil
  "If non-nil, ADDED timestamp is active.")

(defconst orgability-title "Reading list"
  "Title of `orgability-file'.")

(defconst orgability-category "reading-list"
  "Category of `orgability-file' entries.")

(defconst orgability-relations-start-re "^[ \t]*:RELATIONS:[ \t]*$"
  "Regular expression matching the first line of a relations drawer.")

(defun orgability-create-file (file)
  "Create an orgability FILE if it doesn't exist."
  (unless (file-exists-p file)
    (with-temp-file file
      (insert "#+TITLE: " orgability-title "\n"
              "#+CATEGORY: " orgability-category "\n\n"))))

;;;###autoload
(defun orgability-clip ()
  "Store an URL from the clipboard."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (orgability-store-url url)))

;;;###autoload
(defun orgability-store-url (url)
  "Store an URL."
  (interactive "sUrl: ")
  (let ((title (org-cliplink-retrieve-title-synchronously url)))
    (orgability-add-entry title url)))

(defun orgability-add-entry (title url &optional props)
  "Add read entry with TITLE and URL and optional PROPS."
  ;; (interactive "sTitle: \nsURL: ")
  (unless orgability-file
    (user-error "`orgability-file' is not set"))
  (orgability-create-file orgability-file)
  (save-excursion
    (let ((buffer (find-file-noselect orgability-file)))
      (with-current-buffer buffer
        (if (and orgability-add-to-top
                 (progn (goto-char (point-min))
                        (search-forward "*" nil t)))
            (previous-line)
          (goto-char (point-max)))
        (org-insert-heading nil nil t)
        (when orgability-todo-keyword
          (insert orgability-todo-keyword " "))
        (insert title "\n")
        (org-set-property "URL" url)
        (org-set-property "ADDED" (concat
                                   (if orgability-active-timestamp "<" "[")
                                   (format-time-string "%Y-%02m-%02d")
                                   (if orgability-active-timestamp ">" "]")))
        (mapc (lambda (p) (org-set-property (car p) (cdr p))) props)
        (save-buffer)))))

(defun orgability-add-relation (link title)
  "Add relation for the entry at point."
  (orgability-with-entry
   (orgability-goto-relations-block)
   (newline-and-indent)
   (insert (concat "- " (org-make-link-string link title)))))

(defun orgability-delete-relation ()
  "Select and delete relation for the entry at point."
  (interactive)
  (let* (id (org-id-get-create)
            (relations (orgability-list-relations))
            (target (completing-read
                     "Relation: "
                     (mapcar #'cdr
                             (orgability-list-relations))))
            (link (car (find-if (lambda (x)
                                  (string-equal target
                                                (cdr x)))
                                relations))))
    (orgability-with-entry
     (orgability-goto-relations-block)
     (while (not (looking-at ":END:"))
       (forward-line 1)
       (beginning-of-line)
       (when (looking-at (concat "^.*" link ".*$"))
         (kill-whole-line))))

    (when-let* ((is-id (string-prefix-p "id:" link))
                (brain-id (string-remove-prefix "id:" link))
                (entry (org-brain-entry-from-id brain-id))
                (is-headline (not (org-brain-filep entry))))
      (org-with-point-at (org-brain-entry-marker entry)
        (goto-char (cdr (org-get-property-block)))
        (forward-line 1)
        (when (looking-at org-brain-resources-start-re)
          (while (not (looking-at ":END:"))
            (forward-line 1)
            (beginning-of-line)
            (when (looking-at (concat "^.*" id ".*$"))
              (kill-whole-line))))))

    (when-let* ((is-file (string-prefix-p "file:" link))
                (file (string-remove-prefix "file:" link))
                (entry (org-brain-path-entry-name file))
                (is-file (org-brain-filep entry)))
      (with-current-buffer (find-file-noselect (org-brain-entry-path entry))
        (goto-char (point-min))
        (or (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
            (goto-char (point-max)))
        (when (re-search-backward org-brain-resources-start-re nil t)
          (while (not (looking-at ":END:"))
            (forward-line 1)
            (beginning-of-line)
            (when (looking-at (concat "^.*" id ".*$"))
              (kill-whole-line))))))))

(defun orgability-goto-relations-block ()
  "Move the point inside of relations block."
  (goto-char (cdr (org-get-property-block)))
  (forward-line 1)
  (if (looking-at orgability-relations-start-re)
      (end-of-line)
    (progn
      (open-line 1)
      (indent-for-tab-command)
      (insert ":RELATIONS:\n")
      (indent-for-tab-command)
      (insert ":END:")
      (re-search-backward orgability-relations-start-re nil t)
      (end-of-line))))

(defun orgability-list-relations ()
  "Get the relations list of entry at point."
  (interactive)
  (orgability-with-entry
   (orgability-goto-relations-block)
   (forward-line 1)
   (beginning-of-line)
   (let* ((data-raw (buffer-substring (point) (search-forward ":END:")))
          (data (with-temp-buffer
                  (insert data-raw)
                  (org-element-parse-buffer)))
          (links
           (delete-dups
            (org-element-map data 'link
              (lambda (link)
                (cons (org-element-property :raw-link link)
                      (when-let ((desc (car (org-element-contents link))))
                        (replace-regexp-in-string "[ \t\n\r]+" " " desc))))
              nil nil t))))
     links)))

;;;###autoload
(defun orgability-add-brain-relation ()
  "Add two-way relation with brain entry."
  (interactive)
  (let ((entry (org-brain-choose-entry
                "Entry: " (append (org-brain-files t)
                                  (org-brain-headline-entries)))))
    (orgability-add-relation (orgability-get-brain-link entry)
                             (org-brain-title entry))
    (orgability-with-entry
     (org-brain-add-resource (concat "id:" (org-id-get-create))
                             (org-entry-get nil "ITEM")
                             nil
                             entry))))

(defun orgability-get-brain-link (brain-entry)
  "Get link to BRAIN-ENTRY."
  (if (org-brain-filep brain-entry)
      (concat "file://" (org-brain-entry-path brain-entry))
    (concat "id:" (org-brain-entry-identifier brain-entry))))

(defmacro orgability-with-entry (&rest body)
  "Move to buffer and point of current entry for the duration of BODY."
  `(cond ((eq major-mode 'org-mode)
          (org-with-point-at (point) ,@body))
         ((eq major-mode 'org-agenda-mode)
          (org-agenda-with-point-at-orig-entry nil ,@body))))

(provide 'orgability)

;;; orgability.el ends here
