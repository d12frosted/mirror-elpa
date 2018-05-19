;;; orgability.el --- reading list manager in `org-mode'

;; Copyright (c) 2018 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 25 Apr 2018

;; Keywords: org-mode
;; Homepage: https://github.com/d12frosted/orgability

;; Package-Version: 0.0.1
;; Package-Requires: ((org-mode "9.1.0") (org-cliplink "0.2") (org-board "1136"))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(require 'orgability-utils)
(require 'orgability-brain)
(require 'org-cliplink)
(require 'org-board)

(defvar orgability-file nil
  "File for storing reading list.")

(defvar orgability-add-to-top t
  "If non-nil, entries are added to the top of the `orgability-file'.")

(defvar orgability-todo-keyword "TODO"
  "If non-nil, entries are added with a todo keyword.")

(defvar orgability-active-timestamp nil
  "If non-nil, ADDED timestamp is active.")

(defvar orgability-auto-archive t
  "If non-nil, entry is automatically archived.")

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
        (save-buffer)
        (when orgability-auto-archive
          (org-board-archive))))))

;;;###autoload
(defun orgability-add-relation ()
  "Add two-way relation with other entry."
  (interactive)
  (let* ((entry (orgability-brain-choose-entry))
         (link (orgability-brain-get-link entry)))
    (unless (orgability--has-relation link)
      (orgability--add-relation (orgability-brain-get-link entry)
                                (org-brain-title entry))
      (orgability-brain-add-relation (org-id-get-create)
                                     entry))))

;;;###autoload
(defun orgability-delete-relation ()
  "Select and delete relation for the entry at point."
  (interactive)
  (let* ((id (org-id-get-create))
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
     (orgability-remove-till (concat "^.*" link ".*$") ":END:"))
    (orgability-brain-delete-relation id
                                      (orgability-unwrap-link link))))

(defun orgability--has-relation (link)
  "Returns non-nil if entry at point has a relation with LINK."
  (let ((result))
    (orgability-with-entry
     (orgability-goto-relations-block)
     (while (not (looking-at-p ":END:"))
       (forward-line 1)
       (beginning-of-line)
       (when (looking-at-p (concat "^.*" link ".*$"))
         (setq result t))))
    result))

(defun orgability--add-relation (link title)
  "Add relation for the entry at point."
  (unless (orgability--has-relation link)
    (orgability-with-entry
     (orgability-goto-relations-block)
     (newline-and-indent)
     (insert (concat "- " (org-make-link-string link title))))))

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

(provide 'orgability)

;;; orgability.el ends here
