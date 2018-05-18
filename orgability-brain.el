;;; orgability-brain.el --- reading list manager in `org-mode'

;; Copyright (c) 2018 Boris Buliga

;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Created: 25 Apr 2018

;; Keywords: org-mode
;; Homepage: https://github.com/d12frosted/orgability

;; Package-Version: 0.0.1
;; Package-Requires: ((org-mode "9.1.0") (org-brain "0.5"))

;; This file is not part of GNU Emacs.
;;; License: GPLv3

;;; Commentary:
;;

;;; Code:
;;

(require 'org-brain)

(defun orgability-brain-choose-entry ()
  "PROMPT for an brain entry and return it."
  (org-brain-choose-entry
   "Entry: " (append (org-brain-files t)
                     (org-brain-headline-entries))))

(defun orgability-brain-add-relation (orgability-id brain-entry)
  "Add relation from BRAIN-ENTRY to ORGABILITY-ID"
  (org-brain-add-resource (concat "id:" (org-id-get-create))
                          (org-entry-get nil "ITEM")
                          nil
                          brain-entry))

(defun orgability-brain-delete-relation (orgability-id brain-id)
  "Delete relation from BRAIN-ID to ORGABILITY-ID"
  (when (null orgability-id)
    (error "orgability-id can't be null"))
  (when (null brain-id)
    (error "brain-id can't be null"))
  (let ((entry (or (org-brain-entry-from-id brain-id)
                   (org-brain-path-entry-name brain-id))))
    (if (org-brain-filep entry)
        (with-current-buffer (find-file-noselect (org-brain-entry-path entry))
          (goto-char (point-min))
          (or (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
              (goto-char (point-max)))
          (when (re-search-backward org-brain-resources-start-re nil t)
            (orgability-remove-till (concat "^.*" orgability-id ".*$") ":END:")))
      (org-with-point-at (org-brain-entry-marker entry)
        (goto-char (cdr (org-get-property-block)))
        (forward-line 1)
        (when (looking-at org-brain-resources-start-re)
          (orgability-remove-till (concat "^.*" orgability-id ".*$") ":END:"))))))

(defun orgability-brain-get-link (brain-entry)
  "Get link to BRAIN-ENTRY."
  (if (org-brain-filep brain-entry)
      (concat "file://" (org-brain-entry-path brain-entry))
    (concat "id:" (org-brain-entry-identifier brain-entry))))

(provide 'orgability-brain)

;;; orgability-brain.el ends here
