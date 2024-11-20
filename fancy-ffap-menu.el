;;; fancy-ffap-menu.el --- Interface for viewing and choosing URLs to open. -*- lexical-binding: t -*-

;; Copyright (C) 2024  Yuval Langer

;; Maintainer: yuval.langer@gmail.com
;; Keywords: convenience
;; Package:

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The FFAP (Find File At Point!  Look it up, it's legit and not at
;; all a cheeky joke!) URLs Menu is used to view and choose the
;; visible URLs one would like to open in a browser (EWW, links2, or
;; something deficient like Firefox).  The entry points are (TODO:
;; shouldn't I leave it up to the user?) `C-x C-b'
;; (`fancy-ffap-menu-list-urls').

;;; Code:

(require 'tabulated-list)

(defvar-keymap fancy-ffap-menu-mode-map
  :doc "Local keymap for `fancy-ffap-menu-mode' buffers."
  :parent tabulated-list-mode-map
  "o" #'fancy-ffap-menu-mark-open
  "a" #'fancy-ffap-menu-mark-open-all
  "x" #'fancy-ffap-menu-open-marked-entries
  "u" #'fancy-ffap-menu-unmark
  "U" #'fancy-ffap-menu-unmark-all
  )

(easy-menu-define fancy-ffap-menu-mode-menu fancy-ffap-menu-mode-map
  "Menu for `fancy-ffap-menu-mode' buffers."
  '("fancy-ffap-menu"
    ["Mark Open" fancy-ffap-menu-mark-open
     :help "Mark URL on this line for being displayed by `browse-url' command"]
    ["Unmark all" fancy-ffap-menu-unmark-all
     :help "Cancel all requested operations on URLs"]
    ["Unmark" fancy-ffap-menu-unmark
     :help "Cancel all requested operations on buffer on this line and move down"]
    "---"
    ["Execute" Buffer-menu-execute
     :help "Save and/or delete buffers marked with s or k commands"]))

(define-derived-mode fancy-ffap-menu-mode tabulated-list-mode
  "fancy ffap menu mode")

;; https://gnu.org/
;; https://farside.link/
;; https://farside.link/
;; https://gnu.org/
;; https://farside.link/

(defun fancy-ffap-menu-mark-open-all ()
  "Open all URLs of entries marked with the open mark."
  (interactive nil fancy-ffap-menu-mode)
  (save-excursion
    (goto-char (point-min))
    (tabulated-list-set-col 0 "o" t)
    (while (not (eobp))
      (tabulated-list-set-col 0 "o" t)
      (forward-line 1))))

(defun fancy-ffap-menu-mark-open (&optional arg)
  "Mark entry or entries for opening.

If ARG is 0 or not provided, mark current entry and advance to the next entry.
If ARG is negative, mark current entry, and (ARG - 1) previous entries.
If ARG is positive, mark current entry, and (ARG - 1) next entries."
  (interactive "p" fancy-ffap-menu-mode)
  (when (or (null arg) (= arg 0))
    (setq arg 1))
  (while (< 0 arg)
    (tabulated-list-set-col 0 "o" t)
    (forward-line 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tabulated-list-set-col 0 "o" t)
    (forward-line -1)
    (setq arg (1+ arg))))

(defun fancy-ffap-menu-unmark-open (&optional arg)
  "Unmark URL for opening.

If ARG is 0 or not provided, unmark current entry and advance to the next entry.
If ARG is negative, unmark current entry and (ARG - 1) previous entries.
If ARG is positive, unmark current entry and (ARG - 1) next entries."
  (interactive "p" fancy-ffap-menu-mode)
  (when (or (null arg) (= arg 0))
    (setq arg 1))
  (while (< 0 arg)
    (tabulated-list-set-col 0 " " t)
    (forward-line 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (tabulated-list-set-col 0 " " t)
    (forward-line -1)
    (setq arg (1+ arg))))

(defcustom fancy-ffap-menu-use-header-line t
  "If non-nil, use the header line to display URLs Menu column titles."
  :type 'boolean
  :group 'fancy-ffap-menu-menu)

(defun fancy-ffap-menu-beginning ()
  "Move point to the first entry line."
  (goto-char (point-min))
  (unless fancy-ffap-menu-use-header-line
    (forward-line)))

(defun fancy-ffap-menu--marked-to-open-urls ()
  (save-excursion
    (goto-char (point-min))
    (let (urls)
      (fancy-ffap-menu-beginning)
      (while (re-search-forward "^o" nil t)))
    ))

(defun fancy-ffap-menu--get-marked-open-entries ()
  "Get a list of all entries marked for opening."
  (interactive nil fancy-ffap-menu-mode)
  (save-excursion
    (fancy-ffap-menu-beginning)
    (while (not (eobp))
      (let ((entry-id (tabulated-list-get-id))
            (entry-body (tabulated-list-get-entry)))
        (message "%s %s" entry-id entry-body)))))

(defun fancy-ffap-menu--get-marked-entries (mark)
  "Get a list of all entries marked with MARK."
  (interactive nil fancy-ffap-menu-mode)
  (let (entries)
    (dolist (entry tabulated-list-entries)
      (pcase entry
        (`(,_entry-id ["o" ,entry-url]) (push entry-url entries))
        (`(,_entry-id [,_entry-mark ,_entry-url]) '())
        (otherwise (error "Unknown type of entry: %s" otherwise))))
    (delete-dups entries)))

(defun fancy-ffap-menu-open-marked-entries ()
  "Open entries marked for opening with `browse-url'."
  (interactive nil fancy-ffap-menu-mode)
  (dolist (url (fancy-ffap-menu--get-marked-entries "o"))
    (browse-url url)))

(defun fancy-ffap-menu-list-urls ()
  "Display a list of current buffer's URLs.
The list is displayed in a buffer named \"*fancy ffap URLs list*\""
  (interactive)
  (display-buffer (fancy-ffap-menu-list-urls-noselect)))

(defun fancy-ffap-menu-url (&optional error-if-non-existent-p)
  "Return the URL described by the current Buffer Menu line.
If there is no URL here, return nil if ERROR-IF-NON-EXISTENT-P
is nil or omitted, and signal an error otherwise."
  (let ((url (tabulated-list-get-id)))
    (cond ((null url)
	   (if error-if-non-existent-p
	       (error "No URL on this line")))
	  (t url))))

(defun fancy-ffap-menu-marked-open-urls (&optional unmark)
  "Return the list of URLs marked with `fancy-ffap-menu-mark'.
If UNMARK is non-nil, unmark them."
  (let (urls)
    (fancy-ffap-menu-beginning)
    (while (re-search-forward "^o" nil t)
      (let ((buffer (fancy-ffap-menu-url)))
	(if (and buffer unmark)
	    (tabulated-list-set-col 0 " " t))
	(if (buffer-live-p buffer)
	    (push buffer buffers))))
    (nreverse buffers)))

(defun fancy-ffap-menu--refresh (&optional url-list)
  (let ((marked-buffers (fancy-ffap-menu-marked-open-urls))
	(filter-predicate (and (functionp fancy-ffap-menu-filter-predicate)
			       Buffer-menu-filter-predicate))
	entries
        name-width)
    (dolist (url (ffap-menu-rescan))
      (when (or url-list
		(and (or (not filter-predicate)
                         (funcall filter-predicate url))))
        (push (list url
                    (vector " "
                            url))
              entries)))
    (setq url-width (apply 'max
                           (mapcar (pcase-lambda ((entry-id [entry-mark entry-url]))
                                     entry-url)
                                   entries)))
    (setq tabulated-list-format
          (vector
           '("M" 1 t)
           '("URL" url-width t) ;; :pad-right 0
           ))
    (setq tabulated-list-use-header-line fancy-ffap-menu-use-header-line)
    (setq tabulated-list-entries (nreverse entries)))
  (tabulated-list-init-header))

(defun fancy-ffap-menu-list-urls-noselect ()
  "Create and return a Buffer Menu buffer.
This is called by `fancy-ffap-menu-list-urls' and others as a subroutine."
  (interactive nil fancy-ffap-menu-mode)
  (let ((old-buffer (current-buffer))
        (buffer (get-buffer-create "*fancy ffap URLs list*"))
        (urls (mapcar 'car (ffap-menu-rescan))))
    (with-current-buffer buffer
      (fancy-ffap-menu-mode)
      (setq-local tabulated-list-format
                  (vector
                   '("M" 1 t)
                   '("URL" 80 t) ;; :pad-right 0
                   ))
      (setq-local tabulated-list-use-header-line t)
      (setq-local tabulated-list-entries (mapcar (lambda (url) (list nil (vector " " url))) urls))
      (fancy-ffap-menu--refresh urls)
      (setq-local tabulated-list-use-header-line fancy-ffap-menu-use-header-line)
      (tabulated-list-print))
    buffer))

;;; fancy-ffap-menu.el ends here
