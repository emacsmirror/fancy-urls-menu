;;; fancy-ffap-menu.el --- Interface for viewing and choosing URLs to open. -*- lexical-binding: t -*-

;; Copyright (C) 1985-2024 Free Software Foundation, Inc.

;; Maintainer: emacs-devel@gnu.org
;; Keywords: convenience
;; Package: emacs

;; This file is part of GNU Emacs.

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

(define-derived-mode fancy-ffap-menu-mode tabulated-list-mode
  "fancy ffap menu mode")

;; https://gnu.org/
;; https://farside.link/
;; https://farside.link/
;; https://gnu.org/
;; https://farside.link/

(defvar-keymap fancy-ffap-menu-mode-map
  :doc "Local keymap for `fancy-ffap-menu-mode' buffers."
  :parent tabulated-list-mode-map
  "o" #'fancy-ffap-menu-mark-open
  "a" #'fancy-ffap-menu-mark-open-all
  "x" #'fancy-ffap-menu-open-marked-urls
  "u" #'fancy-ffap-menu-unmark
  "U" #'fancy-ffap-menu-unmark-all
  )

(defun fancy-ffap-menu-mark-open-all ()
  (interactive nil fancy-ffap-menu-mode)
  (save-excursion
    (goto-char (point-min))
    (tabulated-list-set-col 0 "o" t)
    (while (not (eobp))
      (tabulated-list-set-col 0 "o" t)
      (forward-line 1))))

(defun fancy-ffap-menu-mark-open (&optional arg)
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

(defun fancy-ffap-menu-unmark (&optional arg)
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

(defun fancy-ffap-menu--get-marked-entries ()
  (interactive nil fancy-ffap-menu-mode)
  (save-excursion
    (fancy-ffap-menu-beginning)
    (while (not (eobp))
      (let ((entry-id (tabulated-list-get-id))
            (entry-body (tabulated-list-get-entry)))
        (message "%s %s" entry-id entry-body)))))

(defun fancy-ffap-menu--get-marked-urls (mark)
  (interactive nil fancy-ffap-menu-mode)
  (let (entries)
    (dolist (entry tabulated-list-entries)
      (pcase entry
        (`(,entry-id ["o" ,entry-url]) (push entry-url entries))
        (`(,entry-id [,entry-mark ,entry-url]) '())
        (otherwise (error "Unknown type of entry: %s" otherwise))))
    (delete-dups entries)))

(defun fancy-ffap-menu-open-marked-urls ()
  (interactive nil fancy-ffap-menu-mode)
  (dolist (url (fancy-ffap-menu--get-marked-urls "o"))
    (browse-url url)))

(defun fancy-ffap-menu-list-urls ()
  (interactive)
  (let ((old-buffer (current-buffer))
        (buffer (get-buffer-create "moo"))
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
      (tabulated-list-print))))

;;; fancy-ffap-menu.el ends here
