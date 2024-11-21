;;; fancy-ffap-menu.el --- Interface for viewing and choosing URLs to open -*- lexical-binding: t -*-

;; Copyright (C) 2024  Yuval Langer

;; Maintainer: yuval.langer@gmail.com
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.0
;; Homepage: https://codeberg.org/kakafarm/emacs-fancy-ffap-menu/

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

(require 'ffap)
(require 'tabulated-list)

(defvar-local fancy-ffap-menu-filter-predicate nil
  "Function to filter out URLs in the URL list.
URLs that don't satisfy the predicate will be skipped.
The value should be a function of one argument; it will be
called with the URL.  If this function returns non-nil,
then the URL will be displayed in the URL list.")

(defvar-local fancy-ffap-menu-url-list nil
  "The current list of URLs or function to return URLs.")

(defvar-keymap fancy-ffap-menu-mode-map
  :doc "Local keymap for `fancy-ffap-menu-mode' buffers."
  :parent tabulated-list-mode-map
  "o" #'fancy-ffap-menu-mark
  "a" #'fancy-ffap-menu-mark-all
  "x" #'fancy-ffap-menu-open-marked-entries
  "u" #'fancy-ffap-menu-unmark
  "U" #'fancy-ffap-menu-unmark-all)

(easy-menu-define fancy-ffap-menu-mode-menu fancy-ffap-menu-mode-map
  "Menu for `fancy-ffap-menu-mode' buffers."
  '("fancy-ffap-menu"
    ["Mark URL" fancy-ffap-menu-mark
     :help "Mark URL for browsing with `fancy-ffap-menu-browser'"]
    ["Mark all URLs" fancy-ffap-menu-mark-all
     :help "Mark all URLs for browsing with `fancy-ffap-menu-browser'."]
    "---"
    ["Unmark" fancy-ffap-menu-unmark
     :help "Unmark a URL for browsing."]
    ["Unmark all URLs" fancy-ffap-menu-unmark-all
     :help "Unmark all URLs for browsing."]
    "---"
    ["Execute" Buffer-menu-execute
     :help "Open all marked URLs with `fancy-ffap-menu-browser'."]))

(define-derived-mode fancy-ffap-menu-mode tabulated-list-mode
  "fancy ffap menu mode")

(defun fancy-ffap-menu-mark-all ()
  "Mark all entries for opening."
  (interactive nil fancy-ffap-menu-mode)
  (save-excursion
    (fancy-ffap-menu-beginning)
    (while (not (eobp))
      (tabulated-list-set-col 0 "o" t)
      (forward-line 1))))

(defun fancy-ffap-menu-unmark-all ()
  "Mark all entries for opening."
  (interactive nil fancy-ffap-menu-mode)
  (save-excursion
    (fancy-ffap-menu-beginning)
    (while (not (eobp))
      (tabulated-list-set-col 0 " " t)
      (forward-line 1))))

(defun fancy-ffap-menu-mark (&optional arg)
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

(defun fancy-ffap-menu-unmark (&optional arg)
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

(defun fancy-ffap-menu-open-marked-entries ()
  "Open entries marked for opening with `browse-url'."
  (interactive nil fancy-ffap-menu-mode)
  (dolist (url (fancy-ffap-menu-marked-urls))
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

(defun fancy-ffap-menu-marked-urls (&optional unmark)
  "Return the list of URLs marked with `fancy-ffap-menu-mark'.
If UNMARK is non-nil, unmark them."
  (let (urls)
    (fancy-ffap-menu-beginning)
    (while (re-search-forward "^o" nil t)
      (let ((url (fancy-ffap-menu-url)))
        (when (and url unmark)
          (tabulated-list-set-col 0 " " t))
        (push url urls)))
    (nreverse urls)))

(defun fancy-ffap-menu--entry-to-url (entry)
  "Return the URL of ENTRY."
  (pcase entry
    (`(,_entry-id [,_entry-mark ,entry-url]) entry-url)))

(defun fancy-ffap-menu--refresh (&optional url-list)
  "Refresh URLs table.

When URL-LIST is provided, it must be either a list of URLs or a
function that returns a list of URLs."
  (let ((marked-urls (fancy-ffap-menu-marked-urls))
        (filter-predicate (and (functionp fancy-ffap-menu-filter-predicate)
                               fancy-ffap-menu-filter-predicate))
        entries
        url-width)
    (dolist (url (cond
                  ((functionp url-list)
                   (funcall url-list))
                  (url-list)
                  ((functionp fancy-ffap-menu-url-list)
                   (funcall fancy-ffap-menu-url-list))
                  (fancy-ffap-menu-url-list)))
      (when (or (null filter-predicate)
                (funcall filter-predicate url))
        (push (list url
                    (vector (cond
                             ((member url marked-urls) "o")
                             (t " "))
                            url))
              entries)))
    (setq url-width (apply 'max
                           0
                           (mapcar (lambda (entry)
                                     (length (fancy-ffap-menu--entry-to-url entry)))
                                   entries)))
    (setq tabulated-list-format
          (vector
           '("M" 1 t)
           `("URL" ,url-width t) ;; :pad-right 0
           ))
    (setq tabulated-list-use-header-line fancy-ffap-menu-use-header-line)
    (setq tabulated-list-entries (nreverse entries)))
  (tabulated-list-init-header))

(defun fancy-ffap-menu--ffap-menu-rescan-urls ()
  "Return all URLs in current buffer.

Returned list includes duplicates."
  (mapcar 'car (ffap-menu-rescan)))

(defun fancy-ffap-menu-list-urls-noselect (&optional url-list filter-predicate)
  "Create and return a Buffer Menu buffer.
This is called by `fancy-ffap-menu-list-urls' and others as a subroutine.

If URL-LIST is non-nil, it should be either a list of URLs or a
function that returns a list of URLs; it means list those URLs
and no others.  See more at `fancy-ffap-menu-url-list'.

If FILTER-PREDICATE is non-nil, it should be a function that
filters out URLs from the list of URLs.  See more at
`fancy-ffap-menu-filter-predicate'."
  (let ((buffer (get-buffer-create "*Fancy FFAP URLs list*"))
        (url-list (or url-list
                      (fancy-ffap-menu--ffap-menu-rescan-urls))))
    (with-current-buffer buffer
      (fancy-ffap-menu-mode)
      (setq fancy-ffap-menu-url-list url-list)
      (setq fancy-ffap-menu-filter-predicate filter-predicate)
      (fancy-ffap-menu--refresh url-list)
      (tabulated-list-print))
    buffer))

(ert-deftest fancy-ffap-menu-list-urls-noselect-test-url-list-empty ()
  (with-temp-buffer
    (let ((fancy-buffer (fancy-ffap-menu-list-urls-noselect '())))
      (with-current-buffer fancy-buffer
        (fancy-ffap-menu-beginning)
        (fancy-ffap-menu-mark-all)
        (should (equal '()
                       (fancy-ffap-menu-marked-urls)))))))

(ert-deftest fancy-ffap-menu-list-urls-noselect-test-url-list-only-gnu ()
  (with-temp-buffer
    (let ((fancy-buffer (fancy-ffap-menu-list-urls-noselect '("https://gnu.org/"))))
      (with-current-buffer fancy-buffer
        (fancy-ffap-menu-beginning)
        (fancy-ffap-menu-mark-all)
        (should (equal '("https://gnu.org/")
                       (fancy-ffap-menu-marked-urls)))))))

(ert-deftest fancy-ffap-menu-list-urls-noselect-test-empty ()
  (with-temp-buffer
    (let ((fancy-buffer (fancy-ffap-menu-list-urls-noselect)))
      (with-current-buffer fancy-buffer
        (fancy-ffap-menu-beginning)
        (fancy-ffap-menu-mark-all)
        (should (equal '()
                       (fancy-ffap-menu-marked-urls)))))))

(ert-deftest fancy-ffap-menu-list-urls-noselect-test-some-urls ()
  (with-temp-buffer
    (insert "
;; https://gnu.org/
;; https://farside.link/
;; https://farside.link/
;; https://gnu.org/
;; https://farside.link/
")
    (let ((fancy-buffer (fancy-ffap-menu-list-urls-noselect)))
      (with-current-buffer fancy-buffer
        (fancy-ffap-menu-beginning)
        (fancy-ffap-menu-mark-all)
        (should (equal '("https://gnu.org/" "https://farside.link/")
                       (fancy-ffap-menu-marked-urls)))))))

(provide 'fancy-ffap-menu)

;;; fancy-ffap-menu.el ends here
