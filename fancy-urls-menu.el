;;; fancy-urls-menu.el --- Interface for viewing and opening URLs in current buffer -*- lexical-binding: t -*-

;; Copyright (C) 2024  Yuval Langer

;; Maintainer: yuval.langer@gmail.com
;; Keywords: convenience
;; Package-Requires: ((emacs "29.1"))
;; Version: 0.1.1
;; Homepage: https://codeberg.org/kakafarm/emacs-fancy-urls-menu/

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

;; Run the `fancy-urls-menu-list-urls' command to get the URLs of the current
;; buffer.  The Fancy URLs Menu provides a way of finding URLs in the
;; current buffer and opening them in a browser (Emacs EWW, links2, or
;; something terrible like Firefox).  It is hopefully an improvement
;; to the ffap-menu of the FFAP package (Find File At Point!  Look it
;; up, it's legit and not at all a cheeky joke!) and uses the
;; ffap-menu-rescan at its core.  It derives from
;; `Tabulated-list-mode', so it is similar in user experience to
;; `list-buffers', `list-processes', and `list-packages', as all three
;; derive from `Tabulated-list-mode'.

;;; Code:

(require 'ffap)
(require 'tabulated-list)

(defvar-local fancy-urls-menu-filter-predicate nil
  "Function to filter out URLs in the URL list.
URLs that don't satisfy the predicate will be skipped.
The value should be a function of one argument; it will be
called with the URL.  If this function returns non-nil,
then the URL will be displayed in the URL list.")

(defvar-local fancy-urls-menu-url-list nil
  "The current list of URLs or function to return URLs.")

(defvar-keymap fancy-urls-menu-mode-map
  :doc "Local keymap for `fancy-urls-menu-mode' buffers."
  :parent tabulated-list-mode-map
  "o" #'fancy-urls-menu-mark
  "a" #'fancy-urls-menu-mark-all
  "x" #'fancy-urls-menu-open-marked-entries
  "u" #'fancy-urls-menu-unmark
  "U" #'fancy-urls-menu-unmark-all)

(easy-menu-define fancy-urls-menu-mode-menu fancy-urls-menu-mode-map
  "Menu for `fancy-urls-menu-mode' buffers."
  '("fancy-urls-menu"
    ["Mark URL" fancy-urls-menu-mark
     :help "Mark URL for browsing with `fancy-urls-menu-browser'"]
    ["Mark all URLs" fancy-urls-menu-mark-all
     :help "Mark all URLs for browsing with `fancy-urls-menu-browser'."]
    "---"
    ["Unmark" fancy-urls-menu-unmark
     :help "Unmark a URL for browsing."]
    ["Unmark all URLs" fancy-urls-menu-unmark-all
     :help "Unmark all URLs for browsing."]
    "---"
    ["Execute" Buffer-menu-execute
     :help "Open all marked URLs with `fancy-urls-menu-browser'."]))

(define-derived-mode fancy-urls-menu-mode tabulated-list-mode
  "fancy URLs menu mode")

(defun fancy-urls-menu-mark-all ()
  "Mark all entries for opening."
  (interactive nil fancy-urls-menu-mode)
  (save-excursion
    (fancy-urls-menu-beginning)
    (while (not (eobp))
      (tabulated-list-set-col 0 "o" t)
      (forward-line 1))))

(defun fancy-urls-menu-unmark-all ()
  "Mark all entries for opening."
  (interactive nil fancy-urls-menu-mode)
  (save-excursion
    (fancy-urls-menu-beginning)
    (while (not (eobp))
      (tabulated-list-set-col 0 " " t)
      (forward-line 1))))

(defun fancy-urls-menu-mark (&optional arg)
  "Mark entry or entries for opening.

If ARG is 0 or not provided, mark current entry and advance to the next entry.
If ARG is negative, mark current entry, and (ARG - 1) previous entries.
If ARG is positive, mark current entry, and (ARG - 1) next entries."
  (interactive "p" fancy-urls-menu-mode)
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

(defun fancy-urls-menu-unmark (&optional arg)
  "Unmark URL for opening.

If ARG is 0 or not provided, unmark current entry and advance to the next entry.
If ARG is negative, unmark current entry and (ARG - 1) previous entries.
If ARG is positive, unmark current entry and (ARG - 1) next entries."
  (interactive "p" fancy-urls-menu-mode)
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

(defcustom fancy-urls-menu-browser #'browse-url
  "Function used to open a URL."
  :type 'function
  :group 'fancy-urls-menu-menu)

(defcustom fancy-urls-menu-use-header-line t
  "If non-nil, use the header line to display URLs Menu column titles."
  :type 'boolean
  :group 'fancy-urls-menu-menu)

(defun fancy-urls-menu-beginning ()
  "Move point to the first entry line."
  (goto-char (point-min))
  (unless fancy-urls-menu-use-header-line
    (forward-line)))

(defun fancy-urls-menu-open-marked-entries ()
  "Open entries marked for opening with `fancy-urls-menu-browser'."
  (interactive nil fancy-urls-menu-mode)
  (dolist (url (fancy-urls-menu-marked-urls))
    (apply fancy-urls-menu-browser (list url))))

;;;###autoload
(defun fancy-urls-menu-list-urls ()
  "Display a list of current buffer's URLs.
The list is displayed in a buffer named \"*fancy ffap URLs list*\""
  (interactive)
  (display-buffer (fancy-urls-menu-list-urls-noselect)))

(defun fancy-urls-menu-url (&optional error-if-non-existent-p)
  "Return the URL described by the current URL Menu line.
If there is no URL here, return nil if ERROR-IF-NON-EXISTENT-P
is nil or omitted, and signal an error otherwise."
  (let ((url (tabulated-list-get-id)))
    (cond ((null url)
           (if error-if-non-existent-p
               (error "No URL on this line")))
          (t url))))

(defun fancy-urls-menu-marked-urls (&optional unmark)
  "Return the list of URLs marked with `fancy-urls-menu-mark'.
If UNMARK is non-nil, unmark them."
  (let (urls)
    (fancy-urls-menu-beginning)
    (while (re-search-forward "^o" nil t)
      (let ((url (fancy-urls-menu-url)))
        (when (and url unmark)
          (tabulated-list-set-col 0 " " t))
        (push url urls)))
    (nreverse urls)))

(defun fancy-urls-menu--entry-to-url (entry)
  "Return the URL of ENTRY."
  (pcase entry
    (`(,_entry-id [,_entry-mark ,entry-url]) entry-url)))

(defun fancy-urls-menu--refresh (&optional url-list)
  "Refresh URLs table.

When URL-LIST is provided, it must be either a list of URLs or a
function that returns a list of URLs."
  (let ((marked-urls (fancy-urls-menu-marked-urls))
        (filter-predicate (and (functionp fancy-urls-menu-filter-predicate)
                               fancy-urls-menu-filter-predicate))
        entries
        url-width)
    (dolist (url (cond
                  ((functionp url-list)
                   (funcall url-list))
                  (url-list)
                  ((functionp fancy-urls-menu-url-list)
                   (funcall fancy-urls-menu-url-list))
                  (fancy-urls-menu-url-list)))
      (when (or (null filter-predicate)
                (funcall filter-predicate url))
        (push (list url
                    (vector (cond
                             ((member url marked-urls) "o")
                             (t " "))
                            url))
              entries)))
    (setq url-width (apply #'max
                           0
                           (mapcar (lambda (entry)
                                     (length (fancy-urls-menu--entry-to-url entry)))
                                   entries)))
    (setq tabulated-list-format
          (vector
           '("M" 1 t)
           `("URL" ,url-width t) ;; :pad-right 0
           ))
    (setq tabulated-list-use-header-line fancy-urls-menu-use-header-line)
    (setq tabulated-list-entries (nreverse entries)))
  (tabulated-list-init-header))

(defun fancy-urls-menu--ffap-menu-rescan-urls ()
  "Return all URLs in current buffer.

Returned list includes duplicates."
  (mapcar #'car (ffap-menu-rescan)))

(defun fancy-urls-menu-list-urls-noselect (&optional url-list filter-predicate)
  "Create and return a Buffer Menu buffer.
This is called by `fancy-urls-menu-list-urls' and others as a subroutine.

If URL-LIST is non-nil, it should be either a list of URLs or a
function that returns a list of URLs; it means list those URLs
and no others.  See more at `fancy-urls-menu-url-list'.

If FILTER-PREDICATE is non-nil, it should be a function that
filters out URLs from the list of URLs.  See more at
`fancy-urls-menu-filter-predicate'."
  (let ((buffer (get-buffer-create "*Fancy FFAP URLs list*"))
        (url-list (or url-list
                      (fancy-urls-menu--ffap-menu-rescan-urls))))
    (with-current-buffer buffer
      (fancy-urls-menu-mode)
      (setq fancy-urls-menu-url-list url-list)
      (setq fancy-urls-menu-filter-predicate filter-predicate)
      (fancy-urls-menu--refresh url-list)
      (tabulated-list-print))
    buffer))

(provide 'fancy-urls-menu)

;;; fancy-urls-menu.el ends here
