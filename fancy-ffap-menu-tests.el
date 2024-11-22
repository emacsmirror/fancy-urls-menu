;;; fancy-ffap-menu-tests.el --- Interface for viewing and choosing URLs to open -*- lexical-binding: t -*-

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

;;; Code:

(require 'ert)
(require 'fancy-ffap-menu)

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

(provide 'fancy-ffap-menu-tests)

;;; fancy-ffap-menu-tests.el ends here
