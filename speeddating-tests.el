;;; speeddating-tests.el --- Tests for speeddating.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>

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

;; Tests

;;; Code:

(require 'ert)
(require 'speeddating)

(ert-deftest ISO-8601 ()
  (with-temp-buffer
    (insert "1999-12-31")
    (backward-char 1)
    (speeddating-increase 1)
    (should (string= (buffer-string) "2000-01-01")))
  (with-temp-buffer
    (insert "2018-03-18T19:39:13+08:00")
    (goto-char 6)
    (speeddating-increase 10)
    (should (string= (buffer-string) "2019-01-18T19:39:13+08:00"))))

(ert-deftest monday ()
  (with-temp-buffer
    (insert "Monday")
    (backward-char 1)
    (speeddating-increase 1)
    (should (string= (buffer-string) "Tuesday"))))

(provide 'speeddating-tests)
;;; speeddating-tests.el ends here
