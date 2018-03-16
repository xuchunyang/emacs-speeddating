;;; speeddating.el --- Increasing or decreasing dates & times  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/emacs-speeddating
;; Package-Requires: ((emacs "24.3"))
;; Keywords: date time
;; Created: Thu, 15 Mar 2018 15:17:39 +0800
;; Version: 0

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

;; Increasing or decreasing dates and time at point.

;;; Code:

(require 'rx)
(require 'thingatpt)
(require 'cl-lib)

(defun speeddating--on-subexp-p (num)
  "Return t if the point is on the subexpression NUM."
  (and (>= (point) (match-beginning num))
       (<  (point) (match-end num))))

;;;###autoload
(defun speeddating-increase (inc)
  "Increase the date and time at point."
  (interactive "*p")
  ;; "%Y-%m-%d" 1999-12-31
  (if (thing-at-point-looking-at
       (rx (group (repeat 4 digit))
           "-"
           (group (repeat 2 digit))
           "-"
           (group (repeat 2 digit)))
       9)
      ;; (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF)
      (let ((time (parse-time-string (match-string 0)))
            (pt (point)))
        (setq time (mapcar (lambda (x) (if x x 0)) time))
        (cond ((speeddating--on-subexp-p 3)
               (cl-incf (nth 3 time) inc))
              ((speeddating--on-subexp-p 2)
               (cl-incf (nth 4 time) inc))
              ((speeddating--on-subexp-p 1)
               (cl-incf (nth 5 time) inc))
              ((speeddating--on-subexp-p 0)
               (user-error "Don't know how to increase/decrease, please move point"))
              (t (error "When pigs fly")))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (format-time-string "%Y-%m-%d" (apply #'encode-time time)))
        (goto-char pt))
    (user-error "No date and time at point or \
speeddating doesn't yet understand its format")))

;;;###autoload
(defun speeddating-decrease (dec)
  "Decrease the date and time at point."
  (interactive "*p")
  (speeddating-increase (- dec)))

(provide 'speeddating)
;;; speeddating.el ends here
