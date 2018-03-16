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

;; (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF)
(defmacro speeddating--sec    (time) `(nth 0 ,time))
(defmacro speeddating--minute (time) `(nth 1 ,time))
(defmacro speeddating--hour   (time) `(nth 2 ,time))
(defmacro speeddating--day    (time) `(nth 3 ,time))
(defmacro speeddating--month  (time) `(nth 4 ,time))
(defmacro speeddating--year   (time) `(nth 5 ,time))
(defmacro speeddating--dow    (time) `(nth 6 ,time))
(defmacro speeddating--dst    (time) `(nth 7 ,time))
(defmacro speeddating--utcoff (time) `(nth 8 ,time))

;;;###autoload
(defun speeddating-increase (inc)
  "Increase the date and time at point."
  (interactive "*p")
  (let* ((4-digits (rx (group (repeat 4 digit))))
         (2-digits (rx (group (repeat 2 digit))))
         (yyyy 4-digits)
         (MM   2-digits)
         (dd   2-digits)
         (HH   2-digits)
         (mm   2-digits)
         (ss   2-digits))
    (cond
     ;; %Y-%m-%d yyyy-MM-dd 1999-12-31
     ((thing-at-point-looking-at (format "%s-%s-%s" yyyy MM dd) (1- (length "yyyy-MM-dd")))
      (let ((time (decode-time)))
        (setf (speeddating--year time) (string-to-number (match-string 1))
              (speeddating--month time) (string-to-number (match-string 2))
              (speeddating--day time) (string-to-number (match-string 3)))
        (cond ((speeddating--on-subexp-p 1)
               (cl-incf (speeddating--year time) inc))
              ((speeddating--on-subexp-p 2)
               (cl-incf (speeddating--month time) inc))
              ((speeddating--on-subexp-p 3)
               (cl-incf (speeddating--day time) inc))
              ((speeddating--on-subexp-p 0)
               (user-error "Don't know how to increase/decrease, please move point"))
              (t (error "When pigs fly")))
        (let ((old-point (point)))
          (delete-region (match-beginning 0) (match-end 0))
          (insert (format-time-string "%Y-%m-%d" (apply #'encode-time time)))
          (goto-char old-point))))
     ;; %H:%M:%S HH:mm:ss 23:02:59
     ((thing-at-point-looking-at (format "%s:%s:%s" HH mm ss) (1- (length "HH:mm:ss")))
      (let ((time (decode-time)))
        (setf (speeddating--hour time) (string-to-number (match-string 1))
              (speeddating--minute time) (string-to-number (match-string 2))
              (speeddating--sec time) (string-to-number (match-string 3)))
        (cond ((speeddating--on-subexp-p 1)
               (cl-incf (speeddating--hour time) inc))
              ((speeddating--on-subexp-p 2)
               (cl-incf (speeddating--minute time) inc))
              ((speeddating--on-subexp-p 3)
               (cl-incf (speeddating--sec time) inc))
              ((speeddating--on-subexp-p 0)
               (user-error "Don't know how to increase/decrease, please move point"))
              (t (error "When pigs fly")))
        (let ((old-point (point)))
          (delete-region (match-beginning 0) (match-end 0))
          (insert (format-time-string "%H:%M:%S" (apply #'encode-time time)))
          (goto-char old-point))))
     (user-error "No date and time at point or \
speeddating doesn't yet understand its format"))))

;;;###autoload
(defun speeddating-decrease (dec)
  "Decrease the date and time at point."
  (interactive "*p")
  (speeddating-increase (- dec)))

(provide 'speeddating)
;;; speeddating.el ends here
