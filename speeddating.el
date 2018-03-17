;;; speeddating.el --- Increasing or decreasing dates & times  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Xu Chunyang

;; Author: Xu Chunyang <mail@xuchunyang.me>
;; Homepage: https://github.com/xuchunyang/emacs-speeddating
;; Package-Requires: ((emacs "25"))
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

;;;; Requirements

(require 'rx)
(require 'thingatpt)
(require 'cl-lib)
(require 'seq)

;;;; Custom options

(defgroup speeddating nil
  "Increasing or decreasing dates & times."
  :group 'convenience)

(defcustom speeddating-formats
  '("%a, %d %b %Y %H:%M:%S %z"          ; Email, "date --rfc-email"
    "%d %B %Y"
    "%Y-%m-%d"
    "%H:%M:%S")
  "The date & time formats list.
The format uses the same syntax as `format-time-string'."
  :type '(repeat (choice string))
  :group 'speeddating)

;;;; Internal variables and functions

(defvar speeddating--abbrev-weekdays
  '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar speeddating--full-weekdays
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(defvar speeddating--abbrev-months
  '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar speeddating--full-months
  '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

;; (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF)
;;   0    1     2    3    4    5    6   7     8


(defun speeddating--time-inc-sec    (time inc) (cl-incf (nth 0 time) inc))
(defun speeddating--time-inc-minute (time inc) (cl-incf (nth 1 time) inc))
(defun speeddating--time-inc-hour   (time inc) (cl-incf (nth 2 time) inc))
(defun speeddating--time-inc-day    (time inc) (cl-incf (nth 3 time) inc))
(defun speeddating--time-inc-month  (time inc) (cl-incf (nth 4 time) inc))
(defun speeddating--time-inc-year   (time inc) (cl-incf (nth 5 time) inc))

(defmacro speeddating--time-set-sec    (val) `(lambda (time string) (setf (nth 0 time) ,val)))
(defmacro speeddating--time-set-minute (val) `(lambda (time string) (setf (nth 1 time) ,val)))
(defmacro speeddating--time-set-hour   (val) `(lambda (time string) (setf (nth 2 time) ,val)))
(defmacro speeddating--time-set-day    (val) `(lambda (time string) (setf (nth 3 time) ,val)))
(defmacro speeddating--time-set-month  (val) `(lambda (time string) (setf (nth 4 time) ,val)))
(defmacro speeddating--time-set-year   (val) `(lambda (time string) (setf (nth 5 time) ,val)))
(defmacro speeddating--time-set-dow    (val) `(lambda (time string) (setf (nth 5 time) ,val)))

(defvar speeddating--format-spec
  (list
   (list "%a"
         :reg (regexp-opt speeddating--abbrev-weekdays t)
         :len 3
         :set (speeddating--time-set-dow (1+ (seq-position speeddating--abbrev-weekdays string)))
         :inc #'speeddating--time-inc-day)
   (list "%A"
         :reg (regexp-opt speeddating--full-weekdays t)
         :len 9
         :set (speeddating--time-set-dow (1+ (seq-position speeddating--full-weekdays string)))
         :inc #'speeddating--time-inc-day)
   (list "%b"
         :reg (regexp-opt speeddating--abbrev-months t)
         :len 3
         :set (speeddating--time-set-month (1+ (seq-position speeddating--abbrev-months string)))
         :inc #'speeddating--time-inc-month)
   (list "%B"
         :reg (regexp-opt speeddating--full-months t)
         :len 9
         :set (speeddating--time-set-month (1+ (seq-position speeddating--full-months string)))
         :inc #'speeddating--time-inc-month)
   (list "%Y"
         :reg (rx (group (repeat 4 digit)))
         :len 4
         :set (speeddating--time-set-year (string-to-number string))
         :inc #'speeddating--time-inc-year)
   (list "%m"
         :reg (rx (group (repeat 2 digit)))
         :len 2
         :set (speeddating--time-set-month (string-to-number string))
         :inc #'speeddating--time-inc-month)
   (list "%d"
         :reg (rx (group (repeat 2 digit)))
         :len 2
         :set (speeddating--time-set-day (string-to-number string))
         :inc #'speeddating--time-inc-day)
   (list "%H"
         :reg (rx (group (repeat 2 digit)))
         :len 2
         :set (speeddating--time-set-hour (string-to-number string))
         :inc #'speeddating--time-inc-hour)
   (list "%M"
         :reg (rx (group (repeat 2 digit)))
         :len 2
         :set (speeddating--time-set-minute (string-to-number string))
         :inc #'speeddating--time-inc-minute)
   (list "%S"
         :reg (rx (group (repeat 2 digit)))
         :len 2
         :set (speeddating--time-set-sec (string-to-number string))
         :inc #'speeddating--time-inc-sec)
   (list "%z"
         :reg (rx (group (or "-" "+") (repeat 4 digit)))
         :len 5
         :set (lambda (time string)
                (let ((sign (intern (substring string 0 1)))
                      (hour (string-to-number (substring string 1 3)))
                      (minute (string-to-number (substring string 3))))
                  (setf (nth 8 time) (funcall sign (+ (* hour 60 60) (* minute 60))))))
         :inc (lambda (_time _inc)
                (user-error
                 "Increasing or decreasing time zone is not yet supported"))))
  "List of (%-spec regexp length set inc).")

(defun speeddating--format-split (string)
  (let ((index 0)
        (end (length string))
        (list ()))
    (while (< index end)
      (if (and (= (aref string index) ?%)
               (< (1+ index) end))
          (progn
            (push (substring string index (+ index 2)) list)
            (cl-incf index 2))
        (push (string (aref string index)) list)
        (cl-incf index)))
    (nreverse list)))

;; (speeddating--format-split "%Y-%m-%d")
;;      => ("%Y" "-" "%m" "-" "%d")

(defun speeddating--format-to-regexp (string)
  (mapconcat
   (lambda (x)
     (if (= (length x) 2)
         (let ((plist (alist-get x speeddating--format-spec nil nil #'equal)))
           (if plist (plist-get plist :reg) (error "Unsupported format %s" x)))
       (regexp-quote x)))
   (speeddating--format-split string) ""))

;; (speeddating--format-to-regexp "%Y-%m-%d")
;;      => "\\([[:digit:]]\\{4\\}\\)-\\([[:digit:]]\\{2\\}\\)-\\([[:digit:]]\\{2\\}\\)"

(defun speeddating--format-length (string)
  (apply
   #'+
   (mapcar
    (lambda (x)
      (if (= (length x) 2)
          (let ((plist (alist-get x speeddating--format-spec nil nil #'equal)))
            (if plist (plist-get plist :len) (error "Unsupported format %s" x)))
        1))
    (speeddating--format-split string))))

;; (speeddating--format-length "%Y-%m-%d")
;;      => 10

(defun speeddating--format-to-list (string)
  (seq-filter
   (lambda (x)
     (when (= (length x) 2)
       (if (alist-get x speeddating--format-spec nil nil #'equal)
           t
         (error "Unsupported %s" x))))
   (speeddating--format-split string)))

;; (speeddating--format-to-list "%Y-%m-%d")
;;      => ("%Y" "%m" "%d")

(defun speeddating--time-normalize (time)
  (pcase-let ((`(,_sec0 ,_minute0 ,_hour0 ,day0 ,month0 ,year0 ,dow0 ,_dst0 ,utcoff0) (decode-time))
              (`(,sec ,minute ,hour ,day ,month ,year ,dow ,dst ,utcoff) time))
    (setq sec (or sec 0)
          minute (or minute 0)
          hour (or hour 0)
          utcoff (or utcoff utcoff0))
    (setq month (or month month0)
          year (or year year0))
    (if (null dow)
        (setq day (or day day0))
      (setq day (or day (+ day0 (- dow dow0)))))
    (list sec minute hour day month year dow dst utcoff)))

(defun speeddating--format-get-time (string)
  (when (thing-at-point-looking-at
         (speeddating--format-to-regexp string)
         (1- (speeddating--format-length string)))
    (let ((time (list nil nil nil nil nil nil nil nil nil)))
      (seq-do-indexed
       (lambda (x index)
         (let ((plist (alist-get x speeddating--format-spec nil nil #'equal)))
           (funcall (plist-get plist :set) time (match-string (1+ index)))))
       (speeddating--format-to-list string))
      ;; (message "Matched raw time: %s" time)
      ;; Normalize time
      (setq time (speeddating--time-normalize time))
      ;; (message "Normalized time: %s" time)
      time)))

(defun speeddating--format-inc-time (string inc)
  (let ((time (speeddating--format-get-time string)))
    (when time
      (let ((list (speeddating--format-to-list string))
            (group 1)
            (found nil))
        (while (and list (null found))
          (when (speeddating--on-subexp-p group)
            (setq found (car list)))
          (pop list)
          (cl-incf group))
        (if found
            (let ((plist (alist-get found speeddating--format-spec nil nil #'equal)))
              (funcall (plist-get plist :inc) time inc)
              (speeddating--format-replace-time string time))
          (user-error "Don't know which field to increase or decrease, try to move point"))))))

(defun speeddating--format-replace-time (string time)
  (let ((new (format-time-string string (apply #'encode-time time)))
        (old-point (point)))
    (delete-region (match-beginning 0) (match-end 0))
    (insert new)
    (goto-char old-point)))

(defun speeddating--on-subexp-p (num)
  "Return t if the point is on the subexpression NUM."
  (and (>= (point) (match-beginning num))
       (<  (point) (match-end num))))

;;;; User commands

;;;###autoload
(defun speeddating-increase (inc)
  "Increase the date and time at point by INC."
  (interactive "*p")
  (let ((formats (copy-sequence speeddating-formats))
        (found nil))
    (while (and formats (null found))
      (when (speeddating--format-inc-time (pop formats) inc)
        (setq found t)))
    (unless found
      (user-error
       "No date and time at point or speeddating doesn't yet understand its format"))))

;;;###autoload
(defun speeddating-decrease (dec)
  "Decrease the date and time at point by DEC."
  (interactive "*p")
  (speeddating-increase (- dec)))

(provide 'speeddating)
;;; speeddating.el ends here
