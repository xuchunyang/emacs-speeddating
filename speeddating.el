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

(defcustom speeddating-formats '("%d %B %Y"
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

(defvar speeddating--format-spec
  (list
   (list "%a"
         :regexp (regexp-opt speeddating--abbrev-weekdays t)
         :length 3
         :setter (lambda (time string)
                   (setf (nth 6 time) (1+ (seq-position speeddating--abbrev-weekdays string))))
         :update (lambda (time inc) (cl-incf (nth 3 time) inc)))
   (list "%A"
         :regexp (regexp-opt speeddating--full-weekdays t)
         :length 9
         :setter (lambda (time string)
                   (setf (nth 6 time) (1+ (seq-position speeddating--full-weekdays string))))
         :update (lambda (time inc) (cl-incf (nth 3 time) inc)))
   (list "%b"
         :regexp (regexp-opt speeddating--abbrev-months t)
         :length 3
         :setter (lambda (time string)
                   (setf (nth 4 time) (1+ (seq-position speeddating--abbrev-months string))))
         :update (lambda (time inc) (cl-incf (nth 4 time) inc)))
   (list "%B"
         :regexp (regexp-opt speeddating--full-months t)
         :length 9
         :setter (lambda (time string)
                   (setf (nth 4 time) (1+ (seq-position speeddating--full-months string))))
         :update (lambda (time inc) (cl-incf (nth 4 time) inc)))
   (list "%Y"
         :regexp (rx (group (repeat 4 digit)))
         :length 4
         :setter (lambda (time string)
                   (setf (nth 5 time) (string-to-number string)))
         :update (lambda (time inc) (cl-incf (nth 5 time) inc)))
   (list "%m"
         :regexp (rx (group (repeat 2 digit)))
         :length 2
         :setter (lambda (time string)
                   (setf (nth 4 time) (string-to-number string)))
         :update (lambda (time inc) (cl-incf (nth 4 time) inc)))
   (list "%d"
         :regexp (rx (group (repeat 2 digit)))
         :length 2
         :setter (lambda (time string)
                   (setf (nth 3 time) (string-to-number string)))
         :update (lambda (time inc) (cl-incf (nth 3 time) inc)))
   (list "%H"
         :regexp (rx (group (repeat 2 digit)))
         :length 2
         :setter (lambda (time string)
                   (setf (nth 2 time) (string-to-number string)))
         :update (lambda (time inc) (cl-incf (nth 1 time) inc)))
   (list "%M"
         :regexp (rx (group (repeat 2 digit)))
         :length 2
         :setter (lambda (time string)
                   (setf (nth 1 time) (string-to-number string)))
         :update (lambda (time inc) (cl-incf (nth 2 time) inc)))
   (list "%S"
         :regexp (rx (group (repeat 2 digit)))
         :length 2
         :setter (lambda (time string)
                   (setf (nth 0 time) (string-to-number string)))
         :update (lambda (time inc) (cl-incf (nth 0 time) inc))))
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
           (if plist (plist-get plist :regexp) (error "Unsupported format %s" x)))
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
            (if plist (plist-get plist :length) (error "Unsupported format %s" x)))
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
           (funcall (plist-get plist :setter) time (match-string (1+ index)))))
       (speeddating--format-to-list string))
      (message "Matched raw time: %s" time)
      ;; Normalize time
      (setq time (speeddating--time-normalize time))
      (message "Normalized time: %s" time)
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
              (funcall (plist-get plist :update) time inc)
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
