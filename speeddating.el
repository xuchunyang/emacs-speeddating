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

(defcustom speeddating-formats '("%Y-%m-%d"
                                 "%H:%M:%S")
  "The date & time formats list.
The format uses the same syntax as `format-time-string'."
  :type '(repeat (choice string))
  :group 'speeddating)

;;;; Internal functions

(defun speeddating--format-string-split (string)
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

;; (speeddating--format-string-split "%Y-%m-%d")
;;      => ("%Y" "-" "%m" "-" "%d")

(defun speeddating--format-string-to-regexp (string)
  (let ((4-digits (rx (group (repeat 4 digit))))
        (2-digits (rx (group (repeat 2 digit)))))
    (mapconcat
     (lambda (x)
       (if (= (length x) 2)
           (pcase x
             ("%Y" 4-digits)
             ("%m" 2-digits)
             ("%d" 2-digits)
             ("%H" 2-digits)
             ("%M" 2-digits)
             ("%S" 2-digits)
             (_    (error "Unsupported %s" x)))
         (regexp-quote x)))
     (speeddating--format-string-split string) "")))

;; (speeddating--format-string-to-regexp "%Y-%m-%d")
;;      => "\\([[:digit:]]\\{4\\}\\)-\\([[:digit:]]\\{2\\}\\)-\\([[:digit:]]\\{2\\}\\)"

(defun speeddating--format-string-length (string)
  (apply
   #'+
   (mapcar
    (lambda (x)
      (if (= (length x) 2)
          (pcase x
            ("%Y" 4)
            ("%m" 2)
            ("%d" 2)
            ("%H" 2)
            ("%M" 2)
            ("%S" 2)
            (_    (error "Unsupported %s" x)))
        1))
    (speeddating--format-string-split string))))

;; (speeddating--format-string-length "%Y-%m-%d")
;;      => 10

(defun speeddating--format-string-to-list (string)
  (delq nil
        (mapcar
         (lambda (x)
           (when (= (length x) 2)
             (pcase x
               ("%Y" 'year)
               ("%m" 'month)
               ("%d" 'day)
               ("%H" 'hour)
               ("%M" 'minute)
               ("%S" 'sec)
               (_    (error "Unsupported %s" x)))))
         (speeddating--format-string-split string))))

;; (speeddating--format-string-to-list "%Y-%m-%d")
;;      => (year month day)

(defun speeddating--format-string-get-time (string)
  (when (thing-at-point-looking-at
         (speeddating--format-string-to-regexp string)
         (1- (speeddating--format-string-length string)))
    (let ((now (decode-time)))
      (seq-let (sec minute hour day month year dow dst utcoff) now
        (seq-do-indexed
         (lambda (elt index)
           ;; Not working under Lexical binding
           ;; (set elt (string-to-number (match-string (1+ index))))
           (let ((val (string-to-number (match-string (1+ index)))))
             (pcase elt
               ;; Use backquote instead of regular quote here for compatibility
               ;; with Emacs 24.5.
               (`sec    (setq sec    val))
               (`minute (setq minute val))
               (`hour   (setq hour   val))
               (`day    (setq day    val))
               (`month  (setq month  val))
               (`year   (setq year   val))
               (`dow    (setq dow    val))
               (`dst    (setq dst    val))
               (`utcoff (setq utcoff val)))))
         (speeddating--format-string-to-list string))
        (list sec minute hour day month year dow dst utcoff)))))

(defun speeddating--format-string-inc-time (string inc)
  (let ((time (speeddating--format-string-get-time string)))
    (when time
      (seq-let (sec minute hour day month year dow dst utcoff) time
        (let ((list (speeddating--format-string-to-list string))
              (group 1)
              (found nil))
          (while (and list (null found))
            (when (speeddating--on-subexp-p group)
              (setq found (car list)))
            (pop list)
            (cl-incf group))
          (if found
              ;; Not working under Lexical binding
              ;; (set found (1+ (eval found)))
              (pcase found
                ;; Use backquote instead of regular quote here for compatibility
                ;; with Emacs 24.5.
                (`sec    (cl-incf sec    inc))
                (`minute (cl-incf minute inc))
                (`hour   (cl-incf hour   inc))
                (`day    (cl-incf day    inc))
                (`month  (cl-incf month  inc))
                (`year   (cl-incf year   inc))
                (`dow    (cl-incf dow    inc))
                (`dst    (cl-incf dst    inc))
                (`utcoff (cl-incf utcoff inc)))
            (user-error (concat "Don't know which field to increase or decrease, "
                                "try to move point"))))
        (speeddating--format-string-replace-time
         string
         (list sec minute hour day month year dow dst utcoff))))))

(defun speeddating--format-string-replace-time (string time)
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
      (when (speeddating--format-string-inc-time (pop formats) inc)
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
