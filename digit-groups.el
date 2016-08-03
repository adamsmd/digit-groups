;;; digit-groups -- Make it easier to read large numbers by highlighting digits at specified place-value positions (e.g., thousand, million, billion, etc.)

;; Copyright (C) 2016 Michael D. Adams

;; Author: Michael D. Adams <http://michaeldadams.org>
;; URL: http://bitbucket.com/adamsmd/digit-groups
;; License: MIT
;; Version: 0.1
;; Package-Version: 0.1
;; Package-Requires: ((dash "2.12.1"))

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Commentary:
;; See documentation for the `digit-groups` customization group.

;;; Code:

(require 'dash)

(defvar digit-groups-old-mode-hooks nil)
;;;### autoload
(defun digit-groups--set-mode-hooks (symbol value)
  "Set digit-group-mode-hooks (which SYMBOL must be) to VALUE."
  (set-default symbol value)
  (--map (remove-hook it 'digit-groups-enable)
         digit-groups-old-mode-hooks)
  (setq digit-groups-old-mode-hooks value)
  (--map (add-hook it 'digit-groups-enable) value))

;;; BEGIN CUSTOM

(defgroup digit-groups nil
  "Make it easier to read large numbers by highlighting digits at selected place-value positions (e.g., thousand, million, billion, etc.).

For example, in the text `9876543210.123456789`, the default
configuration would make bold the 3, 6, and 9 before the
decimal (`.`) because they are in the thousand's, million's, and
billion's positions as well as the 3, 6, and 9 after the
decimal (`.`) because they are in the thousandth's, millionth's,
and billionth's positions.

In order to work, requires that font-lock-mode be enabled.  To
enable `digit-groups`, customize `digit-groups-mode-hooks` to
include the mode hooks for the modes on which you want it
enabled.  To enable for all modes, customize
`digit-groups-mode-hooks` to `'(text-mode-hook prog-mode-hook
special-mode-hook).` To enable for just the current buffer call
`digit-groups-enable`.

The default configuration highlights every place-value position
for 10^i when i is a multiple of 3 between 3 and 60 or -3 and
-60 (inclusive).  This can be changed by customizing
`digit-groups-groups`.

Changes to the configuration may require you to reload any
affected buffers."
  :group 'font-lock
  :package-version '(digit-groups . "0.1"))

(defcustom digit-groups-mode-hooks nil
  "The mode hooks for the modes in which to highlight digit groups.  To enable for everything, set to '(text-mode-hook prog-mode-hook special-mode-hook)."
  :type '(repeat symbol)
  :group 'digit-groups
  :set 'digit-groups--set-mode-hooks)

(defface digit-groups-default-face '((t (:weight bold)))
  "Default face for highlighting digit groups."
  :group 'digit-groups)

(defcustom digit-groups-groups
  (--map (cons it ''digit-groups-default-face)
         (append
          (--iterate (+ it 3)  3 20)
          (--iterate (- it 3) -3 20)))
  "Positions of digits to highlight and the face with which to highlight them.
Use 0 for the one's digit, 1 for the ten's digit, 2 for the
hundred's digit, etc.  Use -1 for the tenth's digit, -2 for the
hundredth's digit, -3 for the thousandth's digit, etc."
  :type '(alist :key-type integer :value-type face)
  :group 'digit-groups)

(defcustom digit-groups-decimal-separator "\\."
  "Separator between integral and factional parts of a number.  Common values include `\\\\.`, `,`, and `\\\\.\\|,`."
  :type '(regexp)
  :group 'digit-groups)

(defcustom digit-groups-digits "[:digit:]"
  "What characters count as a digit.  Will be place inside character-class brackets.  Must not start with `^`."
  :type '(string)
  :group 'digit-groups)

;;; END CUSTOM

(defun digit-groups--repeat-string (n s)
  "Concatenate N copies of S."
  (cond
   ((= 0 n) "")
   (t (concat s (digit-groups--repeat-string (- n 1) s)))))

(defun digit-groups--add-digits (n old)
  "Add a group of size N to OLD.
If N is non-negative, add a pre-decimal group.  Otherwise, add a post-decimal group."
  (let ((highlighted (concat "\\(" "[" digit-groups-digits "]" "\\)"))
        (non-highlighted (digit-groups--repeat-string (- (abs n) 1) (concat "[" digit-groups-digits "]"))))
    (let ((group
           (if (>= n 0)
               (concat highlighted non-highlighted)
             (concat non-highlighted highlighted))))
      (if (string-equal "" old)
          group
        (concat "\\(?:" old "\\)?" group)))))

(defun digit-groups--make-group-regexp (positions)
  "Make a regexp to highlight characters at POSITIONS."
  (cond
   ((null positions)
    (error "Internal error in digit-groups: null argument to digit-groups--make-group-regexp"))
   ((null (cdr positions)) "")
   (t
    (digit-groups--add-digits
     (- (car (cdr positions)) (car positions))
     (digit-groups--make-group-regexp (cdr positions))))))

;;;### autoload
(defun digit-groups-enable ()
  "Add font-lock keywords to highlight digit groups in the current buffer."
  (interactive)
  (let ((faces
         (--map-indexed
          (list (+ 1 it-index) (cdr it) 'prepend t)
          (--sort (> (car it) (car other))
                  digit-groups-groups)))
        (positions (mapcar 'car digit-groups-groups)))
    (let ((pos-positions (cons -1 (-sort '< (--filter (>= it 0) positions))))
          (neg-positions (cons  0 (-sort '> (--filter (<  it 0) positions)))))
      (let ((pos-regexp (digit-groups--make-group-regexp pos-positions))
            (neg-regexp (digit-groups--make-group-regexp neg-positions))
            (decimal (concat "\\(?:" digit-groups-decimal-separator "\\)" )))
        (let ((regexp
               (concat
                pos-regexp
                "\\(?:" decimal neg-regexp "[" digit-groups-digits "]*" "\\)?"
                "\\(?:" "[^" digit-groups-digits "]" "\\|" "\\'" "\\)"
                )))
          (font-lock-add-keywords nil (list (cons regexp faces))))))))

(provide 'digit-groups)
;;; digit-groups ends here
