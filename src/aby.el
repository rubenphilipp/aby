;;; aby.el --- Aby provides fragment orchestration capacities.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ruben Philipp

;; Author: Ruben Philipp <rubenphilipp@MBP-von-Ruben.fritz.box>
;; Keywords: 

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

;; Aby is a simple fragment/snippet manager for Emacs. It lets you easily
;; insert the contents of files from a designated snippets directory in an Emacs
;; buffer.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ----------------------------------------------------------------------------
;;; HELPER FUNCTIONS
;;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* aby/eval-file
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-01
;;; 
;;; DESCRIPTION
;;; Loads the content of an .el file into a temporary buffer, evaluates the
;;; code and returns the result of the last expression.
;;;
;;; ARGUMENTS
;;; The path to the file to be evaluated.
;;; 
;;; RETURN VALUE
;;; The result of the last expression.
;;; 
;;; SYNOPSIS
(defun eval-file (file)
  ;;; ****
  (eval
   (ignore-errors
     (read-from-whole-string
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* aby/aby-replace-from-list-in-string
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-01
;;; 
;;; DESCRIPTION
;;; Successively replace contents of a string from an input list of lists
;;; of the following format:
;;; ((regex1 replacement1)
;;;  (regex2 replacement2)
;;;  ;; ...
;;;  (regexn replacementn))
;;;
;;; ARGUMENTS
;;; - The input string.
;;; - The list of replacement values (see above).
;;; 
;;; RETURN VALUE
;;; The resulting string after applying the replacement rules.
;;;
;;; EXAMPLE

[
(aby-replace-from-list-in-string "Hello beautiful world"
                                 '(("Hello" "Salut,")
                                   ("[aeiou]" "-")))

;; => "S-l-t, b---t-f-l w-rld"
]

;;; SYNOPSIS
(defun aby-replace-from-list-in-string (string replacements)
  ;;; ****
  (cl-loop for rep in replacements
           do
           (setf string (replace-regexp-in-string (nth 0 rep)
                                                  (nth 1 rep)
                                                  string))
           finally return string))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ----------------------------------------------------------------------------
;;; CONFIGURATION
;;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom aby-fragments-dir "~/fragments"
  "The source directory of the Aby fragments")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ----------------------------------------------------------------------------
;;; CORE
;;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun aby-insert ()
  (interactive)
  (message "hallo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'aby)
;;; aby.el ends here
