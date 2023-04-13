;;; aby.el --- Aby provides fragment orchestration capacities.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ruben Philipp

;; Author: Ruben Philipp <me@rubenphilipp.com>
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
;; insert the contents of files from a designated snippets directory into an
;; Emacs buffer.

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
(defun aby-eval-file (file)
  ;;; ****
  (eval
   (ignore-errors
     (read-from-whole-string
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ----------------------------------------------------------------------------
;;; CONFIGURATION
;;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* aby/aby-fragments-dir 
;;; DESCRIPTION
;;; The source directory of the Aby fragments.
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom aby-fragments-dir "~/fragments/"
  "The source directory of the Aby fragments")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* aby/aby-auto-replacements
;;; DESCRIPTION
;;; A list of replacements with the default replacement rules.
;;; Must be of the following form:
;;; ((var1 regex1 replacement1 comment1)
;;;  (var2 regex2 replacement2 comment2)
;;;  ;; ...
;;;  (varn regexn replacementn commentn))
;;; NB: The comment serves documentation purposes and is optional.
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom aby-auto-replacements nil
  "A list of lists with default replacement rules. Aby will replace the
   regex in the first element of the list with the content of the second
   element.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* aby/aby-file-extension
;;; DESCRIPTION
;;; The default extension for Aby instruction files. Default: "aby.el". 
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom aby-file-extension "aby.el"
  "The default extension for Aby instruction files.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ----------------------------------------------------------------------------
;;; CORE
;;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* aby/aby-get-instruction-files
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-02
;;; 
;;; DESCRIPTION
;;; This function reads the contents of the fragment directory and returns
;;; a list of the relative filenames.
;;; This is, obviously, necessary for auto-completion. 
;;; 
;;; RETURN VALUE
;;; A list with the relative filename paths (without file extension).
;;; 
;;; SYNOPSIS
(defun aby-get-instruction-files ()
  ;;; ****
  (let ((files (directory-files-recursively aby-fragments-dir
                                            aby-file-extension))
        (rel-dir (expand-file-name aby-fragments-dir)))
    (mapcar #'(lambda (f)
                (file-relative-name
                 ;; removed as file-names can contain more than one
                 ;; dot
                 ;; RP  Sun Apr  2 22:58:57 2023
                 ;;(file-name-sans-extension f)
                 (replace-regexp-in-string (concat "."
                                                   aby-file-extension
                                                   "$")
                                           "" f)
                 rel-dir))
            ;; remove emacs backup files
            ;; RP  Mon Apr  3 01:09:36 2023
            (mapcar #'(lambda (f)
                        (if (string= "~" (substring f -1 nil))
                            ""
                          f))
                    files))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* aby/aby-get-absolute-instruction-file-path
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-02
;;; 
;;; DESCRIPTION
;;; Return the full file path of single Aby instruction file, e.g. retreived via
;;; aby-get-instruction-files.
;;;
;;; ARGUMENTS
;;; The relative path to an Aby instruction-file.
;;; 
;;; RETURN VALUE
;;; A string with the full path of the instruction file.
;;; 
;;; SYNOPSIS
(defun aby-get-absolute-instruction-file-path (filename)
  ;;; ****
  (concat aby-fragments-dir
                    filename
                    "."
                    aby-file-extension))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* aby/aby-instruct
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-02
;;; 
;;; DESCRIPTION
;;; This function is used in Aby instruction files to generate the
;;; replacements list which will then be parsed by aby-insert.
;;; It takes the replacements and instructions for preparing the fragments
;;; in the aby-insert function. These are mainly replacement rules (either
;;; interactively, by prompting for completions or static values). 
;;;
;;; ARGUMENTS
;;; The name of the fragment file (relative to the instruction file).
;;;
;;; OPTIONAL ARGUMENTS
;;; - static-rep. A list of three-item-lists with the static, i.e. "hard coded"
;;;   replacements. The list must take the form:
;;;   ((var1 regex1 value1 comment1)
;;;    (var2 regex2 value2 comment2)
;;;    ;; ...
;;;    (varn regexn valuen commentn))
;;;   The first element must be a symbol which serves as the index for the
;;;   association list which is parsed during the aby-insert prodecure. This
;;;   enables accessing the value via a dynamic-rep.
;;;   The comment (string) serves documentation purposes and is optional. 
;;;   Default= NIL.
;;; - ask-rep. A list of two-item-lists with the regexs to interactively
;;;   replace. The regexs will be interactively replaced with values
;;;   given by the user by prompting. The list must take the form:
;;;   ((var1 regex1 prompt1 comment1)
;;;    (var2 regex2 prompt2 comment2)
;;;    ;; ...
;;;    (varn regexn promptn commentn))
;;;   The prompt must be a string which will be printed in the Minibuffer
;;;   during the read-string query. If it is NIL, the regex will be displayed.
;;;   The comment (string) serves documentation purposes and is optional.
;;;   Default = NIL.
;;; - dynamic-rep. A list of three-item-lists of the form:
;;;   ((var1 regex1 function1 comment1)
;;;    (var2 regex2 function2 comment2)
;;;    ;; ...
;;;    (varn regexn functionn commentn))
;;;   The function must be either a lambda or existing function that takes
;;;   the replacements association list as first argument and returns the
;;;   replacement value as a string.
;;;   The comment (string) serves documentation purposes and is optional.
;;;   NB: aby-insert automatically adds some elements to the replacements
;;;       association list. These are relevant values which are not directly
;;;       available during the execution of the code in the instruction file
;;;       and are thus passed to the the dynamic-replacement function via the
;;;       replacement list. The following keys will be set:
;;;       - 'aby-fragment-directory. This is the file-name-directory of the
;;;         instruction file. This is very useful when loading additional
;;;         fragment files in the ask-rep lambda function.
;;;         See examples/org/lilypond-src.el for an example.
;;;       - 'buffer-file-name. This is the (buffer-file-name) of the buffer
;;;         visited during insert (i.e. the buffer, the resulting string will
;;;         be pasted in). 
;;;   Default = NIL.
;;; NB 1: All var items in the *-rep arguments (i.e. the car) must be unique!
;;; NB 2: When the regex is NIL, no replacements will take place. Instead,
;;;       the value from the list respectively the prompt (in case of ask-rep)
;;;       will be stored in the replacement list. This comes in handy, e.g.
;;;       when dynamically adding certain parts to the output fragment
;;;       depending on previously made decisions via a prompt (cf. the
;;;       org/lilypond-src example).
;;; - omit-replacements. A boolean indicating whether to omit the
;;;   replacements from the aby-auto-replacements. Default = NIL.
;;; 
;;; RETURN VALUE
;;; An association list with the replacements rules.
;;;
;;; SYNOPSIS
(defun aby-instruct (fragment-file
                     &optional
                     static-rep
                     ask-rep
                     dynamic-rep
                     omit-replacements)
  ;;; ****
  ;; perform tests
  (unless (and (listp static-rep)
               (listp ask-rep)
               (listp dynamic-rep))
    (error "aby-instruct: The replacement rules must be of type list."))
  (unless (booleanp omit-replacements)
    (error "aby-instruct: The omit-replacements argument must be of type ~
            boolean."))
  (unless (stringp fragment-file)
    (error "aby-instruct: The fragment-file must be of type string."))
  ;; test if var fields are unique
  (cl-loop with lsts = (append static-rep
                               ask-rep
                               dynamic-rep)
           with vars = (mapcar #'car lsts)
           for var in vars
           for i from 1
           for subl = (nthcdr i vars)
           when (member var subl)
           do
           (error "aby-instruct: Malformed arguments. There is a duplicate ~
                   variable."))
  ;; construct replacements rules list
  (let ((rep-rules (list
                    (cons 'fragment-file fragment-file)
                    (cons 'static-rep static-rep)
                    (cons 'ask-rep ask-rep)
                    (cons 'dynamic-rep dynamic-rep)
                    (cons 'omit-replacements omit-replacements)
                    (cons 'type 'aby-rep-rules))))
    rep-rules))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* aby/aby-insert
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-02
;;; 
;;; DESCRIPTION
;;; This is the core function of Aby. It prompts for a fragment file (in the
;;; fragments directory) and then performs the replacement and insertion
;;; process, as defined in the instruction-file.
;;; The function scans the fragments directory for all .el files, evaluates
;;; the selected file and parses the data returnd from an replacement rules
;;; association list (cf. aby-instruct).
;;; The replacement process order is as follows:
;;; - static replacements
;;; - ask-replacements (interactive, cf. aby-instruct)
;;; - dynamic-replacements
;;; - auto-replacements
;;; IMPORTANT: As this function evaluates code, make sure that all .el files
;;; in your fragments directory are meticulously scrutinized. Otherwise, Aby
;;; might be able to do some harm to your Emacs.
;;;
;;; ARGUMENTS
;;; A string spedifying the Aby fragment to open from the fragments directory
;;; (optional). When NIL, Aby prompts for a file.
;;; 
;;; RETURN VALUE
;;; Either T, if the operation succeeded, or NIL, when something failed.
;;; 
;;; SYNOPSIS
(defun aby-insert (&rest fragment)
  ;;; ****
  (interactive)
  ;; check if aby-fragments-dir ends with a "/"
  (unless (string= (substring aby-fragments-dir -1 nil)
                   "/")
    ;; add the trailing slash
    (setf aby-fragments-dir (concat aby-fragments-dir "/")))
  (let* ((fragments (aby-get-instruction-files))
         (fragment (if (car fragment)
                       (car fragment)
                     (completing-read "Which fragment should I add? "
                                      fragments)))
         ;; instruction file
         (fragment-i-file-path
          (aby-get-absolute-instruction-file-path fragment))
         (fragment-i-file fragment-i-file-path)
         (rep-rules (aby-eval-file fragment-i-file)))
    ;; test if fragment file is non-nil
    (when (string= "" fragment)
      (error "Please choose a fragment."))
    ;; test if return value is an aby-list
    (unless (eql (alist-get 'type rep-rules) 'aby-rep-rules)
      (error "aby-insert: The returned list is not of type 'aby-rep-rules."))
    (let ((rep-list '())
          (fragment-data (file-to-string
                          (concat
                           (file-name-directory fragment-i-file-path)
                           (alist-get 'fragment-file rep-rules)))))
      ;; add file-name-directory to rep-rules list
      (setf (alist-get 'aby-fragment-directory rep-list)
            (file-name-directory fragment-i-file-path))
      ;; add the buffer-file-name to rep-rules list
      ;; RP  Sun Apr  9 18:09:09 2023
      (setf (alist-get 'buffer-file-name rep-list)
            (buffer-file-name))
      ;; perform replacements
      ;; static replacements
      (cl-loop for rep in (alist-get 'static-rep rep-rules)
               for varn = (nth 0 rep)
               for regex = (nth 1 rep)
               for replacement = (nth 2 rep)
               for comment = (nth 3 rep)
               do
               (setf (alist-get varn rep-list) replacement)
               ;; just do something when regex is non-NIL
               (when regex
                 (setf fragment-data
                       (replace-regexp-in-string regex
                                                 replacement
                                                 fragment-data))))
      ;; ask replacements
      (cl-loop for rep in (alist-get 'ask-rep rep-rules)
               for varn = (nth 0 rep)
               for regex = (nth 1 rep)
               for prompt = (nth 2 rep)
               for comment = (nth 3 rep)
               do
               (let* ((prompt (if prompt
                                  prompt
                                (concat "Replace " regex " with: ")))
                      (replacement (read-string prompt)))
                 (setf (alist-get varn rep-list) replacement)
                 ;; just do something when regex is non-NIL
                 (when regex
                   (setf fragment-data
                         (replace-regexp-in-string regex
                                                   replacement
                                                   fragment-data)))))
      ;; dynamic replacements
      (cl-loop for rep in (alist-get 'dynamic-rep rep-rules)
               for varn = (nth 0 rep)
               for regex = (nth 1 rep)
               for rep-fun = (cadr (nth 2 rep))
               for comment = (nth 3 rep)
               do
               (let ((replacement
                      ;; call replacement function
                      (funcall rep-fun rep-list)))
                 (setf (alist-get varn rep-list) replacement)
                 ;; just do something when regex is non-NIL
                 (when regex
                   (setf fragment-data
                         (replace-regexp-in-string regex
                                                   replacement
                                                   fragment-data)))))
      ;; auto-replacements
      ;; just perform if auto-replacements are desired
      ;; (see 'omit-replacements in aby-instruct)
      (unless (alist-get 'omit-replacements rep-rules)
        (cl-loop for rep in aby-auto-replacements
                 for varn = (nth 0 rep)
                 for regex = (nth 1 rep)
                 for replacement = (nth 2 rep)
                 for comment = (nth 3 rep)
                 do
                 (setf (alist-get varn rep-list) replacement)
                 ;; just do something when regex is non-NIL
                 (when regex
                   (setf fragment-data
                         (replace-regexp-in-string regex
                                                   replacement
                                                   fragment-data)))))
      ;; INSERT REPLACEMENTS
      (insert fragment-data))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'aby)
;;; aby.el ends here
