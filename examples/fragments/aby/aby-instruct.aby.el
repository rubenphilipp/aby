;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; aby-instruct.aby.el
;;;
;;; NAME
;;; aby-instruct
;;;
;;; DESCRIPTION
;;; This file inserts a template for the definition of aby-instruct statements.
;;; Recursion alert... ;-]
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-13
;;;
;;; $$ Last modified:  23:54:14 Thu Apr 13 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((fragment "aby-instruct.el")
       (dynamic-rep '((file-name
                       "{{file-name}}"
                       '(lambda (reps)
                          (let ((filename (alist-get 'buffer-file-name
                                                     reps)))
                            ;; insert the file-name from the replacements
                            ;; variable. when NIL (e.g. when the buffer
                            ;; is a scratch-buffer, insert nothing
                            (if (null filename)
                                ""
                              (file-name-nondirectory filename)))))))
       (ask-rep '((fragment-file
                   "{{fragment-file}}"
                   "Fragment file: ")))
       (static-rep '()))
  (aby-instruct fragment
                static-rep
                ask-rep
                dynamic-rep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF aby-instruct.aby.el
