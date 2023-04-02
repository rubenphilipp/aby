;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; robodoc-class.el
;;;
;;; DESCRIPTION
;;; An example Aby instruction-file.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-02
;;;
;;; $$ Last modified:  16:35:27 Sun Apr  2 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* (;; the file containing the template data
       ;; relative to this instruction-file
       (fragment "robodoc-class.lisp")
       ;; a list with static-replacements (cf. doc)
       (static-rep '((test1 "{{test}}" "nichts")))
       ;; a list with ask-replacements
       (ask-rep '((class-name "{{class-name}}")))
       ;; a list with dynamic replacements
       ;; the function must take the assoc list with
       ;; the replacements as first argument (cf. doc)
       (dynamic-rep
        '((class-short "{{class-name-short}}"
                       '(lambda (reps)
                          (car
                           (last
                            (split-string 
                             (alist-get 'class-name
                                        reps)
                             "/"))))))))
  ;; this constructs the instruction association list
  ;; and must be the return value of this file
  (aby-instruct fragment
                static-rep
                ask-rep
                dynamic-rep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF
