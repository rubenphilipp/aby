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
;;; $$ Last modified:  23:18:56 Sun Apr  2 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* (;; The file containing the template data
       ;; relative to this instruction-file.
       (fragment "robodoc-class.lisp")
       ;; A list with static-replacements (cf. doc).
       ;; This is just a demonstration and litarally
       ;; does nothing as there is no occurence in
       ;; robodoc-class.lisp matching the regex
       ;; "{{test}}".
       (static-rep '((test1 "{{test}}" "nothing")))
       ;; A list with ask-replacements.
       ;; The third element is the prompt string.
       ;; For more information cf. the doc for
       ;; aby-instruct.
       (ask-rep '((class-name "{{class-name}}" "Class name: ")))
       ;; A list with dynamic replacements
       ;; the function must take the assoc list with
       ;; the replacements as first argument. (cf. doc)
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
