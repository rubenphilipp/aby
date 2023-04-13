;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; {{file-name}}
;;; 
;;; DESCRIPTION
;;; 
;;;
;;; AUTHOR
;;; {{author}}
;;;
;;; CREATED
;;; {{created}}
;;;
;;; $$ Last modified: 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* (;; the file containing the template data
       ;; relative to this instruction-file
       (fragment "{{fragment-file}}")
       ;; a list with static-replacements (cf. doc)
       (static-rep '())
       ;; a list with ask-replacements
       (ask-rep '(;; the first element is the symbol for the variable
                  ;; the second element is the replacement-regex
                  ;; the third element is the prompt (optional)
                  ;; the fourth element is a documentation comment (optional)
                  ))
       ;; a list with dynamic replacements
       ;; the function must take the assoc list with
       ;; the replacements as first argument (cf. doc)
       (dynamic-rep '())
  ;; this constructs the instruction association list
  ;; and must be the return value of this file
  (aby-instruct fragment
                static-rep
                ask-rep
                dynamic-rep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF {{file-name}}

