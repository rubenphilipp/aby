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

(let* ((fragment "{{fragment-file}}")
       (static-rep '(
                     ;; (var-n regex-n value-n comment-n)
                     ))
       (ask-rep '(
                  ;; (var-n regex-n prompt-n comment-n)
                  ))
       (dynamic-rep '(
                      ;; (var-n regex-n function-n comment-n)
                      )))
  (aby-instruct fragment
                static-rep
                ask-rep
                dynamic-rep))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF {{file-name}}

