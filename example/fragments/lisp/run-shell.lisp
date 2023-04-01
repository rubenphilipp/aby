;; +------------------------------+
;; | run shell commands from lisp |
;; +------------------------------+


;; synchronous

(uiop:run-program "pwd" :output *standard-output*)

(setf pwd (uiop:run-program "pwd" :output :string))

;; Remove newline

(setf pwd (remove #\Newline pwd))

;; arithmetics

(setf theformula "2.4 + sqrt(5)")

(uiop:run-program (concatenate 'string "bc <<< 'scale=4; " theformula "'") :output :string)

(uiop:run-program "bc <<< 'scale=4; 2.3 + sqrt(6)'" :output :string)

;; pwd without newline

(setf pwd (remove #\Newline (setf pwd (uiop:run-program "pwd" :output :string))))

(uiop:run-program "bc <<< '2.4 + 2'" :output :string)


;; asynchronous

(setf *program1* (uiop:launch-program "bash" :input :stream :output :stream))

(write-line "print" (uiop:process-info-input *program1*))



(uiop:process-alive-p *program1*)

(uiop:close-streams *program1*)
