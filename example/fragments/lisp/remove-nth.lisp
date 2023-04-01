;; remove-nth


(defun first-n (list n)
  "removes the nth element from a list"
  (let (
	(n (cond
	     ((> n (length list)) (length list))
	     (t n)))
	)
    (loop for i from 0 to (1- n) collect
				 (nth i list))))

;; \/\/\/\/\/

(defun remove-nth (list n)
  "removes the nth element from a list"
  (append (first-n list (1- n)) (nthcdr n list)))

;; /\/\/\/\/\
