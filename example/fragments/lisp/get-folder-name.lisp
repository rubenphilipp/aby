;; extract the name of a folder for a given path (e.g. from uiop:subdirectories)

(setf thepath "/Users/rubenphilipp/Music/01_Entre-Nous/2_samples/0_piano/0_pitched/frequencies/440/")


(defun getdirname (dir)
  "extract the name of a folder for a given path"
  (let (
	(search1 (search "/" dir :from-end t))
	(dirlen (length dir))
	)
    (cond
      ((equal search1 (1- dirlen)) (subseq (subseq dir 0 search1) (1+ (search "/" (subseq dir 0 search1) :from-end t))))
      (t (subseq dir (1+ search1)))
      )
    )
  )


;; ----

(getdirname thepath) => "440"
