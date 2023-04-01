;; Count files in a folder

(pathname-type "testfolder/notes-short_6.wav") ;; gets the file extension

(setf thecwd (uiop/os:getcwd)) ; get current working directory

(namestring thecwd)

;; get files in the folder
(uiop:directory-files (concatenate 'string (namestring (uiop/os:getcwd)) "testfolder/"))

(uiop:directory-files "/Users/rubenphilipp/code/snippets/lisp/testfolder/")
