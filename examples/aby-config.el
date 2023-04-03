;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; aby-config.el
;;;
;;; DESCRIPTION
;;; This file contains the configuration for the Aby package. I.e. mainly the
;;; location of the fragments directory.
;;; NB: It is necessary that the Aby package is available.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-04-01
;;;
;;; $$ Last modified:  09:32:03 Mon Apr  3 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; must end with a slash!!
(setq aby-fragments-dir "/Users/mnemosyne/code/aby/example/fragments/")

(setq aby-auto-replacements
      `((author "{{author}}" "Mnemosyne")
        (author-email "{{author-email}}" "mnemosyne@nil.org"
        (created "{{created}}" ,(format-time-string "%Y-%m-%d"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF aby-config.el
