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
;;; $$ Last modified:  16:39:06 Sun Apr  2 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq aby-fragments-dir "/Users/rubenphilipp/code/aby/example/fragments/")

(setq aby-auto-replacements
      `((author "{{author}}" ,+rp-document-author+)
        (author-email "{{author-email}}" ,(concat "<"
                                   +rp-document-author-mail+
                                   ">"))
        (created "{{created}}" ,(format-time-string "%Y-%m-%d"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF aby-config.el
