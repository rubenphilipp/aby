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
;;; $$ Last modified:  12:22:26 Thu Apr 20 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; must end with a slash!!
(setq aby-fragments-dir "/Users/mnemosyne/code/aby/example/fragments/")

;;; These are replacements which will be performed
;;; automatically in every fragment (both in instruction-file-based
;;; fragments and plain-fragments)
(setq aby-auto-replacements
      `((author "{{author}}" "Mnemosyne")
        (author-email "{{author-email}}" "mnemosyne@nil.org")
        (created "{{created}}" ,(format-time-string "%Y-%m-%d"))))

;;; This is the default file extension for aby instruction-files
(setq aby-file-extension "aby.el")

;;; This is a RegEx indicating Aby plain-fragments
(setq aby-plain-fragment-indicator ".abyss\\.?+\\..+$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF aby-config.el
