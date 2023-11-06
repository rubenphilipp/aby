;;; aby.el --- Aby provides fragment orchestration capacities.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Ruben Philipp

;; Author: Ruben Philipp <me@rubenphilipp.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Aby is a simple fragment/snippet manager for Emacs. It lets you easily
;; insert the contents of files from a designated snippets directory into an
;; Emacs buffer.

;;; Code:

(require 'aby)

;;;###autoload
(define-minor-mode aby-mode
  "Toggle aby-mode."
  :lighter "aby"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c i i") 'aby-insert)
            map)
  ;; startup / teardown
  ;; nothing here, yet
  ;; RP  Fri Jul  7 00:23:55 2023
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'aby-mode)

;;; aby-mode.el ends here
