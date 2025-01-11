;;; lpa-layer.el --- Layer -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lpa-util)

(declare-function lpa--ensure-list "lpa-util")

(defun lpa--define-bindings (keymap bindings)
  "Define bindings"
  (dolist (binding bindings)
    (let* ((keys (lpa--ensure-list (car binding)))
           (def (cdr binding)))
      (message "--------------def: %s" def)
      (dolist (key keys)
	(define-key keymap (kbd key) def)))))

(provide 'lpa-bind)
;;; lpa-bind.el ends here
