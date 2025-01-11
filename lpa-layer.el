;;; lpa-layer.el --- Layer -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'lpa-var)
(require 'lpa-bind)

(declare-function lpa-define-bindings "lpa-bind")

(defun lpa-define-layer (layer-symbol &rest props)
  "Define layer"
  (let* ((name (plist-get props :name))
	 (bindings (plist-get props :bind))
	 (suppress (plist-get props :suppress))
         (keymap (make-keymap))
	 (layer-props (plist-put (copy-sequence props) :keymap keymap))
	 (layer-entry (assoc layer-symbol lpa--layers)))  ; Use sparse keymap for efficiency
    (when suppress
      (suppress-keymap keymap t))
    
    (lpa--define-bindings keymap bindings)

    (if layer-entry
	(setcdr layer-entry layer-props)
      (push (cons layer-symbol layer-props) lpa--layers ))
    layer-props))

(defun lpa-switch-layer (layer-symbol)
  "Switch to the layer specified by LAYER-SYMBOL."
  (interactive
   (list (intern (completing-read "Switch to layer: "
                                  (mapcar #'car lpa--layers)))))
  (when lpa-mode
    (let ((keymap (lpa-get-layer-keymap layer-symbol)))
      (setq lpa--active-layer layer-symbol)
      (force-mode-line-update)
      (use-local-map keymap)
      (message "Switched to %s" layer-symbol))))

(defun lpa-get-layer-keymap (layer-symbol)
  "Get layer keymap"
  (let ((layer-entry (assoc layer-symbol lpa--layers)))
    (if (not layer-entry)
	(error "Layer %s not found" layer-symbol)
      (plist-get (cdr layer-entry) :keymap))))

(provide 'lpa-layer)
;;; lpa-layer.el ends here
