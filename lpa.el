;;; lpa.el --- A flexible package for layer switching and key binding in Emacs -*- lexical-binding: t -*-

;;; Commentary:
;; This package enables creating layers, configuring layer properties, and binding
;; shortcuts with specific methods in each layer. Layers can be toggled using the
;; `lpa-switch-layer` function and the entire lpa-mode functionality can be
;; toggled on and off within the definition of the `lpa-mode` minor mode. A
;; default layer can be set using the `lpa-default-layer` variable.

;;; Code:

(require 'cl-lib)

(defvar lpa-default-layer nil
  "Default layer to switch to if no layer is specified.")

(defvar lpa-after-layer-switch-hook nil
  "Hook run after switching layers.")

(defvar lpa-layer-configurations nil
  "List of defined layer configurations.")

(defvar lpa--active-layer-symbol nil
  "Current active layer symbol.")

(defvar lpa--layer-keymaps nil
  "Alist storing keymaps for each layer.")

(defvar lpa-inhibit-self-insert-keys '([(32 . 126)])
  "Inhibit char items, [(start . end)] range, (\"SPC\" \"RET\") list") ;; (where-is-internal 'self-insert-command)


(defun lpa--ensure-list (arg)
  "Ensure the input ARG is a list. If it's not a list, convert it to a list containing the argument."
  (if (listp arg)
      arg
    (list arg)))

(defun lpa-undefined-key (key)
  (interactive)
  (message "%s key undefined" key))

(defun lpa--define-key (keymap binding)
  "Define key to keymap"

  (let* ((keys (lpa--ensure-list (car binding)))
	 (def (cdr binding)))
    
    (dolist (key keys)
      (if (and (not (functionp def))
	       (listp def))
	  (let ((sub-keymap (make-sparse-keymap))
		(sub-bindings def))
	    (progn (lpa--define-key-bindings sub-keymap
					     sub-bindings)
		   (define-key keymap
			       (kbd key)
			       sub-keymap)))
	(define-key keymap
		    (kbd key)
		    def)))))

(defun lpa--define-key-bindings (keymap bindings &optional inhibit-self-insert-command)
    "Define keys in KEYMAP according to BINDINGS.
If MODE is non-nil, define keys only when the current mode matches MODE."
    (let ((modes nil))
      (dolist (binding bindings)
	(catch 'continue
	  (progn (when (eq binding
			   :mode)
		   (setq modes t)
		   (throw 'continue nil))
		 (when modes
		   (setq modes (lpa--ensure-list binding))
		   (throw 'containue nil))))

	(if modes
	    (dolist (mode modes)
	      (let ((mode-keymap (make-sparse-keymap))
		    (mode-hook (intern (concat (symbol-name mode) "-hook"))))
		(lpa--define-key mode-keymap
				 binding)
		(set-keymap-parent mode-keymap
				   keymap)
		(add-hook mode-hook (lambda ()
				      (interactive)
				      (if lpa-mode
					  (use-local-map mode-keymap))))))
	  (lpa--define-key keymap binding)))

      ;; 绑定未定义的字符
      (if inhibit-self-insert-command
	  (dolist (list-or-vector lpa-inhibit-self-insert-keys)
	    (cond ((listp list-or-vector)
		   (dolist (key list-or-vector)
		     (define-key keymap
				 (kbd key)
				 (lambda ()
				   (interactive)
				   (lpa-undefined-key key)))))
		  ((vectorp list-or-vector)
		   (let* ((pair (aref list-or-vector 0))
			  (start (car pair))
			  (end (cdr pair))
			  (is-list (listp pair)))
		     (dotimes (i (- end start -1))
		       (let* ((char (+ start i))
			      (key (char-to-string char))
			      (defined-key (lookup-key keymap key)))
			 (unless defined-key
			   (define-key keymap
				       (kbd key)
				       (lambda ()
					 (interactive)
					 (lpa-undefined-key key)))))))))))))

   
(defun lpa--generate-layer-keymaps ()
  "Generate keymaps for all layers in `lpa-layer-configurations`."

  (setq lpa--layer-keymaps nil)
  (dolist (layer lpa-layer-configurations)
    (let ((layer-symbol (car layer))
	  (layer-bindings (plist-get (cdr layer) :bind))
	  (layer-inhibit-self-insert-command (plist-get (cdr layer) :inhibit-self-insert-command))
	  (keymap (make-keymap)))
      ;; Process layer bindings
      (when (listp layer-bindings)
	(lpa--define-key-bindings keymap
				  layer-bindings
				  layer-inhibit-self-insert-command))

      ;; concat to lpa--kayer-keymaps
      (setq lpa--layer-keymaps (cons (cons layer-symbol keymap)
				     lpa--layer-keymaps)))))



(defun lpa-switch-layer (layer-symbol)
  "Switch to the layer specified by LAYER-SYMBOL."

  (interactive
   (list
    (intern (completing-read "Switch to layer: "
			     (mapcar #'car lpa-layer-configurations)))))
  (if lpa-mode
      (let ((layer (assoc layer-symbol lpa-layer-configurations)))
	(if layer
	    (let ((keymap (cdr (assoc layer-symbol lpa--layer-keymaps))))
	      (setq lpa--active-layer-symbol layer-symbol)
	      (force-mode-line-update)
	      (use-local-map keymap)
	      (run-hooks 'lpa-after-layer-switch-hook)
	      (message "Switched to %s" layer-symbol))
	  (error "Layer %s not found" layer-symbol)))
    (message "lpa-mode is not enabled")))

(define-minor-mode lpa-mode
  "Toggle LPA mode."

  :lighter (:eval (plist-get (cdr (assoc lpa--active-layer-symbol
					 lpa-layer-configurations))
			     :name))
  (if lpa-mode
      (progn
	(lpa--generate-layer-keymaps)
	(when (listp lpa-layer-configurations)
	  (lpa-switch-layer
	   (or lpa-default-layer
	       (car (car lpa-layer-configurations))))))
    (setq lpa--active-layer-symbol nil)
    (use-local-map nil)
    (force-mode-line-update)))


(provide 'lpa)
;;; lpa.el ends here
