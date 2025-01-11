;;; lpa-var.el --- LPA variables -*- lexical-binding: t; -*-

;;; Commentary:
;;; Internal variables and customizable variables.

;;; Code:

(require 'lpa-layer)
(require 'lpa-var)


(define-minor-mode lpa-mode
  "Toggle LPA mode."
  :lighter (:eval (plist-get (cdr (assoc lpa--active-layer
                                         lpa--layers))
                             :name))
  (if lpa-mode
      (lpa-switch-layer (or lpa-default-layer
                            (caar lpa--layers)))
    
    (setq lpa--active-layer nil)
    (use-local-map nil)
    (force-mode-line-update)))

(provide 'lpa)
;;; lpa.el ends here
