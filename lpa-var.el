;;; lpa-var.el --- LPA variables -*- lexical-binding: t; -*-

;;; Commentary:
;;; Internal variables and customizable variables.

;;; Code:

(defvar lpa-default-layer nil
  "Default layer to switch when mode start.")

(defvar lpa--base-layer nil
  "Current base layer symbol.")

(defvar lpa--overlaying-layers '()
  "Overlaying layers stack.")

(defvar lpa--layers '()
  "Cached layers.")

(provide 'lpa-var)
;;; lpa-var.el ends here
