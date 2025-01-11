;;; lpa-util.el -*- lexical-binding: t; -*-

;;; Code:

(defun lpa--ensure-list (arg)
  "Ensure the input ARG is a list. If it's not a list, convert it to a list containing the argument."
  (if (or (null arg) (listp arg))
      arg
    (list arg)))

(provide 'lpa-util)
;;; lpa-util.el ends here
