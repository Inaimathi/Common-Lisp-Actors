(in-package #:cl-actors)

(defun curry (f &rest args)
  "Simple currying implementation."
  (lambda (&rest rem)
    (apply f (append rem args))))