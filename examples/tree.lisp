(defun left (tree) (first tree))
(defun right (tree) (second tree))

;Join 
(define-actor joincont (customer (firstnum nil)) (message)
  (if (null firstnum)
      (progn (setf firstnum message))
      (progn (send customer (* firstnum message)) #'sink)))

(define-actor treeprod () (customer tree)
  (if (atom tree)
      (send customer tree)
      (let  ((newcust (joincont :customer customer))
	     (lp (treeprod))
	     (rp (treeprod)))
	(send lp newcust (left tree))
	(send rp newcust (right tree)))))