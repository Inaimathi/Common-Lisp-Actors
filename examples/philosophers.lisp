(define-actor phil (left right (sticks 0)) 
  (guard _ (zerop sticks)) (progn (format t "Got one") (incf sticks))
  _ (progn 
      (format t "Got both")
      (send left (list :drop self)) (send right (list :drop self))
      (format t "Dropped")
      (send left (list :pick self)) (send right (list :pick self))
      (setf sticks 0)))

(define-actor chopstick ((h nil) (w nil)) 
  (guard (list :pick target) (null h)) (progn (send target nil) 
					      (setf h target w nil))
  (list :pick target) (setf w target)

  (guard (list :drop target) (null w)) (setf w nil h nil)
  (list :drop target) (progn (send w nil)
			     (setf h w w nil)))