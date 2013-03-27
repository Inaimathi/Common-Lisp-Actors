(define-actor phil (l r (sticks 0)) 
  (guard _ (zerop sticks)) (progn (format t "Got one") (incf sticks))
  _ (progn 
      (format t "Got both")
      (send l :drop self) (send r :drop)
      (format t "Dropped")
      (send l :pick self) (send r :pick self)
      (setf sticks 0)))

(define-actor chopstick ((h nil) (w nil)) 
  (guard (list :pick target) (null h)) (progn (send target nil) 
					      (setf h target w nil))
  (list :pick target) (setf w target)

  (guard (list :drop target) (null w)) (setf w nil h nil)
  (list :drop target) (progn (send w nil)
			     (setf h w w nil)))