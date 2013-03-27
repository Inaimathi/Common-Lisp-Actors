(define-actor bouncer (name stream) 
  _ (progn (sleep 3)
	   (format stream "~a : bouncing~%" name)
	   :bounce))

(defparameter b1 (bouncer :name "Bouncer 1" :stream *standard-output*))
(defparameter b2 (bouncer :name "Bouncer 2" :stream *standard-output*))

(chain b1 b2 b1)

(send b1 :bounce)
