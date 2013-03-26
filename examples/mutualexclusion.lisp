(define-actor sem ((h nil)) (m)
  (if (get? m)
      (if (null h)
	  (progn (send (cust m) t) 
		 (setf h (cust m)))
	  (send (cust m) nil))
      (when (release? m) (setf h nil))))

(define-actor customer (semaphore) (m)
  (if m
      (progn (pr "Critical code")
	     (send semaphore (make-release)))
      (send semaphore (make-get self))))
	     
(defun make-release () `("release" ))
(defun make-get (cust) `("get" ,cust)) 

(defun get? (message) (equal (first message) "get"))
(defun release? (message) (equal (first message) "release"))

(defun cust (message) (second message))