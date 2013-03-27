(define-actor cell (c) 
  (list :get target) (send target c)
  (list :set contents) (setf c contents))  
