(in-package #:cl-actors)

(defclass message-queue ()
  ((messages :accessor messages :initarg :messages :initform nil)
   (last-cons :accessor last-cons :initarg :last-cons :initform nil
    :documentation "Cached end of the list")
   (len :accessor len :initarg :len :initform 0
    :documentation "Cached message queue length. Modified by enqueue and dequeue")
   (lock :initform (bt:make-lock) :accessor lock
    :documentation "Lock for this message queue")
   (max-len :accessor max-len :initarg :max-len :initform nil
    :documentation "If present, queue maintains at most this many elements")
   (flag :initform (bt:make-condition-variable) :accessor flag
    :documentation "Condition variable used to notify that a message was enqueued")))

(defun make-queue (&optional max-len) 
  (make-instance 'message-queue :max-len max-len))

(defmethod full-p ((queue message-queue))
  (with-slots (len max-len)
      (and max-len (>= len max-len))))

(defmethod empty-p ((queue message-queue)) 
  (= (len queue) 0))

(defmethod enqueue (object (queue message-queue))
  "Adds an element to the back of the given queue in a thread-safe way."
  (with-slots (lock messages max-len len flag last-cons) queue
    (with-lock-held (lock)
      (let ((o (list object)))
	(cond ((empty-p queue)
	       (setf messages o 
		     last-cons messages
		     len 1))
	      ((full-p queue)
	       (pop messages)
	       (setf (cdr last-cons) o 
		     last-cons o))
	      (t (setf (cdr last-cons) o
		       last-cons o)
		 (incf len)))))
    (condition-notify flag)
    messages))

(defmethod dequeue ((queue message-queue) &optional (timeout 0))
  "Pops a message from the given queue in a thread-safe way.
If the target queue is empty, blocks until a message arrives.
If timeout is not zero, errors after timeout."
  (with-slots (messages lock flag len) queue 
    (with-timeout (timeout)
      (with-lock-held (lock)
	(unless messages (condition-wait flag lock))
	(decf len)
	(pop messages)))))

(defmethod dequeue-no-hang ((queue message-queue))
  "Pops a message from the given queue in a thread-safe way.
If the target queue is empty, returns NIL.
The second value specifies whether an item was found in queue (this is meant
to disambiguate the situation where a queue contains the message NIL)"
  (with-slots (messages lock flag len) queue
    (with-lock-held (lock)
      (if messages
	  (progn
	    (decf len)
	    (values (pop messages) t))
	  (values nil nil)))))