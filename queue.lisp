(in-package #:cl-actors)

(defclass message-queue ()
  ((messages :accessor messages :initarg :messages :initform nil)
   (last-cons :accessor last-cons :initarg :last-cons :initform nil
    :documentation "Cached end of the list")
   (len :accessor len :initarg :len :initform 0
    :documentation "Cached message queue length. Modified by enqueue and dequeue")
   (max-len :accessor max-len :initarg :max-len :initform nil
    :documentation "If present, queue maintains at most this many elements")
   (lock :initform (bt:make-lock) :accessor lock
    :documentation "Lock for this message queue")
   (flag :initform (bt:make-condition-variable) :accessor flag
    :documentation "Condition variable used to notify that a message was enqueued")))

(defun make-queue (&optional max-len) 
  (make-instance 'message-queue :max-len max-len))

(defmethod enqueue (object (queue message-queue))
  "Adds an element to the back of the given queue in a thread-safe way."
  (with-slots (lock messages max-len len flag last-cons) queue
    (with-lock-held (lock)
      (let ((o (list object)))
	(cond ((or (null messages) (null last-cons))
	       (setf messages o 
		     last-cons messages
		     len 1))
	      ((= len max-len)
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
If the target queue is empty, returns NIL."
  (with-slots (messages lock flag len) queue
    (with-lock-held (lock)
      (when messages
	(decf len)
	(pop messages)))))