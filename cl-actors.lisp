(in-package #:cl-actors)

;;;;;;;;;; Class and creation/destruction constructs
(defclass actor ()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor name)
   (behavior :initarg :behavior
	     :initform (error ":behavior must be specified")
	     :accessor behavior
	     :documentation "Behavior")
   (targets :initarg :targets :initform nil 
	    :accessor targets)
   (in :initform (make-queue) :accessor in
       :documentation "Queue of incoming messages")
   thread))

(defmethod initialize-instance :after ((self actor) &key)
  "Uses the main function name to create a thread"
  (with-slots (behavior in name thread) self
    (setf thread 
	  (bt:make-thread 
	   (lambda () 
	     (loop
		(handler-case
		    (let ((res (funcall behavior (dequeue in))))
		      (loop for target in (targets self)
			 do (enqueue res target)))
		  (match-error (e)
		    (format t "There isn't a match clause that fits. Do something more intelligent with unmatched messages.~%~a~%~%" e))
		  (error (e)
		    (format t "BLEARGH! I AM SLAIN! (this should kill the actor, and possibly call some fall-back mechanism)~%~a~%~%" e)))))
	   :name name))))

(defun make-actor (behavior name)
  "The shell of an actor"
  (make-instance 'actor
                 :name (concatenate 'string "Actor: " name)
                 :behavior behavior))

(defmethod stop-actor ((self actor))
  "Stops the actor thread"
  (with-slots (thread) self (destroy-thread thread)))

(defmacro define-actor (name state &body match-clauses)
  "Macro for creating actors with the behavior specified by body"
  `(defun ,name (&key (self) ,@state)
     (declare (ignorable self)) ;; You might not care about referencing self
     (setf self 
	   (make-actor 
	    (lambda (message) 
	      (ematch message
		((list :ping target) (send target (list :pong (get-universal-time))))
		,@(loop for (m b) on match-clauses by #'cddr collecting (list m b)))) 
	    ,(string name)))
     self))

;;;;;;;;;; Manual sending/queuing methods
(defmethod enqueue (object (target actor))
  (enqueue object (in target)))

(defmethod send ((self message-queue) message)
  "Asynchronously enqueues a message in the target queue."
  (bt:make-thread (lambda () (enqueue message self)))
  (values))

(defmethod send ((self actor) message)
  "Asynchronously enqueues a message in the target actors' in-queue."
  (send (in self) message))

;;;;;;;;;; Sending Protocols
(defmethod round-robin ((self actor))
  (let ((ts (targets self)))
    (lambda (msg)
      (unless ts (setf ts (targets self)))
      (list msg (pop ts)))))

(defmethod send-all ((self actor))
  (lambda (msg)
    (loop for target in (targets self)
       do (enqueue (list res) (in target)))))

;;;;;;;;;; Connection functions
(defun chain (&rest actors)
  "Takes a list of actors and links each to the next."
  (loop for (a1 a2) on actors
     while a2 do (link a1 a2)))

(defmethod link ((selves list) (target actor))
  "Causes each of [selves] to send its results to [target] from now on."
  (mapcar (lambda (s) (link s target)) selves))

(defmethod link ((self actor) (targets list))
  "Causes [self] to send its result to each [target] from now on."
  (mapcar (lambda (tgt) ;; damn you, default truth value!
	    (link self tgt))
	  targets))

(defmethod link ((self actor) (target actor))
  "Causes [self] to send its results to [target] from now on."
  (push target (targets self)))