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
   (watched-by :initarg :watched-by :initform nil 
	     :accessor watched-by)
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
		for res = (funcall behavior (dequeue in))
		when (and res (watched-by self))
		  ;; TODO -- Add customization to protocol, rather than always send-all
		do (loop for target in (watched-by self)
		      do (enqueue (list res) target))))
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
	      (match message
		((list :ping target) (send target :pong (get-universal-time)))
		,@(loop for (m b) on match-clauses by #'cddr collecting (list m b)))) 
	    ,(string name)))
     self))

;;;;;;;;;; Manual sending/queuing methods
(defmethod enqueue (object (target actor))
  (enqueue object (in target)))

(defmethod send ((self message-queue) &rest message)
  "Creates a message sending thread to push a new message into the target message-queue."
  (bt:make-thread (lambda () (enqueue message self)))
  (values))

(defmethod send ((self actor) &rest message)
  "`send`s a message to the IN quque of the target actor."
  (apply #'send (in self) message))

;;;;;;;;;; Sending Protocols
(defmethod round-robin ((self actor))
  (let ((ts (watched-by self)))
    (lambda (msg)
      (unless ts (setf ts (watched-by self)))
      (list msg (pop ts)))))

(defmethod send-all ((self actor))
  (lambda (msg)
    (loop for target in (watched-by self)
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
  (push target (watched-by self)))