(in-package #:cl-actors)

;;;;;;;;;; Class and creation/destruction constructs
(defclass actor ()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor name)
   (behavior :initarg :behavior
	     :initform nil
	     :accessor behavior
	     :documentation "Behavior")
   (targets :initarg :targets :initform nil 
	    :accessor targets)
   (supervisors :initarg :supervisors :initform nil
		:accessor supervisors)
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
		(let ((msg (dequeue in)))
		  (handler-case
		      (let ((res (funcall behavior msg)))
			(loop for target in (targets self)
			   do (enqueue res target)))
		    (match-error (e)
		      (declare (ignore e))
		      (when (supervisors self)
			(loop for s in (supervisors self)
			   do (enqueue (list :unhandled-message self msg) s))))
		    (error (e)
		      (format t "BLEARGH! I AM SLAIN! (this should kill the actor, and possibly call some fall-back mechanism)~%~a~%~%" e))))))
	   :name name))))

(defmethod stop-actor ((self actor))
  "Stops the actor thread"
  (with-slots (thread) self (destroy-thread thread)))

(defmacro make-behavior (state &body match-clauses)
  `(lambda (self ,@state)
     (lambda (message)
       (ematch message
	 ((list :ping target) (send target (list :pong (get-universal-time))))
	 ((list :brainwash maker)
	  ;; NOTE - This needs to do some error checking and attempt at least one message before it gets slotted in. We'll need a peek-queue or something.
	  (setf (behavior self) (funcall maker self ,@state)))
	 ,@(loop for (m b) on match-clauses by #'cddr collecting (list m b))))))

(defmacro define-actor (name state &body match-clauses)
  "Macro for creating actors with the behavior specified by body"
  (let ((state-list (loop for s in state if (listp s) collect (car s) else collect s)))
    `(defun ,name (&key (self) ,@state)
       (declare (ignorable self)) ;; You might not care about referencing self
       (setf self (make-instance 'actor :name (concatenate 'string "Actor: ",(string name))))
       (setf (behavior self) (funcall (make-behavior ,state-list ,@match-clauses) self ,@state-list))
       self)))

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

(defmethod supervise ((self actor) (target actor))
  (push self (supervisors target)))

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