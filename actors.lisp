(in-package #:cl-actors)

(defclass actor ()
  ((name :initarg :name
         :initform (error ":name must be specified")
         :accessor name)
   (behavior :initarg :behavior
	     :initform (error ":behav must be specified")
	     :accessor behavior
	     :documentation "Behavior")
   (in :initform (make-queue) :accessor in
       :documentation "Queue of incoming messages")
   thread))


(defmethod initialize-instance :after ((self actor) &key)
  "Uses the main function name to create a thread"
  (with-slots (behavior in name thread) self
    (setf thread 
	  (bt:make-thread 
	   (lambda () (loop while (apply behavior (dequeue in))))
	   :name name))))

(defmethod send ((self message-queue) &rest message)
  "Creates a message sending thread to push a new message into the target message-queue."
  (bt:make-thread (lambda () (enqueue message self)))
  (values))

(defmethod send ((self actor) &rest message)
  "`send`s a message to the IN quque of the target actor."
  (apply #'send (in self) message))

(defmethod stop-actor ((self actor))
  "Stops the actor thread"
  (with-slots (thread) self (destroy-thread thread)))

(defmacro behav (state vars &body body)
  "Create a behavior that can be attached to any actor."
  `(let ,state
     (labels ((me ,(append vars `(&key self (next #'me next-supplied-p)))
                (when next-supplied-p 
		  (setf next (curry next :self self)))
		,@body))
       #'me)))

(defmacro defactor (name state vars &body body)
  "Macro for creating actors with the behavior specified by body"
  `(defun ,name (&key (self) ,@state)
     (labels ((me ,(append vars `(&key (next #'me next-supplied-p)))
                (when next-supplied-p 
		  (setf next (curry next :self self)))
                ,@body))
       (setf self (make-actor #'me ,(string name))) 
       self)))

(defun make-actor (behav name)
  "The shell of an actor"
  (make-instance 'actor
                 :name (concatenate 'string "Actor: " name)
                 :behavior behav))

(defun curry (f &rest args)
  "Simple currying implementation."
  (lambda (&rest rem)
    (apply f (append rem args))))

(defmethod send-receive ((self actor) message &optional (timeout 0))
  "Sends a message to an actor, then blocks waiting for a response.
This method is intended to allow non-actor systems to interface with a network of actors.
The actor it sends to should expect to be handed a return target,
that target should eventually be sent a response message (not necessarily right away; the receiver
can chain calls, but the initial thread will block until a response arrives)."
  (let ((q (make-queue)))
    (send self q message)
    (car (dequeue q timeout))))