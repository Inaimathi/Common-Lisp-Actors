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
   (out :initform (make-queue 50)
	:documentation "Queue of outgoing messages")
   (err :initform (make-queue 10)
	:documentation "Queue of error/debug messages")

   thread))

(defmethod initialize-instance :after ((self actor) &key)
  "Uses the main function name to create a thread"
  (with-slots (name thread) self
    (setf thread 
	  (bt:make-thread #'(lambda() (main self)) 
			  :name name))))

(defmethod send ((self actor) &rest message)
  "Creates a message sending thread to push a new message into the in` queue of the target actor."
  (bt:make-thread 
   (lambda () (enqueue message (in self))))
  (values))

(defmethod stop-actor ((self actor))
  "Stops the actor thread"
  (with-slots (thread) self (destroy-thread thread)))

;; I think that this should be more of an internal function 
;; than a method (experiment with funcallable-standard-class) 
;;;; -Naveen Sundar G.

;; No opinion 
;; Although I guess it's simple enough to just inline in initialize-instance now...
;;;; -Inaimathi
(defmethod main ((self actor))
  "The main which is started as a thread from the constructor."
  (with-slots (behavior in) self
    (loop while (apply behavior (dequeue in)))))

;; Not sure whether the below would benefit from
;;  1. automatically adding a next call after ,@body
;;  2. automatically sending the result of ,@body to the out queue
;;;; -Inaimathi
(defmacro behav (state vars &body body)
  "Create a behavior that can be attached to any actor."
  `(let ,state
     (labels ((me ,(append vars `(&key self (next #'me next-supplied-p)))
                (setf next (curry next :self self))
		x ,@body))
       #'me)))

;; Same concerns as above (in fact, I'm not entirely clear on why
;; defactor doesn't simply call behav, since that seems to be the point)
;;;; -Inaimathi
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