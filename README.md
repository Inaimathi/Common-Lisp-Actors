This is a simple and easy to use Actor system in Common Lisp. 

# Set-Up
1. Requires Bordeaux threads. http://common-lisp.net/project/bordeaux-threads/ 2. Just load actors.lisp and start using it. 

# Usage 
An small manual can be found at : 
http://www.cs.rpi.edu/~govinn/actors.pdf

1. Creating an actor class or template

    (defactor Actor-Class (state) (message-vars)
       behavior)

2. Creating an actor instance 

    (setq my-actor (Actor-Class (:state-var_1 value_1 ... :state-var_n value_n)))

3. Sending a message

    (send my-actor message_args)

# Features

1. Concurrency using the actors model.
2. Dynamic behavior change of actors.

# Examples

1. A ticker: Keeps printing out a count every 2 seconds, starting from 0 and incrementing it every 2 seconds. 

    ; create the ticker template
    (defactor ticker ((counter 0)) (m) 
       (sleep 2) 
       (pr counter)
       (incf counter) 
       (send self nil) 
       next)
       
    ; Create an instance
    (defparameter t1 (ticker))
    
    ; send a message (async)
    (send t1 nil)
    
    ; to stop use
    (stop-actor t1)

2. A print actor: Prints the message which was sent to it. A very useful utility actor. 

    ; create the actor template
    (defactor print-actor (stream) (val) 
       (format stream "~a~%" val)
       next)
       
    ; initialize a new instance
    (defparameter printer (print-actor :stream *standard-output*))
    
    ; send values for printing
    (send printer "hello, world")

3. A factorial computing actor : The name says it all :)

    ; create the template
    (defactor fact ((temp 1)) (n cust) 
      (if (equal 1 n) 
          (progn (send cust (* temp 1))
                 (setf temp 1))
	  (progn (setf temp (* n temp))
                 (send self (- n 1) cust)))
      next)

    ; create a new instance 
    (setf f (fact))
    
    ; send a value
    (send f 4 print-actor)

4. A nagger for fun : Works only in Mac OS X. Keeps saying out aloud "please work" every 10 seconds :)

    (defactor nagger () () 
       (sleep 10)
       (trivial-shell:shell-command "say please work")
       (send self) 
       next)
       
    ; anonymous actor , no way to stop the nagging 
    (send (nagger))
