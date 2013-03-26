;;;; --------------------------------------------------------------------------
;;;; @file   package.lisp
;;;; @author Nikhil J. Shetty <nikhil.j.shetty@gmail.com> 
;;;; @date   Thu May  6 00:32:51 2010
;;;;
;;;; @brief  Package definitions
;;;; --------------------------------------------------------------------------
(in-package #:cl-user)

(defpackage #:cl-actors
  (:use #:cl #:bordeaux-threads)
  (:export 
   #:actor #:define-actor #:self #:send #:send-receive #:stop-actor
   #:link #:chain
   #:make-queue #:enqueue #:dequeue #:dequeue-no-hang 
                #:full-p #:empty-p #:len #:messages))
