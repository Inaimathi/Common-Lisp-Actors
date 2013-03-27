;;;; --------------------------------------------------------------------------
;;;; @file   package.lisp
;;;; @author Nikhil J. Shetty <nikhil.j.shetty@gmail.com> 
;;;; @date   Thu May  6 00:32:51 2010
;;;;
;;;; @brief  Package definitions
;;;; --------------------------------------------------------------------------
(in-package #:cl-user)

(defpackage #:cl-actors
  (:use #:cl #:bordeaux-threads #:optima)
  (:export 
   ;; actor-related
   #:actor #:define-actor #:stop-actor #:self 
   #:in #:behavior #:watched-by #:thread
   #:send #:link #:chain
   ;; queue-related
   #:make-queue #:enqueue #:dequeue #:dequeue-no-hang 
   #:full-p #:empty-p #:len #:messages
   ;; pattern-related
   #:match #:multiple-value-match #:guard))
