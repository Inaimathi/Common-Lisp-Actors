;;;;===========================================================================
;;;; @file   cl-actors.asd
;;;; @author Nikhil Shetty <nikhil.j.shetty@gmail.com>
;;;; @date   Thu May  6 00:19:07 2010
;;;; @author Naveen Sundar G. <naveensundarg@gmail.com>
;;;; @date   Thu Apr 5 2012
;;;; @brief asdf-install package file for cl-actors
;;;; @author Inaimathi <leo.zovic@gmail.com>
;;;; @date   Jup 25 2012
;;;; @brief queue system and output queues
;;;;===========================================================================

(defpackage #:cl-actors-asd (:use #:asdf #:cl))
(in-package :cl-actors-asd)

(defsystem cl-actors
  :author      "Nikhil Shetty <nikhil.j.shetty@gmail.com>"
  :version     "1.0"
  :licence     "BSD"
  :description ""
  :depends-on (:bordeaux-threads)
  :serial t
  :components ((:file "package")
	       (:file "queue")
               (:file "actors")))


