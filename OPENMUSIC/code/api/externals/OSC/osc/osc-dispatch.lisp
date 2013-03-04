;; -*- mode: lisp -*-
;;
;; patern matching and dispatching for OSC messages 
;;
;; copyright (C) 2004 FoAM vzw
;;
;; You are granted the rights to distribute and use this software
;; under the terms of the Lisp Lesser GNU Public License, known 
;; as the LLGPL. The LLGPL consists of a preamble and the LGPL. 
;; Where these conflict, the preamble takes precedence. The LLGPL
;; is available online at http://opensource.franz.com/preamble.html 
;; and is distributed with this code (see: LICENCE and LGPL files)
;;

;; authors
;;  - nik gaffney <nik@f0.am>

;; requirements
;;  - not too useful without osc
;;  - probably cl-pcre for matching (when it happens). 

;; commentary
;;  an osc de-/re -mungulator which should deal with piping data
;;  from incoming messages to the function/handler/method
;;  designated by the osc-address. 
;;
;;  NOTE: only does direct matches for now, no pattern globs,
;;        with single function per uri     

;; changes
;;  2005-02-27 18:31:01
;;  - initial version

(in-package :osc)

;; should probably be a clos object or an alist.
;; for now, a hash table is enuf.

(defun make-osc-tree ()
  (make-hash-table :test 'equalp))


;;; ;; ;;;;;;  ;        ;  ;  ;
;;
;; register/delete and dispatch. .. 
;; 
;;;;  ; ; ;   ;; 

(defun dp-register (tree address function)
  "registers a function to respond to incoming osc message. since
   only one function should be associated with an address, any
   previous registration will be overwritten"
  (setf (gethash address tree)
	function))

(defun dp-remove (tree address)
  "removes the function associated with the given address.."
  (remhash address tree))

(defun dp-match (tree pattern)
"returns a list of functions which are registered for
 dispatch for a given address pattern.."
  (list (gethash pattern tree)))

(defun dispatch (tree osc-message)
  "calls the function(s) matching the address(pattern) in the osc 
   message with the data contained in the message"
  (let ((pattern (car osc-message)))
    (dolist (x (dp-match tree pattern))
      (unless (eq x NIL)
        (apply #'x (cdr osc-message))))))

