;;; -*- mode: lisp -*-
;;;
;;; an implementation of the OSC (Open Sound Control) protocol
;;;
;;; copyright (C) 2004 FoAM vzw. 
;;;
;;; You are granted the rights to distribute and use this software
;;; under the terms of the Lisp Lesser GNU Public License, known 
;;; as the LLGPL. The LLGPL consists of a preamble and the LGPL. 
;;; Where these conflict, the preamble takes precedence. The LLGPL
;;; is available online at http://opensource.franz.com/preamble.html 
;;; and is distributed with this code (see: LICENCE and LGPL files)
;;;
;;; authors 
;;;
;;;  nik gaffney <nik@f0.am>
;;;
;;; requirements
;;;
;;;  dependent on sbcl, cmucl or openmcl for float encoding, other suggestions
;;;  welcome. 
;;;
;;; commentary
;;;
;;;  this is a partial implementation of the OSC protocol which is used
;;;  for communication mostly amongst music programs and their attatched
;;;  musicians. eg. sc3, max/pd, reaktor/traktorska etc+. more details 
;;;  of the protocol can be found at the open sound control pages -=> 
;;;                     http://www.cnmat.berkeley.edu/OpenSoundControl/
;;; 
;;;   - doesnt send nested bundles or timetags later than 'now' 
;;;   - malformed input -> exception
;;;   - int32 en/de-coding based on code (c) Walter C. Pelissero
;;;   - unknown types are sent as 'blobs' which may or may not be an issue
;;;
;;;  see the README file for more details...
;;;
;;; known BUGS
;;;   - encoding a :symbol which is unbound, or has no symbol-value will cause
;;;     an error
;;;

(defpackage :osc
  (:use :cl)
  (:documentation "OSC aka the 'open sound control' protocol")
  (:export :encode-message
	   :encode-bundle
	   :decode-message
	   :decode-bundle))

(in-package :osc)
 
;(declaim (optimize (speed 3) (safety 1) (debug 3)))

;;;;;; ;    ;;    ;     ; ;     ; ; ;         ;
;; 
;;   eNcoding OSC messages
;;
;;;; ;;  ;;   ; ; ;;           ;      ;  ;                  ;

(defun encode-bundle (data &optional timetag)
  "will encode an osc message, or list of messages as a bundle
   with an optional timetag (symbol or 64bit int).
   doesnt handle nested bundles"
  (cat '(35 98 117 110 100 108 101 0)	; #bundle
       (if timetag
           (encode-timetag timetag)
           (encode-timetag :now))
       (if (listp (car data))
	   (apply #'cat (mapcar #'encode-bundle-elt data))
	 (encode-bundle-elt data))))

(defun encode-bundle-elt (data)
  (let ((message (apply #'encode-message data)))
    (cat (encode-int32 (length message)) message)))      

(defun encode-message (address &rest data)
  "encodes an osc message with the given address and data."
  (concatenate '(vector (unsigned-byte 8))
	       (encode-address address)
	       (encode-typetags data)
	       (encode-data data)))

(defun encode-address (address)
  (cat (map 'vector #'char-code address) 
       (string-padding address)))

(defun encode-typetags (data)
  "creates a typetag string suitable for the given data.
  valid typetags according to the osc spec are ,i ,f ,s and ,b
  non-std extensions include ,{h|t|d|S|c|r|m|T|F|N|I|[|]}
                             see the spec for more details. ..

  NOTE: currently handles the following tags 
   i => #(105) => int32
   f => #(102) => float
   s => #(115) => string 
   b => #(98)  => blob
  and considers non int/float/string data to be a blob." 

  (let ((lump (make-array 0 :adjustable t 
			  :fill-pointer t)))
    (macrolet ((write-to-vector (char)
                 `(vector-push-extend
                   (char-code ,char) lump)))
      (write-to-vector #\,)
      (dolist (x data) 
        (typecase x
          (integer (write-to-vector #\i))
          (float (write-to-vector #\f))
          (simple-string (write-to-vector #\s))
	  (t (write-to-vector #\b)))))
    (cat lump
         (pad (padding-length (length lump))))))     
		  
(defun encode-data (data)
  "encodes data in a format suitable for an OSC message"
  (let ((lump (make-array 0 :adjustable t :fill-pointer t)))
    (macrolet ((enc (f)
                 `(setf lump (cat lump (,f x)))))
      (dolist (x data) 
        (typecase x
          (integer (enc encode-int32)) 
          (float (enc encode-float32)) 
          (simple-string (enc encode-string))
	  (t (enc encode-blob))))
      lump)))

                
;;;;;; ;    ;;    ;     ; ;     ; ; ;         ;
;; 
;;    decoding OSC messages
;;
;;; ;;    ;;     ; ;     ;      ;      ; ;

(defun decode-bundle (data)
  "decodes an osc bundle into a list of decoded-messages, which has
   an osc-timetagas its first element"
  (let ((contents '()))
    (if (equalp 35 (elt data 0))	; a bundle begins with '#'
	(let ((timetag (subseq data 8 16)) 
	      (i 16)
	      (bundle-length (length data)))
	  (loop while (< i bundle-length)
	     do (let ((mark (+ i 4))
		      (size (decode-int32
			     (subseq data i (+ i 4)))))
		  (if (eq size 0)
		      (setf bundle-length 0)
		      (push (decode-bundle
			     (subseq data mark (+ mark size)))
			    contents))
		  (incf i (+ 4 size))))
	  (push timetag contents))
	(decode-message data))))
     
(defun decode-message (message)
  "reduces an osc message to an (address . data) pair. .." 
  (declare (type (vector *) message))
  (let ((x (position (char-code #\,) message)))
    (if (eq x NIL)
        (format t "message contains no data.. ")
	(cons (decode-address (subseq message 0 x))
	      (decode-taged-data (subseq message x))))))
 
(defun decode-address (address)
  (coerce (map 'vector #'code-char 
	       (delete 0 address))
	  'string))

(defun decode-taged-data (data)
  "decodes data encoded with typetags...
  NOTE: currently handles the following tags 
   i => #(105) => int32
   f => #(102) => float
   s => #(115) => string
   b => #(98)  => blob"

  (let ((div (position 0 data)))
    (let ((tags (subseq data 1 div)) 
	  (acc (subseq data (padded-length div)))
	  (result '()))
      (map 'vector
	   #'(lambda (x)
	       (cond
		((eq x (char-code #\i)) 
		 (push (decode-int32 (subseq acc 0 4)) 
		       result)
		 (setf acc (subseq acc 4)))
		((eq x (char-code #\f))
		 (push (decode-float32 (subseq acc 0 4)) 
		       result)
		 (setf acc (subseq acc 4)))
		((eq x (char-code #\s))
		 (let ((pointer (padded-length (position 0 acc))))
		   (push (decode-string 
			  (subseq acc 0 pointer))
			 result)
		   (setf acc (subseq acc pointer))))
		((eq x (char-code #\b)) 
		 (let* ((size (decode-int32 (subseq acc 0 4)))
                        (end (padded-length (+ 4 size))))
                   (push (decode-blob (subseq acc 0 end)) 
                         result)
                   (setf acc (subseq acc end))))
		(t (error "unrecognised typetag"))))
	   tags)
      (nreverse result))))


;;;;;; ;; ;; ; ; ;  ;  ; ;;     ;
;;	
;; timetags
;;
;; - timetags can be encoded using a value, or the :now and :time keywords. the
;;   keywords enable either a tag indicating 'immediate' execution, or
;;   a tag containing the current time (which will most likely be in the past
;;   of anyt receiver) to be created.
;;
;; - note: not well tested, and probably not accurate enough for syncronisation.
;;   see also: CLHS 25.1.4 Time, and the ntp timestamp format. also needs to
;;   convert from 2 32bit ints to 64bit fixed point value.
;;
;; - see this c.l.l thread to sync universal-time and internal-time
;;   http://groups.google.com/group/comp.lang.lisp/browse_thread/thread/c207fef63a78d720/adc7442d2e4de5a0?lnk=gst&q=internal-real-time-sync&rnum=1#adc7442d2e4de5a0
;;
;;;; ;; ; ; 

(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun encode-timetag (utime &optional subseconds)
  "encodes an osc timetag from a universal-time and 32bit 'sub-second' part.
   for an 'instantaneous' timetag use (encode-timetag :now) 
   for a timetag with the current time use (encode-timetag :time)"
  (cond
    ;; a 1bit timetag will be interpreted as 'imediately' 
    ((equalp utime :now)
     #(0 0 0 0 0 0 0 1)) 
    ;; converts seconds since 19000101 to seconds since 19700101
    ;; note: fractions of a second is accurate, but not syncronised.
    ((equalp utime :time)
     (cat (encode-int32 (- (get-universal-time) +unix-epoch+))
          (encode-int32 
           (round (* internal-time-units-per-second
                     (second (multiple-value-list  
                              (floor (/ (get-internal-real-time) 
                                        internal-time-units-per-second)))))))))
    ((integerp utime)
     (cat (encode-int32 (+ utime +unix-epoch+))
          (encode-int32 subseconds)))
    (t (error "the time or subsecond given is not an integer"))))

(defun decode-timetag (timetag)
  "decomposes a timetag into unix-time and a subsecond,. . ."
  (list
   (decode-int32 (subseq timetag 0 4))
   (decode-int32 (subseq timetag 4 8))))


;;;;; ; ; ;;    ;; ; ;
;;
;; dataformat en- de- cetera.
;;
;;; ;; ;   ;  ;

;; floats are encoded using implementation specific 'internals' which is not
;; particulaly portable, but 'works for now'. 

(defun encode-float32 (f)
  "encode an ieee754 float as a 4 byte vector. currently sbcl/cmucl specifc"
  #+sbcl (encode-int32 (sb-kernel:single-float-bits f))
  #+cmucl (encode-int32 (kernel:single-float-bits f))
  #+openmcl (encode-int32 (CCL::SINGLE-FLOAT-BITS f))
  #+allegro (encode-int32 (multiple-value-bind (x y) (excl:single-float-to-shorts f)
			    (+ (ash x 16) y)))
  #-(or sbcl cmucl openmcl allegro) (error "cant encode floats using this implementation"))

(defun decode-float32 (s)
  "ieee754 float from a vector of 4 bytes in network byte order"
  #+sbcl (sb-kernel:make-single-float (decode-int32 s))
  #+cmucl (kernel:make-single-float (decode-int32 s))
  #+openmcl (CCL::HOST-SINGLE-FLOAT-FROM-UNSIGNED-BYTE-32 (decode-uint32 s))
  #+allegro (excl:shorts-to-single-float (ldb (byte 16 16) (decode-int32 s))
				    (ldb (byte 16 0) (decode-int32 s)))
  #-(or sbcl cmucl openmcl allegro) (error "cant decode floats using this implementation"))

(defun decode-int32 (s)
  "4 byte -> 32 bit int -> two's compliment (in network byte order)"
  (let ((i (+ (ash (elt s 0) 24)
	      (ash (elt s 1) 16)
	      (ash (elt s 2) 8)
	      (elt s 3))))
    (if (>= i #x7fffffff)
        (- 0 (- #x100000000 i))
	i)))

(defun decode-uint32 (s)
  "4 byte -> 32 bit unsigned int"
  (let ((i (+ (ash (elt s 0) 24)
	      (ash (elt s 1) 16)
	      (ash (elt s 2) 8)
	      (elt s 3))))
    i))

(defun encode-int32 (i)
  "convert an integer into a sequence of 4 bytes in network byte order."
  (declare (type integer i))
  (let ((buf (make-sequence 
	      '(vector (unsigned-byte 8)) 4)))
    (macrolet ((set-byte (n)
		 `(setf (elt buf ,n)
			(logand #xff (ash i ,(* 8 (- n 3)))))))
      (set-byte 0)
      (set-byte 1)
      (set-byte 2)
      (set-byte 3))
    buf))

;; osc-strings are unsigned bytes, padded to a 4 byte boundary 

(defun decode-string (data)
  "converts a binary vector to a string and removes trailing #\nul characters"
  (string-trim '(#\nul) (coerce (map 'vector #'code-char data) 'string)))

(defun encode-string (string)
  "encodes a string as a vector of character-codes, padded to 4 byte boundary"
  (cat (map 'vector #'char-code string) 
       (string-padding string)))

;; blobs are binary data, consisting of a length (int32) and bytes which are
;; osc-padded to a 4 byte boundary.

(defun decode-blob (blob)
  "decode a blob as a vector of unsigned bytes."
  (let ((size (decode-int32
	       (subseq blob 0 4))))
    (subseq blob 4 (+ 4 size)))) 

(defun encode-blob (blob)
  "encodes a blob from a given vector"
  (let ((bl (length blob)))
    (cat (encode-int32 bl) blob
	 (pad (padding-length bl)))))      

;; utility functions for osc-string/padding slonking

(defun cat (&rest catatac)
  (apply #'concatenate '(vector *) catatac))

(defun padding-length (s)
  "returns the length of padding required for a given length of string"
  (declare (type fixnum s))
  (- 4 (mod s 4)))

(defun padded-length (s)
  "returns the length of an osc-string made from a given length of string"
  (declare (type fixnum s))
  (+ s (- 4 (mod s 4))))

(defun string-padding (string)
  "returns the padding required for a given osc string"
  (declare (type simple-string string)) 
  (pad (padding-length (length string))))

(defun pad (n)
  "make a sequence of the required number of #\Nul characters"
  (declare (type fixnum n))
  (make-array n :initial-element 0 :fill-pointer n))

(provide :osc)
;; end
