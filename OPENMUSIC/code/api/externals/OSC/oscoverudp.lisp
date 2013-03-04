(in-package :osc)

;;;;=============================
;;; REDEFS
;;;;=============================

(defun single-float-bits (x)
  (declare (type single-float x))
  (assert (= (float-radix x) 2))
  (if (zerop x)
      (if (eql x 0.0f0) 0 #x-80000000)
      (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
          (integer-decode-float x)
        (assert (plusp lisp-significand))
        ;; Calculate IEEE-style fields from Common-Lisp-style fields.
        ;;
        ;; KLUDGE: This code was written from my foggy memory of what IEEE
        ;; format looks like, augmented by some experiments with
        ;; the existing implementation of SINGLE-FLOAT-BITS, and what
        ;; I found floating around on the net at
        ;;   <http://www.scri.fsu.edu/~jac/MAD3401/Backgrnd/ieee.html>,
        ;;   <http://rodin.cs.uh.edu/~johnson2/ieee.html>,
        ;; and
        ;;   <http://www.ttu.ee/sidu/cas/IEEE_Floating.htm>.
        ;; And beyond the probable sheer flakiness of the code, all the bare
        ;; numbers floating around here are sort of ugly, too. -- WHN 19990711
        (let* ((significand lisp-significand)
               (exponent (+ lisp-exponent 23 127))
               (unsigned-result
                (if (plusp exponent)    ; if not obviously denormalized
                    (do ()
                        (nil)
                      (cond (;; special termination case, denormalized
                             ;; float number
                             (zerop exponent)
                             ;; Denormalized numbers have exponent one
                             ;; greater than the exponent field.
                             (return (ash significand -1)))
                            (;; ordinary termination case
                             (>= significand (expt 2 23))
                             (assert (< 0 significand (expt 2 24)))
                             ;; Exponent 0 is reserved for
                             ;; denormalized numbers, and 255 is
                             ;; reserved for specials like NaN.
                             (assert (< 0 exponent 255))
                             (return (logior (ash exponent 23)
                                             (logand significand
                                                     (1- (ash 1 23))))))

                            (t
                             ;; Shift as necessary to set bit 24 of
                             ;; significand.
                             (setf significand (ash significand 1)
                                   exponent (1- exponent)))))
                    (do ()
                        ((zerop exponent)
                         ;; Denormalized numbers have exponent one
                         ;; greater than the exponent field.
                         (ash significand -1))
                      (unless (zerop (logand significand 1))
                        (warn "denormalized SINGLE-FLOAT-BITS ~S losing bits"
                              x))
                      (setf significand (ash significand -1)
                            exponent (1+ exponent))))))
          (ecase lisp-sign
            (1 unsigned-result)
            (-1 (logior unsigned-result (- (expt 2 31)))))))))


(defun kludge-opaque-expt (x y)
  (expt x y))


(defun make-single-float (bits)
  (cond
    ;; IEEE float special cases
    ((zerop bits) 0.0)
    ((= bits #x-80000000) -0.0)
    (t (let* ((sign (ecase (ldb (byte 1 31) bits)
                      (0  1.0)
                      (1 -1.0)))
              (iexpt (ldb (byte 8 23) bits))
              (expt (if (zerop iexpt) ; denormalized
                        -126
                        (- iexpt 127)))
              (mant (* (logior (ldb (byte 23 0) bits)
                               (if (zerop iexpt)
                                   0
                                   (ash 1 23)))
                       (expt 0.5 23))))
         (* sign (kludge-opaque-expt 2.0 expt) mant)))))


(defun encode-float32 (f)
  "encode an ieee754 float as a 4 byte vector. currently sbcl/cmucl specifc"
  (encode-int32 (single-float-bits f)))

(defun decode-float32 (s)
  "ieee754 float from a vector of 4 bytes in network byte order"
  (make-single-float (decode-int32 s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :osc)
(export  '(open-osc-out-stream close-osc-stream write-osc-msg write-osc-bundle start-osc-server stop-osc-server decode-msg-or-bundle))

;; the representation of an osc msg is a list of string or fixnum objects
;; a bundle is a list of such. Upon reception, decode-bundle gives a list with a time-tag as its first element
;; does not handle floats and int are limited to 2^31

;; as udpsend and udpreceive tend to replace OpenSoundControl in max, there is a simplewrite-osc-msg
;; in addition to write-osc-bundle

;(defun write-osc-msg (message osc-stream)
;  (write-sequence (apply #'osc:encode-message message) osc-stream)
;  (force-output osc-stream))

(defun write-osc-msg (message osc-datagram)
  (comm+:send-message osc-datagram (apply #'osc:encode-message message)))

(defun write-osc-bundle (bundle osc-stream &optional (time-tag :now))
  (write-sequence (osc:encode-bundle bundle time-tag) osc-stream)
  (force-output osc-stream))

;(defun write-osc-bundle (bundle osc-stream &optional (time-tag :now))
;  (write-sequence (osc:encode-bundle bundle time-tag) osc-stream)
;  (force-output osc-stream))

(defun write-osc-bundle (bundle osc-datagram &optional (time-tag :now))
  (comm+:send-message osc-datagram (osc:encode-bundle bundle time-tag)))

;(defun open-osc-out-stream (host port)
;  (comm+:open-udp-stream host port :direction :output :element-type '(unsigned-byte 8)))

(defun open-osc-out-stream (host port)
  (comm+:connect-to-udp-server host port))

;(defun close-osc-stream (osc-stream)
;  (close osc-stream))

(defun close-osc-stream (osc-datagram)
  (comm+:close-datagram osc-datagram))

 
(defun start-osc-server (host port &optional (function #'print-incoming-osc-msg))
 (comm+:start-udp-server :address host :service port :function function
                        :process-name (format nil "OSC receiver on ~S ~S" host port)) )

(defun stop-osc-server (server)
  (comm+:stop-udp-server server :wait t))

(defun print-incoming-osc-msg (msg)
  (capi::beep-pane)
  (format cl-user::*stdout* "Osc received...... ~a~%" (decode-msg-or-bundle msg))
  nil)

(defun decode-msg-or-bundle (msg-or-bundle)
  (if (= (elt msg-or-bundle 0) 35)
      (osc:decode-bundle msg-or-bundle)
    (osc:decode-message msg-or-bundle)))


;; modifications to osc external library -----------------------------------------------
;; just add force-string for symbols

(defun force-string (data) (string-downcase (string data)))

(defun encode-message (address &rest data)
  "encodes an osc message with the given address and data."
  (concatenate '(vector (unsigned-byte 8))
	       (encode-address (force-string  address))
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
          (symbol (write-to-vector #\s))
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
          (symbol (setf lump (cat lump (encode-string (force-string x)))))
	  (t (enc encode-blob))))
      lump)))

;---------------------------------------------------------------------------------


#|


;; start a server for processing incoming osc messages
;; by default, the server will just print the incoming messages on cl-user::*stdout*
;; you can bind cl-user::*stdout* to *standard-output*

(setf os (start-osc-server "localhost" 3500 ))

;; create a stream for writing out osc msgs
(setf outs (open-osc-out-stream "localhost" 2000))

;; write message
(write-osc-msg '("/toto" 100 200 300 "titi" 2000) outs)
(write-osc-bundle '(("/toto" 100 200 300 "titi" 2000) ("/toto" 100 200 300 "titi" 2000) ("/toto" 100 200 300 "titi" 2000)) outs)

;; close osc out stream
(close-osc-stream outs)

;; stop osc incoming server
(stop-osc-server os)


;; this is how you would define your own osc message processing on input

(defun handle-osc-msg (msg host)
  (setf *curmsg* (decode-msg-or-bundle msg))
  ;;do some  processing on *curmsg*
  (format cl-user::*stdout* "~a~%" (mapcar #'type-of *curmsg*))
  ;; return nil so the udp server does not send back an message to the sender
  nil)

(setf os (start-osc-server "localhost" 3500 #'handle-osc-msg))

(stop-osc-server os)


|#
