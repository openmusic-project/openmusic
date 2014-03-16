

(in-package :oa)

(defun om-start-portmidi ()
  #-linux (setf pm::*libportmidi-pathname* (om-lib-pathname pm::*libportmidi-pathname*))
  (pm::load-portmidi-lib)
  (when pm::*libportmidi* (pm::pm-initialize))
  pm::*libportmidi*)

(defun om-stop-portmidi ()
  (when pm::*libportmidi* (pm::pm-terminate)))

;;; used, e.g. to refresh the list of devices
(defun om-restart-portmidi ()
  (when pm::*libportmidi* 
    (pm::pm-terminate)
    (pm::pm-initialize)))

;;;========================================
;;; From H. Taube's CFFI binding
;;;========================================

(defmacro with-pm-error (form)
  (let ((v (gensym)))
    `(let ((,v ,form))
       (if (not (= ,v pm::pmNoError))
	   (if (= ,v pm::pmHostError)
               (error "Host error is: ~a" (pm::pm-get-host-error-text))
             (error (pm::pm-get-error-text ,v))
             )))))


;;;========================================
;;; PortTime
;;;========================================

(defun pm-time-started ()
  (let ((res (pm::pt-started)))
    (if (= res 0) nil t)))

(defun pm-time-start ()
  ;; NB: This has to be called before opening output or input.
  ;; it seems that if its called 2x we get an error.
  (unless (pm-time-started)
    (with-pm-error (pm::pt-start 1 (cffi:null-pointer) (cffi:null-pointer))))
  (values))

(defun pm-time-stop ()
  (when (pm-time-started)
    (with-pm-error (pm::pt-stop)))
  (values))


;;;========================================
;;; Devices
;;;========================================

(defun describe-device (id)
  (multiple-value-bind (interf name input output opened)
      (pm::pm-get-device-info id)
    (list id :interface interf :name name :input input :output output :open opened)
    ))

(defun list-devices ()
  (loop for i below (pm::pm-count-devices)
     collect (print (describe-device i))))

;;;========================================
;;; MESSAGES
;;; FROM CL-PORT-MIDI
;;;========================================

(defun make-message (status data1 data2)
  "=> an integer representing a MIDI message
Combines the integers `status`, `data1` and `data2` to a MIDI message."
  (let ((d2 (boole boole-and (ash data2 16) #xFF0000))
	(d1 (boole boole-and (ash data1 8) #xFF00))
	(st (boole boole-and status #xFF)))
    (boole boole-ior d2
	   (boole boole-ior d1 st))))

(defun message-status (msg)
  "=> the status byte of the MIDI message `msg` as an integer"
  (boole boole-and msg #xFF))

(defun message-data1 (msg)
  "=> the first data byte of the MIDI message `msg` as an integer"
  (boole boole-and (ash msg -8) #xFF))

(defun message-data2 (msg)
  "=> the second data byte of the MIDI message `msg` as an integer"
  (boole boole-and (ash msg -16) #xFF))

(defun make-message* (upper lower data1 data2) ;internal
  "=> a MIDI message as an integer
Works like `make-message` but combines `upper` and `lower` to the status byte."
  (let ((status (boole boole-ior
		       (boole boole-and (ash upper 4) #xF0)
		       (boole boole-and lower #xF))))
    (make-message status data1 data2)))

;; ex. Note-ON on channel 
;; (make-message* 9 channel note velocity)
;; ex. Note-OFF on channel 
;; (make-message* 8 channel note velocity)

;;;========================================
;;; MESSAGES - OUR API
;;;========================================

(defun type-to-midi (type)
  (case type
    (:KeyOff #b1000)
    (:KeyOn #b1001)
    (:KeyPress #b1010)
    (:CtrlChange #b1011)
    (:ProgChange #b1100)
    (:ChanPress #b1101)
    (:PitchBend #b1110)
    (otherwise (print (format nil "PORTMIDI API: UNKNOW MESSAGE: ~S " type)) nil)
    ))

(defun make-midi-bytes (type channel values)
  (let ((type-ref (type-to-midi type))
        (v1 (if (listp values) (car values) values))
        (v2 (or (and (listp values) (cadr values)) 0)))
    (when type-ref (apply 'make-message* (append (list type-ref channel) values)))))

; (make-midi-bytes :keyon 1 '(62 100))



#|

(pm::pm-terminate)
(pm::pm-initialize)

(om-restart-portmidi)

(pm-time-started)
(pm-time-start)

(pm::pm-count-devices)
(LIST-DEVICES)


(pm::pm-get-default-output-device-id)
(pm::pm-get-default-input-device-id)


(setf *midi-out* (pm::pm-open-output 2 1024 0))


(pm::pm-write-short *midi-out* 0 (make-midi-bytes :keyoff 0 '(62 100)))

(pm::pm-write-short *midi-out* 0 (pm::note-on 1 61))

(pm:close-midi *midi-out*)


(pm:get-default-input-device-id)
(setf *midi-in* (pm:open-input 1 1024))

(pm:read-midi *midi-in*)

(pm:close-midi *midi-in*)


(note-on 1 61 100)


;;;; AVEC CL-MIDI

(setf midi::*midi-output* (pm::pm-open-output 2 1024 0))
(midi::write-message )

(setf mess (om-midi::make-note-on-message 0 61 100 1))

(midi::key mess)




|#
