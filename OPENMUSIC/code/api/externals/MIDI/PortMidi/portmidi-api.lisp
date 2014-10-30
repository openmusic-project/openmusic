

(in-package :om-midi)

(defun om-start-portmidi ()
  #-linux (setf pm::*libportmidi-pathname* (oa::om-lib-pathname pm::*libportmidi-pathname*))
  (pm::load-portmidi-lib)
  (when pm::*libportmidi* 
    ;;; REGISTER AS A MIDI I/O SYSTEM
    (pushnew :portmidi om-midi::*midi-systems*)
    (pm::pm-initialize))
  pm::*libportmidi*)

(defun om-stop-portmidi ()
  (when pm::*libportmidi* (pm::pm-terminate)))


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
     collect (describe-device i)))

; (list-devices)

;;;========================================
;;; START/STOP: NOT USEFUL AT THE END SINCE MIDI PORTS WILL STAY OPEN
;;;========================================

;(defvar *midi-out-stream* nil)
; (portmidi-start)

(defun portmidi-start (&optional (buffersize 1024))
  (unless (pm-time-started) (pm-time-start))
  ;(unless *midi-out-stream*
  ;  (handler-bind ((error #'(lambda (err)
  ;                            (print "PortMidi: Could not open MIDI device!!!")
  ;                            (abort))))
  ;    (setf *midi-out-stream* (pm::pm-open-output 0 buffersize 0))))
  )

(defun portmidi-stop ()
  ;(pm::pm-close *midi-out-stream*)
  ;(setf *midi-out-stream* nil)
  (when (pm-time-started) (pm-time-stop))
  t)

;;; used, e.g. to refresh the list of devices
(defun portmidi-restart ()
  (portmidi-close-all-midi-ports)
  (om-stop-portmidi)
  (pm::pm-initialize)
  (print "PortMIDI reinitialized.")
  (let ((devices (list-devices)))
    (if devices 
        (print (format nil "PortMIDI - devices detected:~%~{~A~^~%~}" 
                       (mapcar #'(lambda (device) (format nil "~s [~A]" (nth 4 device) 
                                                         (cond ((and (nth 6 device) (nth 8 device)) "IN-OUT")
                                                               ((nth 6 device) "INPUT")
                                                               ((nth 8 device) "OUTPUT")
                                                               (t "-"))))
                               devices)))
      (print "No MIDI devices detected"))
    ))


;;;========================================
;;; MESSAGES
;;; FROM CL-PORT-MIDI
;;;========================================


(defun pm-channel (channel)
  "=> the bitmask for 'channel"
  (ash 1 channel))

(defun pm-channels (&rest channels)
  "=> a bitmask for multiple channels"
  (apply #'+ (mapcar #'channel channels)))

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
;; (make-message* 9 0 60 100)
;; ex. Note-OFF on channel 
;; (make-message* 8 channel note velocity)

;; (message-status 14208)

(defun message-type (status)
  (ash (logand status #xf0) -4))

(defun message-channel (status)
  (logand status #x0f))

;;;========================================
;;; MESSAGES - OUR API
;;;========================================

(defvar *midi-typenum-table*
  '((:KeyOff #b1000)
    (:KeyOn #b1001)
    (:KeyPress #b1010)
    (:CtrlChange #b1011)
    (:ProgChange #b1100)
    (:ChanPress #b1101)
    (:PitchBend #b1110)
    ))

(defvar *midi-meta-types*
  '(:TimeSign :SeqNum :Textual :Copyright :SeqName
    :InstrName :Lyric :Marker :CuePoint :ChanPrefi
    :EndTrack :Tempo :SMPTEOffset :TimeSign :KeySign
    :Specific))


; (type-to-midi :KeyOff)
(defun type-to-midi (type)
  (or (cadr (find type *midi-typenum-table* :key 'car :test 'equal))
      (and (find type *midi-meta-types*) #xFF)
      (progn (print (format nil "PORTMIDI API - MESSAGE TYPE NOT SUPPORTED: ~S " type)) nil)
      ))

; (midi-to-type 9)
(defun midi-to-type (num)
  (or (car (find num *midi-typenum-table* :key 'cadr :test '=))
      (progn (print (format nil "PORTMIDI API - UNKNOW MESSAGE NUM: ~S " num)) nil)
      ))


(defun portmidi-make-midi-evt (midi-message &optional port)
  (make-midi-evt :type (midi-to-type (message-type (message-status midi-message)))
                 :chan (1+ (message-channel (message-status midi-message)))
                 :port port
                 :fields (list (message-data1 midi-message)
                               (message-data2 midi-message))
                 ))

(defun PMEventBufferMap (fn buf end &optional port)
  (loop for i below end
     for e = (pm::pm-EventBufferElt buf i)
     do (funcall fn (portmidi-make-midi-evt (pm::Event.message e) port) (pm::Event.timestamp e)))
 (values))



(defun 7-msb (14bitval) (logand (ash 14bitval -7) #x7f))
(defun 7-lsb (14bitval) (logand 14bitval #x7f))

(defun make-midi-bytes (type channel vals)
  ;(print (list type channel vals))
  (let ((type-ref (type-to-midi type))
        (v1 (if (listp vals) (car vals) (7-lsb vals)))
        (v2 (if (listp vals) (or (cadr vals) 0) (7-msb vals))))
    (when type-ref (apply 'make-message* (list type-ref channel v1 v2)))))


(defun portmidi-send-evt (evt)
  (when (midi-evt-port evt)
    (let ((out (get-output-stream-from-port (midi-evt-port evt)))
          (bytes (make-midi-bytes (midi-evt-type evt) 
                                  (1- (midi-evt-chan evt)) 
                                  (midi-evt-fields evt))))
    ;(print (midi-evt-type evt))
      (if (and out bytes)
          (pm::pm-write-short out 0 bytes)
        (unless out
          (print (format nil "PortMIDI ERROR: port ~A is not connected. Check MIDI preferences to connect MIDI devices ?" (midi-evt-port evt))) nil)
      ))))



;;;========================================
;;; MIDI IN
;;;========================================
;;;; FROM Taube's CFFI bindings

(defun portmidi-Read (stream *evbuf len) 
  (let ((res (pm::pm-read stream *evbuf len)))
    (if (< res 0)
        (error (pm::pm-get-error-text res))
        res)))

;;; check if there is something in stream
(defun portmidi-Poll (stream)
  (let ((res (pm::pm-poll stream)))
    (cond ((= res 0) nil)
          ((= res 1) t)
          (t (error (pm::pm-get-error-text res))))))

(defstruct midi-in-process (process) (buffer))
 
#|
(defun midi-in-loop (stream buff size &optional (fun #'identity) (port nil))
  (handler-bind ((error #'(lambda (err)
                            (print (format nil "PORTMIDI ERREOR: ~s" err))
                            ;(pm::pm-close stream)
                            (pm::pm-EventBufferFree buff)
                            (mp:process-kill mp::*current-process*)
                            )))
    (let ((out? (get-output-stream-from-port port)))                       
      (loop do
            (if (portmidi-poll stream)
                (let ((n (portmidi-read stream buff size)))
                  (unless (= n 0)
                    (PMEventBufferMap fun buff n port)
                    (when out?
                      (PMEventBufferMap 
                       #'(lambda (message time) (portmidi-send-evt message))
                       buff n port))
                    )
                  (sleep 0.001)))))))
|#


(defun midi-in-loop (stream buff size &optional (fun #'identity) (port nil))
  (UNWIND-PROTECT 
      
      (let ((out? (get-output-stream-from-port port)))                       
        (loop do
              (if (portmidi-poll stream)
                  (let ((n (portmidi-read stream buff size)))
                    (unless (= n 0)
                      (PMEventBufferMap fun buff n port)
                      (when out?
                        (PMEventBufferMap 
                         #'(lambda (message time) (portmidi-send-evt message))
                         buff n port))
                      )
                    (sleep 0.001)))))
    
    (PROGN
      (pm::pm-EventBufferFree buff)
      (mp:process-kill mp::*current-process*)
      )
    
    ))


; (get-input-stream-from-port 0)
(defun portmidi-in-start (portnum function &optional (buffersize 32) redirect-to-port) 
  (multiple-value-bind (in name) (get-input-stream-from-port portnum)
    (if (null in) (progn (print (format nil "PortMidi ERROR: INPUT port ~A is not connected" portnum)) nil)
      (let* ((midibuffer (pm::pm-EventBufferNew buffersize))
             (midiprocess (make-midi-in-process :buffer midibuffer
                                                :process (mp:process-run-function (format nil "MIDI IN (~s)" name) nil
                                                                                  #'midi-in-loop
                                                                                  in midibuffer buffersize function redirect-to-port))))
        midiprocess))))

(defun portmidi-in-stop (midiprocess)
  (when midiprocess 
    (mp:process-kill (midi-in-process-process midiprocess))
    (prog1 
        (pm::pm-EventBufferFree (midi-in-process-buffer midiprocess))
      ;; (when wait
      (mp:process-wait "Wait until MIDI IN process be killed"
                       #'(lambda () (not (mp:process-alive-p (midi-in-process-process midiprocess))))))
    
    ))


#|

(pm::pm-terminate)
(pm::pm-initialize)
(portmidi-restart)
(pm-time-started)
(pm-time-start)

(list-devices)
(pm::pm-get-default-output-device-id)
(pm::pm-get-default-input-device-id)

;;; TEST OUT

(setf *midi-out* (pm::pm-open-output 2 1024 0))

(pm::pm-write-short *midi-out* 0 (make-midi-bytes :keyoff 0 '(62 100)))

(pm:pm-close *midi-out*)

;;; TEST IN

(defparameter indev (pm::pm-open-input 0 256))

; ignore active sensing etc.
(pm::pm-set-filter indev pm::filt-realtime)

; check midi in
(portmidi-Poll indev)

(defparameter midi-buffer (pm::pm-EventBufferNew 32))
(defparameter num (portmidi-Read indev midi-buffer 32))

(PMEventBufferMap 
 (lambda (a b) 
   (print (list "time" b))
   (print (list (message-status a) (message-data1 a) (message-data2 a)))
   (print "====="))
 midi-buffer num)

(pm:EventBufferFree midi-buffer)

(pm::pm-close indev)


;;; recv testing

(defparameter pitch 0)
(loop while (/= pitch 60) do
  (let ((n (pm:Read indev buff 1)))
    (cond ((= n 1)
           (PMEventBufferMap
                (lambda (a b) 
                   b (pm a) (terpri)
                   (setf pitch (pm:Message.data1 a)))
                buff n)))))

; fake
; (defparameter *in* (pm::pm-open-input 0 *portmidi-def-buffer-size*))
; (defun get-input-stream-from-port (n) (values *in* "Oxygen 25"))

(defparameter *test-midi-in*
  (portmidi-in-start 0  
                 (lambda (message time) 
                   ;(print (list "time" time))
                   (print message)
                   ;(portmidi-send-evt message)
                   )
                 1))

(portmidi-in-stop *test-midi-in* :wait t)


|#
