(in-package :om)


(defclass* OSCEvent (simple-score-element)
  ((host :accessor host :initarg :host :initform "127.0.0.1" :documentation "an IP address")
   (port :accessor port :initarg :port :initform 3000 :documentation "a port number")
   (bundle :accessor bundle :initarg :bundle :initform '("/test" 0) :documentation "message formatted as a list"))
   
  (:icon 611)
  (:documentation "An OSC message to be send on port <port> of <host>.

OSCEvent can be instanciated programmatically in OM programs and sent when it is played (e.g. in a maquette, or with 'space' in OM patches).

<bundle> contains the OSC message to send. 
An OSC message consists of a string (URL-style symbolic naming) followed by numerical parameters. Its is formatted as a list in OM.
See http://opensoundcontrol.org/introduction-osc

<bundle> can also contain a list of messages (list of lists) to be sent simultaneously.

Note: default host 127.0.0.1 is the 'localhost', i.e. the message is send to the local computer address.

")
  )


(defmethod allowed-in-maq-p ((self OSCEvent))  t)
(defmethod get-obj-dur ((self OSCEvent)) 0)
(defmethod get-impulsion-pict ((self OSCEvent)) *ctrl-impulsion-pict*)

(defmethod real-dur ((self OSCEvent)) (values 0 0))
(defmethod extent ((self OSCEvent)) 0)
   
(defmethod update-miniview ((self t) (value OSCEvent)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self OSCEvent) x x1 y y1 edparams view)
   (om-with-focused-view view
     (om-draw-rect 0 0 (w view) (h view))
     (om-draw-string 10 20 "OSC Event")
     (om-draw-string 10 30 (string+ (host self) " - " (integer-to-string (port self))))
     (om-draw-string 10 40 (format nil "~s" (bundle self)))
))

(defmethod draw-mini-view  ((self t) (value OSCEvent)) 
   (draw-obj-in-rect value 0 (w self) 0  (h self) (view-get-ed-params self) self))



;;;============================================
;;; OSC PLAYER

(add-player-for-object 'oscevent :osc-player)

(defmethod default-edition-params ((self OSCEvent))
  (pairlis '(player) '(:osc-player)))




