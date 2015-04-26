

(in-package :om)


(enable-player :osc-player)

(defmethod player-name ((player (eql :osc-player))) "Basic OSC player")   ;;; A short name
(defmethod player-desc ((player (eql :osc-player))) "Send OSC events via UDP")   ;;; a description
(defmethod player-special-action ((player (eql :osc-player))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :osc-player))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :osc-player))) :udp)   ;;; communication protocol


(defmethod player-play-object ((engine (eql :osc-player)) (object oscevent) &key interval params)
  ;(print (format nil "~A : play ~A - ~A" engine object (list (port object) (host object) (bundle object))))
  (om-send-osc-bundle (port object) (host object) (bundle object))
  )


