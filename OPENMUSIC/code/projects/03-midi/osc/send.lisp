

(in-package :om)

;;-----------------------------------------------------------------------------
;; The OpenMusic send function. This is for the visual programming style.
;; If you're an ascii-addict, check out send-osc-bundle.
;;-----------------------------------------------------------------------------
(om::defmethod! osc-send (bundle host port)
  :icon 611
  :initvals '(("/test" 0) "127.0.0.1" 3000)
  :indoc '("OSC message" "IP address" "port number")
  :doc "Sends the given and OSC message (<bundle>) port <port> of <host>.

An OSC message consists of a string (URL-style symbolic naming) followed by numerical parameters. Its is formatted as a list in OM.
See http://opensoundcontrol.org/introduction-osc

<bundle> can also contain a list of messages (list of lists) to be sent simultaneously.

Note: default host 127.0.0.1 is the 'localhost', i.e. the message is send to the local computer address.
"
  (om-send-osc-bundle port host bundle)
)


