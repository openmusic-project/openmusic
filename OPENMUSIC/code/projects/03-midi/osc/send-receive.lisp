(in-package :om)


(defmethod! om-receive (port &optional (size *om-udp-max-buf-size*))
  :icon 130
  :doc "Waits for an UDP packet to arrive"
  (let ((connec (om-make-receive-connection port))
        (buff (om-make-pointer size :clear t)))
    (om-receive-udp-packet connec buff size)
    (om-close-udp-connection connec)
    buff
    ))

(defmethod! om-send (host port data-buffer size)
  :icon 130
  :doc "Sends data-buffer out to host (host) via port (port)."
  (let* ((conn (om-make-send-connection host port)))
    (om-send-udp-packet conn data-buffer size)
    (om-close-udp-connection conn)
    ))