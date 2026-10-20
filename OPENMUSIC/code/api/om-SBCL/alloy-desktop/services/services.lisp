(in-package :alloy-desktop)

(defclass desktop-service () ())

(defclass clipboard-service (desktop-service) ())
(defclass drag-service (desktop-service) ())
(defclass tooltip-service (desktop-service) ())
(defclass timer-service (desktop-service) ())
(defclass cursor-service (desktop-service) ())
(defclass transient-drawing-service (desktop-service) ())

(defgeneric clipboard-read-text (service))
(defgeneric clipboard-write-text (service text))
(defgeneric start-timer (service interval function &key repeat))
(defgeneric stop-timer (service timer))
(defgeneric set-cursor (service window cursor))
(defgeneric show-tooltip (service window text x y))
(defgeneric hide-tooltip (service window))
