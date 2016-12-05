;;==================================
;;; AUDIO FILE ACCESS TOOLS (R/W)
;;==================================

(in-package :cl-user)

;;; genertal package for low-level audio features
(defpackage :audio-io
  (:use cl-user common-lisp))

(in-package :audio-io)

(export '(
          om-get-sound-info
          om-get-sound-buffer
          om-free-sound-buffer
          om-save-buffer-in-file
          ) :audio-io)

;;==================================
;;; FILE I/O
;;==================================
;;;Convert path
(defun convert-filename-encoding (path)
  #+cocoa (external-format::decode-external-string (external-format::encode-lisp-string (namestring path) :utf-8) :latin-1)
  #-cocoa (namestring path))

;;;Acquire sound infos
(defun om-get-sound-info (path)
  ;; RETURNS format n-channels sample-rate sample-size size skip
  (sf::sndfile-get-info (convert-filename-encoding path)))

;; RETURNS buffer format n-channels sample-rate sample-size size skip
(defun om-get-sound-buffer (path &optional (type :float) (interleaved nil))
  (when (probe-file path)
  (if interleaved 
      (sf::sndfile-get-sound-buffer (convert-filename-encoding path) type)
    (multiple-value-bind (buffer format n-channels sample-rate sample-size size skip)
        (sf::sndfile-get-2D-sound-buffer (convert-filename-encoding path) type)
          (if (= 1 n-channels)
            
              (let ((buffer2 (fli::allocate-foreign-object :type :pointer)))
                (setf (fli:dereference buffer2) buffer)
                (values buffer2 format n-channels sample-rate sample-size size skip))
            
            (unwind-protect 
                
                (let ((buffer2 (fli::allocate-foreign-object 
                                :type :pointer :nelems n-channels
                                :initial-contents (loop for c from 0 to (1- n-channels) collect 
                                                        (fli::allocate-foreign-object 
                                                         :type type 
                                                         :nelems size)))))
                  (dotimes (i size)
                    (dotimes (c n-channels)
                      ;;; apparently FLI:DEREFERENCE iS MUCH FASTER THAN CFFI:MEM-AREF
                      (setf (fli:dereference (fli:dereference buffer2 :type :pointer :index c) :type type :index i)
                            (fli:dereference buffer :type type :index (+ c (* n-channels i))))
                      ;(setf (cffi::mem-aref (cffi::mem-aref buffer2 :pointer c) type i)
                      ;      (cffi::mem-aref buffer type (+ c (* n-channels i))))
                      ))
                  (values buffer2 format n-channels sample-rate sample-size size skip))
              (cffi::foreign-free buffer)))))))

;;;Free a buffer
(defun om-free-sound-buffer (buffer nch)
  (when nch (dotimes (c nch) (fli::free-foreign-object (fli:dereference buffer :type :pointer :index c))))
  (fli::free-foreign-object buffer))

;;;Save a buffer in a file
(defun om-save-buffer-in-file (buffer filename size nch sr resolution format)
  (sf::sndfile-save-sound-in-file buffer filename size nch sr resolution format))



