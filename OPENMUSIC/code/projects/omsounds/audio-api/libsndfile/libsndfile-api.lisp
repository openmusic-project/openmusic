;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
;;; authors G. Assayag, C. Agon, J. Bresson
;;===========================================================================

;;; Connection with LIBSNDFILE via CFFI

(in-package :sf)


; (sndfile-get-info "/Users/bresson/_SHARED-FILES/WORKSPACES/my-workspace/in-files/Bassclarinet2.aif")
; (map 'list #'digit-char-p (write-to-string (logior SF_FORMAT_FLOAT sf::sf_format_aiff) :base 16))
; (decode-format (logior SF_FORMAT_FLOAT sf::sf_format_aiff))
;(logior (ash sf::sf_format_aiff 1) (ash b 8) c)


(defun decode-format (sndfile-format)
  (let* ((format_list (map 'list #'digit-char-p (write-to-string sndfile-format :base 16)))
         (first-digit (nth 0 format_list))
         (last-digit (car (last format_list)))
         (ff (cond ((and (= 1 first-digit) (< last-digit 6)) "Wav(int)")
                   ((and (= 1 first-digit) (>= last-digit 6)) "Wav(float)")
                   ((and (= 2 first-digit) (< last-digit 6)) "AIFF(int)")
                   ((and (= 2 first-digit) (>= last-digit 6)) "AIFF(float)")
                   (t "Unknown")))
         (ss (cond ((= 1 last-digit) 8)
                   ((= 2 last-digit) 16)
                   ((= 3 last-digit) 24)
                   ((= 4 last-digit) 32)
                   ((= 5 last-digit) 8)
                   ((= 6 last-digit) 32)
                   ((= 7 last-digit) 64)
                   (t 0))))
    (values ff ss)))

  
;;; READ
;; When opening a file for read, the format field should be set to zero before calling sf_open(). 
(defun sndfile-get-info (path)
  "Returns info about the sound file (not the actual data)."
  (cffi:with-foreign-object (sfinfo '(:struct sf::sf_info))
    (setf (cffi:foreign-slot-value sfinfo '(:struct sf::sf_info) 'sf::format) 0) ; Initialize the slots
    (let ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo)))
      (unwind-protect 
          (let* ((size-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::frames))
                 (size (fli::dereference size-ptr :type :int))
                 (channels-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::channels))
                 (channels (fli::dereference channels-ptr :type :int))
                 (sr-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::samplerate))
                 (sr (fli::dereference sr-ptr :type :int))
                 (format-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::format))
                 (format (fli::dereference format-ptr :type :int))
                 (skip (cffi:foreign-slot-value sfinfo '(:struct sf::sf_info) 'sf::seekable)))
            (multiple-value-bind (ff ss)
                (decode-format format)
              ;;;Detection format and Sample size : cf http://www.mega-nerd.com/libsndfile/api.html#open 
              (values ff channels sr ss size skip))
            )
        (unless (= (sf::sf_close sndfile-handle) 0)
          (error "File ~A not properly closed!" path))
        ))))


(defun sndfile-get-sound-buffer (path &optional (datatype :float))
  "Returns a sound data buffer + info. The sound buffer must be freed."
  (cffi:with-foreign-object (sfinfo '(:struct sf::sf_info))
    (setf (cffi:foreign-slot-value sfinfo '(:struct sf::sf_info) 'sf::format) 0) ; Initialize the slots
    (let ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo)))
      (unwind-protect
          (let* ((size-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::frames))
                 (size (fli::dereference size-ptr :type :int))
                 (channels-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::channels))
                 (channels (fli::dereference channels-ptr :type :int))
                 (sr-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::samplerate))
                 (sr (fli::dereference sr-ptr :type :int))
                 (format-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::format))
                 (format (fli::dereference format-ptr :type :int))
                 (skip (cffi:foreign-slot-value sfinfo '(:struct sf::sf_info) 'sf::seekable))
                 (buffer-size (* size channels))
                 (buffer (fli:allocate-foreign-object :type datatype :nelems buffer-size :fill 0))
                 (frames-read 
                  (ignore-errors
                    (case datatype
                      (:double (sf::sf-readf-double sndfile-handle buffer buffer-size))
                      (:float (sf::sf-readf-float sndfile-handle buffer buffer-size))
                      (:int (sf::sf-readf-int sndfile-handle buffer buffer-size))
                      (:short (sf::sf-readf-short sndfile-handle buffer buffer-size))
                      (otherwise (print (concatenate 'string "Warning: unsupported datatype for reading audio data: " (string datatype))))))))
                
            (multiple-value-bind (ff ss)
                (decode-format format)
              (values buffer ff channels sr ss size skip))
            )
        (unless (= (sf::sf_close sndfile-handle) 0)
          (error "File ~A not properly closed!" path))
        )
      )))

;; same function...
;; can we get a de-interleaved buffer with SNDfile ?
(defun sndfile-get-2D-sound-buffer (path &optional (datatype :float))
  "Returns a sound data buffer + info. The sound buffer must be freed."
  (cffi:with-foreign-object (sfinfo '(:struct sf::sf_info))
    (setf (cffi:foreign-slot-value sfinfo '(:struct sf::sf_info) 'sf::format) 0) ; Initialize the slots
    (let ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo)))
      (unwind-protect
          (let* ((size-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::frames))
                 (size (fli::dereference size-ptr :type :int))
                 (channels-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::channels))
                 (channels (fli::dereference channels-ptr :type :int))
                 (sr-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::samplerate))
                 (sr (fli::dereference sr-ptr :type :int))
                 (format-ptr (cffi:foreign-slot-pointer sfinfo '(:struct sf::sf_info) 'sf::format))
                 (format (fli::dereference format-ptr :type :int))
                 (skip (cffi:foreign-slot-value sfinfo '(:struct sf::sf_info) 'sf::seekable))
                 (buffer-size (* size channels))
                 (buffer (fli:allocate-foreign-object :type datatype :nelems buffer-size :fill 0))
                 (frames-read 
                  (ignore-errors
                    (case datatype
                      (:double (sf::sf-readf-double sndfile-handle buffer buffer-size))
                      (:float (sf::sf-readf-float sndfile-handle buffer buffer-size))
                      (:int (sf::sf-readf-int sndfile-handle buffer buffer-size))
                      (:short (sf::sf-readf-short sndfile-handle buffer buffer-size))
                      (otherwise (print (concatenate 'string "Warning: unsupported datatype for reading audio data: " (string datatype))))))))
            
            (multiple-value-bind (ff ss)
                (decode-format format)
              (values buffer ff channels sr ss size skip))
            )
        (unless (= (sf::sf_close sndfile-handle) 0)
          (error "File ~A not properly closed!" path))
        )
      )))


;; WRITE 
(defun sndfile-save-sound-in-file (buffer filename size nch sr resolution format &optional (datatype :float))

  (let* ((res (case resolution
               (8 sf::sf_format_pcm_s8)
               (16 sf::sf_format_pcm_16)
               (24 sf::sf_format_pcm_24)
               (32 sf::sf_format_pcm_32)              
               (otherwise sf::sf_format_pcm_16)))
        (format (logior (case format 
                          (:aiff sf::sf_format_aiff)
                          (:wav sf::sf_format_wav)
                          (:ogg sf::sf_format_ogg)
                          (:flac sf::sf_format_flac)
                          (otherwise sf::sf_format_aiff))
                        res)))
        
    (cffi:with-foreign-object (sfinfo '(:struct sf::sf_info))
      (setf (cffi:foreign-slot-value sfinfo '(:struct sf::sf_info) 'sf::samplerate) sr)
      (setf (cffi:foreign-slot-value sfinfo '(:struct sf::sf_info) 'sf::channels) nch)
      (setf (cffi:foreign-slot-value sfinfo '(:struct sf::sf_info) 'sf::format) format)

      (let ((sndfile-handle-out (sf::sf_open filename sf::SFM_WRITE sfinfo)))
            ;(datatype (fli::pointer-element-type buffer))  ;; not reliable all the time :(

        (unwind-protect
            (let ()
              (case datatype
                (:double (sf::sf-write-double sndfile-handle-out buffer (* nch size)))
                (:float (sf::sf-write-float sndfile-handle-out buffer (* nch size)))
                (:int (sf::sf-write-int sndfile-handle-out buffer (* nch size)))
                (:short (sf::sf-write-short sndfile-handle-out buffer (* nch size)))
                (otherwise (print (concatenate 'string "Warning: unsupported datatype for writing audio data: " (string datatype)))))
              
              (probe-file filename))

          (sf::sf_close sndfile-handle-out))
        
        ))))

