					;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: C. Agon, G. Assayag, J. Bresson

(in-package :om)


;;==============================
;; LOW LEVEL SUPERCLASS
;;==============================


(defclass internalsound (om-cleanup-mixin)
  ((filename :accessor filename :initarg :filename :initform nil)
   
   (audio-format :accessor audio-format :initarg :audio-format :initform nil)
   (device :accessor device :initarg :device :initform nil)
   (number-of-samples :accessor number-of-samples :initarg :number-of-samples :initform nil)
   (sample-rate  :accessor sample-rate :initarg :sample-rate :initform nil)
   (number-of-channels :accessor number-of-channels :initarg :number-of-channels :initform nil)
   (sample-size :accessor sample-size :initarg :sample-size :initform nil)
   (data-position :accessor data-position :initarg :data-position :initform nil)
   (loaded :accessor loaded :initform nil :initarg :loaded)
   
   (sndbuffer :accessor sndbuffer :initarg :sndbuffer :initform nil)

   (pict-sound :initform nil :accessor pict-sound)
   (pict-spectre :initform nil :accessor pict-spectre)
   (display-array :initform nil :accessor display-array)
   (display-ratio :initform nil :accessor display-ratio)
   (display-builder :initform nil :accessor display-builder)

   (player-data :accessor player-data :initform nil :initarg :player-data)
   ))


(defmethod om-cleanup ((self internalsound))
  (when (pict-sound self)
    ;(print "clean picture")
    (om-kill-picture (pict-sound self))))

(defmethod om-sound-file-name ((self internalsound))
   (filename self))

(defmethod om-sound-sample-rate ((self internalsound))
  (when (or (loaded self) (fill-sound-info self))
    (sample-rate self)))

(defmethod om-sound-sample-size ((self internalsound))
  (when (or (loaded self) (fill-sound-info self))
    (sample-size self)))

(defmethod om-sound-n-samples ((self internalsound))
  (if (or (loaded self) (fill-sound-info self))
    (number-of-samples self)
    0))

(defmethod om-sound-n-channels ((self internalsound))
  (when (or (loaded self) (fill-sound-info self))
    (number-of-channels self)))

(defmethod om-sound-data-pos ((self internalsound))
  (when (or (loaded self) (fill-sound-info self))
    (data-position self)))

(defmethod om-sound-format ((self internalsound))
  (when (or (loaded self) (fill-sound-info self))
    (audio-format self)))


(defmethod fill-sound-info ((self internalsound))
  (when (and (filename self) (probe-file (filename self)))
    (print (format nil "Loading sound file : ~s" (namestring (filename self))))
    (multiple-value-bind (format nch sr ss size skip)
        (om-audio::om-sound-get-info (namestring (filename self)))
      (if (and format size nch (> size 0) (> nch 0))
          (progn 
            (setf (audio-format self) format
                  (number-of-samples self) size
                  (number-of-channels self) nch
                  (sample-size self) ss
                  (sample-rate self) sr
                  (data-position self) skip
                  (sndbuffer self) nil)
            (setf (loaded self) t)
            ;(unless (om-supported-audio-format format)
            ;  (print (format nil "Warning : unsupported audio format ~A" format))
            ;  (setf (loaded sound) :error))
            )
        (progn 
          (print (format nil "Error loading file ~s" (filename self)))
          (setf (loaded self) :error))))
    (loaded self)))


;;==============================
;; SOUND BUFFER
;;==============================

(defparameter *default-internal-sample-size* :float)

(defclass om-sound-data (simple-container om-cleanup-mixin)
  ((buffer :accessor buffer :initform nil :initarg :buffer)
   (smpl-type :accessor smpl-type :initform nil :initarg :smpl-type)
   (tracknum :accessor tracknum :initform 0 :initarg :tracknum :type fixnum)
   (size :accessor size :initform nil :initarg :size :type fixnum)
   (nch :accessor nch :initform nil :initarg :nch :type fixnum)
   (sr :accessor sr :initform nil :initarg :sr :type fixnum)))

(defmethod initialize-instance :after ((self om-sound-data) &rest args)
  (unless (smpl-type self)
    (when (buffer self) (setf (smpl-type self)
                              (fli::pointer-element-type (buffer self))))
    ))

(defmethod om-cleanup ((self om-sound-data))
  (when (buffer self)
    ;(print (list "clean buffer" self))
    (om-free-pointer (buffer self))))


(defmethod get-obj-dur ((self om-sound-data)) (round (size self) (/ (sr self) 1000.0)))

(defmethod extent->ms ((self om-sound-data)) (round (size self) (/ (sr self) 1000.0)))

(defmethod allowed-in-maq-p ((self om-sound-data)) t)

(defmethod Class-has-editor-p ((self om-sound-data)) nil)

(defmethod get-om-sound-data ((self string) &optional track)
   (multiple-value-bind (buffer format channels sr ss size skip)
       (om-audio::om-get-sound-buffer self *default-internal-sample-size*)
     (make-instance 'om-sound-data 
                    :type *default-internal-sample-size*
                    :buffer buffer
                    :tracknum (or track 0)
                    :size size
                    :nch channels
                    :sr sr)))

(defmethod get-om-sound-data ((self pathname) &optional track)
  (get-om-sound-data (namestring self) track))

(defmethod set-buffer-from-file ((self internalsound) filename)
  (setf (sndbuffer self) (get-om-sound-data self)))

(defmethod get-om-sound-data ((self om-sound-data) &optional track)
  (let ((rep self))
    (when track (setf (track rep) track))
    rep))

;;==============================
;; OM CLASS
;;==============================

(defclass! sound (simple-score-element internalsound)
  ((tracknum :accessor tracknum :initarg :tracknum :initform 0 :documentation "a track index for multichannel mixing (0 = no specific track)")
   (markers :accessor markers :initarg :markers :initform nil :documentation "a list of markers (s)")
   (vol :accessor vol :initform 100)  
   (pan :accessor pan :initform 0))
   (:icon 287)
   (:documentation "Sound file object.

A sound is initialized with a pathname corresponding to an audio file on the disk.
Connect a pathname to the first input (<self>) to initialize the instance. 
If it is unlocked and unconnected, evaluating the box will open a file chooser dialog and allow the selection of a sound file to load.

The other inputs/outputs correspond to :
- <tracknum> = a track number on which the sound will be dispatch at playback. Tracknum can also be changed from the sound editor.
- <markers> = a list of markers (time in seconds). The markers can also be added/moved/removed from the sound editor.

NOTE: These inputs can be connected and will be evaluated even if something is connected to he <self> input of the box.

Press 'space' to play/stop the sound file.
"))


(defmethod markers ((self sound)) 
  (sort (slot-value self 'markers) '<))

(defmethod get-om-sound-data ((self sound) &optional (track 0))
  (and (om-sound-file-name self)
       (get-om-sound-data (om-sound-file-name self) track)))

(defparameter *default-sound-player* #-linux :libaudiostream #+linux :jackaudio)

(defmethod default-edition-params ((self sound))
  (pairlis '(outport inport player
             zoom grillestep mode winsize winpos show-spectrum) 
           (list nil nil *default-sound-player*
                 1 nil 0 (om-make-point 370 280) (om-make-point 400 20) nil)))



(defmethod initialize-instance :after ((self sound) &rest args)
  (setf (Qvalue self) 1000)
  )

(defmethod extent ((self sound))
  (setf (extent self) (sound-dur-ms self))
  (call-next-method))

;(defmethod extent ((self sound))
;  (unless (slot-value self 'extent) 
;    (setf (extent self) (sound-dur-ms self)))
;  (call-next-method))

(defmethod sound-name ((self sound))
   (pathname-name (om-sound-file-name self)))

(defmethod get-name ((self sound))
  (or (get-filename (om-sound-file-name self)) ""))
    
(defmethod sound-path ((self sound) )
   (om-sound-file-name self))

(defmethod sound-update-pict ((self sound) pict)
  (when (pict-sound self) (om-kill-picture (pict-sound self)))
  (setf (pict-sound self) pict))

(defmethod get-sound-pict ((self sound))
  (pict-sound self))

(defmethod real-dur ((self sound)) 
  (round (extent->ms self)))

(defmethod real-duration ((self sound) time) 
  (values time (+ time (round (extent->ms self)))))

(defmethod! set-channel ((self sound) chan)
  (setf (tracknum self) chan))
 
(defmethod copy-container ((self sound) &optional (pere ()))
  (let ((snd (if (om-sound-file-name self) 
                 (let ((copy (load-sound-file (om-sound-file-name self)))
                       (slots  (class-instance-slots (find-class 'simple-container))))
                   (setf (slot-value copy 'parent) pere)
                   (loop for slot in slots
                         when (not (eq (slot-definition-name slot) 'parent))
                         do (setf (slot-value  copy  (slot-definition-name slot))
                                  (copy-container (slot-value self  (slot-definition-name slot)) copy)))
                   copy)
               (make-instance 'sound))))
    (setf (tracknum snd) (tracknum self))
    (setf (markers snd) (markers self))
    snd))


(defmethod copy-container ((self sound) &optional (pere ()))
  (let ((snd (if (om-sound-file-name self) 
                 (let ((copy (load-sound-file (om-sound-file-name self)))
                       (slots  (class-instance-slots (find-class 'simple-container))))
                   (setf (slot-value copy 'parent) pere)
                   (loop for slot in slots
                       when (not (eq (slot-definition-name slot) 'parent))
                       do (setf (slot-value  copy  (slot-definition-name slot))
                            (copy-container (slot-value self  (slot-definition-name slot)) copy)))
                   copy)
               (make-instance 'sound))))
    (setf (tracknum snd) (tracknum self))
    (setf (markers snd) (markers self))
    (setf (pan snd) (pan self))
    (setf (vol snd) (vol self))
    (setf (pict-spectre snd) (pict-spectre self))
    (when (< *om-version* 6.08) (setf (pict-spectre? snd) (pict-spectre? self))) 
    snd))


;;; copie : meme ptrs (pour le maquette play)
(defun copy-sound-file (sound)
  (let ((thesound (make-instance (type-of sound)
                    :filename (om-sound-file-name sound)
                    :audio-format (om-sound-format sound)
                    :number-of-samples (om-sound-n-samples sound)
                    :sample-rate (om-sound-sample-rate  sound)
                    :number-of-channels (om-sound-n-channels sound)
                    :data-position (om-sound-data-pos sound)
                    )))
    (if thesound
      (progn
        (setf (pict-sound thesound) (pict-sound sound))
        (setf (tracknum thesound) (tracknum sound))
        (setf (markers thesound) (markers sound))
        (setf (vol thesound) (vol sound))
        (setf (pan thesound) (pan sound))
        (setf (extent thesound) nil)
        )
      (om-message-dialog (format nil "Cannot copy the file : ~S" (namestring name)))
      )
    thesound))

;;; copy for play in maq
(defmethod maq-copy-container ((self sound)  &optional (pere ())) 
  (let ((copy self) ;(copy-sound-file self))
        (slots  (class-instance-slots (find-class 'simple-container))))
    (setf (slot-value copy 'parent) pere)
    (loop for slot in slots
          when  (not (eq (slot-definition-name slot) 'parent))
          do (setf (slot-value  copy  (slot-definition-name slot))
                   (copy-container (slot-value self (slot-definition-name slot)) copy)))
    copy))

;;; maquette interface
(defmethod allowed-in-maq-p ((self sound))  (good-val-p? self))
(defmethod get-obj-dur ((self sound)) (extent self))
(defmethod allow-strech-p ((self sound) (factor number)) nil)


(defmethod execption-save-p ((self sound)) 'sound)

(defmethod save-exepcion ((self sound)) 
  (and (om-sound-file-name self)
        (register-resource :sound (om-sound-file-name self))
        `(let ((thesound (load-sound ,(om-save-pathname-relative (om-sound-file-name self))
                                     ,(tracknum self)
                                     ,(vol self)
                                     ,(pan self)
                                     )))
           (when thesound
             (setf (markers thesound) ',(markers self)))
           thesound)))


;;;========
;;; LOAD
;;;========   
 
(defun load-sound-file (name &optional track)
  (let ((sound nil))
    ;;; (om-print (string+ "Loading sound file : " (om-namestring name)))
    (if (probe-file name)
        (progn 
          (setf sound (make-instance 'sound :filename name))
          (build-display-array sound)
          (setf (extent sound) nil))
      ;;; (om-supported-audio-format (om-sound-format thesound)))
      (progn 
        (setf sound (make-instance 'sound))
        (om-message-dialog (format nil (om-str :file-not-found) (namestring name)))))
    sound))


;;; not used for the moment
(defmethod build-display-array-dynamic ((self sound))
  (let* ((ratio 128)
         (size (om-sound-n-samples self))
         (channels (om-sound-n-channels self))
         (array-width (ceiling size ratio)))
;(ratio (round (om-sound-n-samples self) 2000)))) pour un ratio variable. 2000 car nbpix d'un écran environ
;Bien pour les petits fichiers mais mauvais dès que trop grand car bascule trop vite sur la lecture fichier
    (setf (display-ratio self) ratio
          (display-builder self) (om-run-process 
                                  "DisplayArrayBuilder" 
                                  #'(lambda (snd)
                                      ;(setf (display-array snd) (om-audio:om-get-sound-display-array (namestring (filename snd)) ratio))
                                      (setf (display-array snd) 
                                            (make-array (list channels (ceiling size ratio))
                                                        :element-type 'single-float :initial-element 0.0 :allocation :static))
                                      (fli:with-dynamic-lisp-array-pointer 
                                          (ptr (display-array snd) :type :float)
                                        (om-audio:om-fill-sound-display-array (namestring (filename snd)) ptr ratio))
                                      (sound-get-best-pict snd)
                                      (setf (display-builder self) nil)
                                      (print (format nil "~A Loaded..." (filename self)))) self))))


(defmethod build-display-array ((self sound))
  (let* ((ratio 128)
         (size (om-sound-n-samples self))
         (channels (om-sound-n-channels self))
         (array-width (ceiling size ratio)))
;(ratio (round (om-sound-n-samples self) 2000)))) pour un ratio variable. 2000 car nbpix d'un écran environ
;Bien pour les petits fichiers mais mauvais dès que trop grand car bascule trop vite sur la lecture fichier
    (setf (display-ratio self) ratio)
    "DisplayArrayBuilder" 
    (funcall 
     #'(lambda (snd)
         (setf (display-array snd) 
               (make-array (list channels (ceiling size ratio))
                           :element-type 'single-float :initial-element 0.0 :allocation :static))
         (fli:with-dynamic-lisp-array-pointer 
             (ptr (display-array snd) :type :float)
          (om-audio:om-fill-sound-display-array (namestring (filename snd)) ptr ratio))
         (sound-get-best-pict snd)
        ;(setf (display-builder self) nil)
         (print (format nil "~A Loaded..." (filename self))))
     self)))



(defmethod sound-get-display-array-slice ((self sound) nbpix start-time end-time)
  (when (display-array self)
    (let* ((sr (* (om-sound-sample-rate self) 0.001))
           (maxtime (round (om-sound-n-samples self) sr))
           (targettime (- end-time start-time))
           (timeratio (float (/ targettime maxtime 1.0)))
           (win (display-ratio self))
           (maxnbpix (round (* timeratio (cadr (array-dimensions (display-array self))))))
           (start-smp (floor (* start-time sr)))
           (end-smp (ceiling (* end-time sr)))
           (start (floor (* start-time sr) win))
           (end (ceiling (* end-time sr) win))
           (stoppoint (1- (cadr (array-dimensions (display-array self)))));(+ start (1- maxnbpix)))
           (maxi 0.0)
           result)
      (cond ((= nbpix maxnbpix)
             (setq result (display-array self)))
            ((< nbpix maxnbpix)
             (let* ((step (/ (- end start) nbpix 1.0)))
               (setq result (make-array (list (om-sound-n-channels self) nbpix) :element-type 'single-float :initial-element 0.0))
               (dotimes (c (om-sound-n-channels self))
                 (dotimes (i nbpix)
                   (dotimes (j (round step))
                     (setq maxi (max maxi (aref (display-array self) c (min stoppoint (round (+ start j (* i step))))))))
                   (setf (aref result c i) maxi)
                   (setq maxi 0.0)))
               result))
            ((> nbpix maxnbpix)
             (setq result (om-audio:om-get-sound-display-array-slice (namestring (filename self)) nbpix start-time end-time))))
      (values result (< (cadr (array-dimensions result)) nbpix)))))

(defmethod* get-sound () 
   :initvals nil
   :indoc nil
   :doc "load a Aiff file"
   :icon 148
  (let ((name (om-choose-file-dialog 
               :directory (def-load-directory)
               :prompt (om-str :choose-snd) :types (list (om-str :all-files) "*.*" 
                                                         (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif" 
                                                         (format nil (om-str :file-format) "WAV") "*.wav")))
        (rep nil))
    (when name
      (setf *last-loaded-dir* (pathname-dir name))
      (setf rep (load-sound-file name)))
    (unless rep (om-abort))
    rep))

; (get-sound)

(defmethod get-obj-from-file ((type (eql 'aiff)) filename)
  (load-sound-file filename))
(defmethod get-obj-from-file ((type (eql 'aif)) filename)
  (load-sound-file filename))
(defmethod get-obj-from-file ((type (eql 'wav)) filename)
  (load-sound-file filename))

(defmethod* objfromobjs ((self null) (type sound))
   (make-instance 'sound))

(defmethod* objfromobjs ((self string) (type sound))
   (load-sound-file self))

(defmethod* objfromobjs ((self pathname) (type sound))
   (load-sound-file self))

;(defun load-sound (name &optional track)
;  (let ((snd (om-load-if name 'load-sound-file)))
;    (unless snd (setf snd (make-instance 'sound :filename name)))
;    (when (and snd track) (setf (tracknum snd) track))
;    snd))

(defun load-sound (name &optional track vol pan)
  (let ((snd (om-load-if name 'load-sound-file)))
    (unless snd (setf snd (make-instance 'sound :filename name)))
    (when (and snd track) (setf (tracknum snd) track))
    (when (and snd vol) (setf (vol snd) vol))
    (when (and snd pan) (setf (pan snd) pan))
    snd))


;======================
; EDITOR
;======================

(defmethod Class-has-editor-p ((self sound)) t)

(defmethod get-initval ((self sound)) (make-instance 'sound))

(defmethod default-obj-box-size ((self sound)) (om-make-point 80 50))

(defmethod get-editor-class ((self sound)) 'soundEditor)

;;; boxeditcall evaluation
(defmethod cons-new-object ((self sound) args objs)
  (let ((rep (call-next-method)))
    (when rep
      (setf (tracknum rep) (if (integerp (nth 1 args)) (nth 1 args) 0))
      (when (consp (nth 2 args)) (setf (markers rep) (nth 2 args)))
    rep)))




;;; default value at box evaluation
(defmethod make-one-instance ((self sound) &rest slots-vals) 
  (let ((snd (get-sound)))
    (when (and snd (car slots-vals)) 
      (setf (tracknum snd) (car slots-vals))
      (setf (markers snd) (cadr slots-vals)))
    snd))

(defmethod object-remove-extra ((self sound) box)
  (player-cleanup (get-edit-param box 'player) self))

(defmethod player-cleanup (player snd) nil)
  


;============
; OM METHODS
;============


;;; reads a sample at position position in <self>
(defmethod read-sound-sample ((self sound) position &optional (type :float))
  (multiple-value-bind (buffer format nch sr ss size skip)
      (om-audio::om-get-sound-buffer (filename self) type)
    (when buffer
      (let ((snddata (loop for chan from 0 to (- nch 1) collect 
                           (om-read-ptr buffer (+ position chan) type))))
        (om-free-pointer buffer)
        snddata))))


(defmethod* sound-points ((self sound) points &optional (channel 1))
  :initvals '(nil 1000 1)
  :indoc '("a sound object" "number of points" "channel number")
  :doc "Reurns <num> sampled points from the audio waveform of channel <channel> in <self>."
  :icon 221
  (let* ((numdat (om-sound-n-samples  self))
         (numchan (om-sound-n-channels  self))
         (ch (or channel (loop for c from 1 to numchan collect c)))
         (positions (if (listp points) 
                        (sort points '<) 
                      (loop for p from 0 to numdat by (round numdat points) collect p))))
    (if (or (> (list-max (list! ch)) numchan)
            (>  (car (last positions)) numdat))
        (om-message-dialog "Bad input values")
      (multiple-value-bind (buffer format format nch sr ss size skip)
          (om-audio::om-get-sound-buffer (filename self) :float)
        (when buffer
          (let ((data (loop for pos in positions collect
                            (if (listp ch)
                                (loop for c in ch collect (om-read-ptr buffer (+ pos (1- c)) :float))
                              (om-read-ptr buffer (+ pos (1- channel)) :float)))))
            (om-free-pointer buffer)
            data
            )))
      )))


(defmethod! sound-dur ((sound pathname))
  :icon 221
  :initvals '(nil)
  :indoc '("a sound object or file pathname")
  :doc "Returns the duration of <sound> in seconds."
  (let ((thesound (make-instance 'sound :filename sound)))
    (sound-dur thesound)))

(defmethod! sound-dur ((sound string))
  (when (probe-file (pathname sound))
    (sound-dur (pathname sound))))


;;; NON
;(defmethod! sound-dur ((sound sound))
; (if (and sound (om-sound-n-samples-current sound) las-srate
;          (> las-srate 0))
;     (float (/ (om-sound-n-samples-current sound) las-srate))
;   0))

(defmethod! sound-dur ((sound sound))
   (if (and sound (om-sound-n-samples sound) (om-sound-sample-rate sound)
            (> (om-sound-sample-rate sound) 0))
       (float (/ (om-sound-n-samples sound) (om-sound-sample-rate sound)))
     0))


(defmethod! sound-dur-ms ((sound t))
  :initvals '(nil)
  :indoc '("a sound object or file pathname")
  :doc "Returns the duration of <sound> in milliseconds."
  :icon 221
  (round (* 1000 (sound-dur sound))))



;;;===================================
;;; PICTURE A PARTIR DU SOUND FILE
;;;===================================

(defmacro om-sound-protect (sound &body body)
  `(if (equal (loaded ,sound) :error)
       (progn 
         (print (format nil "sound ~s is disabled because of previous errors" (namestring (filename ,sound))))
         nil)
     (or (ignore-errors ,@body)
         (progn
           (print (format nil "error in sound ~s" (namestring (filename ,sound))))
           (setf (loaded ,sound) :error)
           nil))))

; (create-snd-pict "/Users/bresson/_SHARED-FILES/WORKSPACES/my-workspace/in-files/africa.aiff" 1000)

;;;CONS SND PICT WITH MAX DETECTION
(defmethod create-snd-pict-max ((self sound) nbpix)
  (let* ((pict-h 256)
         (nch (om-sound-n-channels self))
         (channels-h (round pict-h nch))
         (offset-y (round channels-h 2))
         (data (sound-get-display-array-slice 
                self 512 0 (round (om-sound-n-samples self) (* (om-sound-sample-rate self) 0.001))))
         pixpoint pixpointprev pict)
    (if data
        (setq pict 
              (om-record-pict *om-default-font2* (om-make-point nbpix pict-h)
                (dotimes (i nch)  
                  (om-draw-line 0 (+ (* i channels-h) offset-y) nbpix (+ (* i channels-h) offset-y)))
                (om-with-fg-color nil *om-steel-blue-color*
                  (dotimes (c nch)
                    (setq pixpointprev (round (* offset-y (* 0.99 (aref data c 0)))))
                    (loop for i from 1 to (1- nbpix) do
                          (setf pixpoint (round (* offset-y (* 0.99 (aref data c (min i (1- nbpix)))))))
                          (om-fill-polygon `(,(om-make-point (1- i) (+ offset-y (* c channels-h) pixpointprev))
                                             ,(om-make-point i (+ offset-y (* c channels-h) pixpoint)) 
                                             ,(om-make-point i (+ offset-y (* c channels-h) (- pixpoint))) 
                                             ,(om-make-point (1- i) (+ offset-y (* c channels-h) (- pixpointprev)))))
                          (setq pixpointprev pixpoint))))))
      (setq pict 
            (om-record-pict *om-default-font2* (om-make-point nbpix pict-h)
              (dotimes (i nch)  
                (om-draw-line 0 (+ (* i channels-h) offset-y) nbpix (+ (* i channels-h) offset-y))
                (om-with-fg-color nil (om-make-color 0.8 0.2 0.2) ;;;ICI EN ROUGE
                  (om-draw-line 0 (+ (* i channels-h) offset-y 2) nbpix (+ (* i channels-h) offset-y 2))
                  (om-draw-line 0 (+ (* i channels-h) offset-y -2) nbpix (+ (* i channels-h) offset-y -2)))))))
    pict))


(defmethod sound-get-best-pict ((self sound))
  (when (pict-sound self) (om-kill-picture (pict-sound self)))
  (setf (pict-sound self)
        (or (om-sound-protect self (create-snd-pict-max self 512)) :error)))

(defmethod sound-get-pict ((self sound))
  (unless (equal (pict-sound self) :error)
    (or (pict-sound self)
        (when (and (not (equal :error (loaded self)))
                   (or (loaded self) (ignore-errors (fill-sound-info self))))
          (when (and (pict-sound self) (not (equal (pict-sound self) :error)))
            (pict-sound self))))))
        

;;;================
;;; SOUND BOX 
;;;================
(defmethod good-val-p? ((self sound)) t)

(defmethod valid-sound-p ((self sound))
  (and (om-sound-file-name self) 
       (ignore-errors (probe-file (om-sound-file-name self)))
       (> (get-obj-dur self) 0)))

(defmethod get-type-of-ed-box ((self sound)) 'OMaiffFilebox)

(defclass OMaiffFilebox (OMBoxEditCall) ())

(defmethod gen-code-call ((self OMaiffFilebox))
   (if (connected? (first (inputs self)))
       `(let ((snd (objFromObjs ,(gen-code (first (inputs self)) (second (inputs self))) ,(value self))))
                (when ,(cadr (decode self)) (setf (tracknum snd) ,(cadr (decode self))))
                (when ,(caddr (decode self)) (setf (markers snd) ,(caddr (decode self))))
                snd)
     `(apply 'make-one-instance (list ,(value self) ,.(cdr (decode self))))))


(defmethod numouts ((self OMaiffFilebox)) 3)

(defmethod correct-box-inputs ((class (eql 'sound)) inputs)
  (loop for input in (get-inputs-from-inst (make-instance class))
        for i = 0 then (+ i 1) collect
        (if (and (nth i inputs) (string-equal (name input) (name (nth i inputs))))
            (nth i inputs) input)))

(defmethod get-frame-class ((self OMaiffFilebox)) 'boxsoundframe)

(defclass boxsoundframe (boxEditorFrame) ())

(defmethod om-get-menu-context ((self boxsoundframe))
  (append 
   (boxframe-default-list self)
   (player-menu-item (object self))
   (object-specific-menu (value (object self)))))

(defmethod object-specific-menu ((self sound))
  (list (om-new-leafmenu "Open with external editor..."
                         #'(lambda () (om-cmd-line (string+ *om-open-cmd* " "  (namestring (sound-path self))))))))


(defmethod update-if-editor ((self OMaiffFilebox))
  (when (editorFrame self)
    (om-close-window (om-view-window (editorFrame self))))
    (call-next-method))


;(defmethod sound-get-new-pict ((self sound) path) 
;  (setf (pict-sound self) (or (om-sound-get-new-pict self path) :error))
;  (pict-sound self))
  
(defmethod draw-mini-view ((self t) (val sound))
  (draw-obj-in-rect val 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod draw-obj-in-rect ((self sound) x x1 y y1 edparams view)
  (let ((picture (if (and (pict-spectre self) (get-param edparams :show-spectrum))
                     (thepict (pict-spectre self))
                   (sound-get-pict self))))
    (om-with-focused-view view 
      (if picture
          (let ((dur (/ (om-sound-n-samples self) (om-sound-sample-rate self)))
                (pos x) (w (- x1 x)) (h (h view)))
            (om-with-fg-color view *om-dark-gray-color*
              (om-draw-picture view picture :pos (om-make-point x y) :size (om-make-point (- x1 x) (- y1 y))))
            (om-with-fg-color view *om-steel-blue-color*
              (om-with-line '(2 2)
                (loop for item in (markers self) do
                      (setf pos (+ x (round (* w item) dur)))
                      (om-draw-line pos y pos y1)))))
        (if (om-sound-n-channels self) 
            (om-with-font *om-default-font1*
                          (om-draw-string 5 14 "Loading")
                          (om-draw-string 5 28 (format nil "~A channels" (om-sound-n-channels self))))
          (let ((path (om-sound-file-name self)))
            (when path
              (om-with-font *om-default-font1*
                            (om-draw-string 5 14 (string+ (pathname-name path) (if (stringp (pathname-type path)) (string+ "." (pathname-type path)) "")
                                                          ":"))
                            (om-draw-string 5 28 (if (probe-file path) (om-str :file-error)
                                                   (string+ (format nil (om-str :file-not-found) (pathname-name path)) "..."))))))))
      ;(when (and (om-sound-n-samples self) (zerop (om-sound-n-samples self)))
      ;  (om-draw-string 15 15 (om-str "Error")))
      )))


(defmethod update-miniview ((self t) (type sound))  (om-invalidate-view self t))

;;; pour les maquettes
(defmethod draw-editor-mode ((self sound) view)
  (draw-obj-in-rect self 0 (w view) 0 (h view) (edition-params (object view)) view)
  (draw-carre view nil))

;;; synth maquette
(defmethod cons-maq-mini-pict ((self sound) frame fontsize size)
  (sound-get-pict self))




  
