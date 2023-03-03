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
;===========================================================================

; SDIF package by J. Bresson

(in-package :om)

;;; CLASSES POUR DES TYPES DE MATRICES SDIF PREDEFINIES

(defclass! SDIF-MTC (sdifmatrix) ()
   (:icon 640)
   (:documentation "All matrix classes for sdif format must iherite from this class."))

(defun getclassfromsignature (sign)
   (cond
    ((or (equal sign :1RES) (equal sign "1RES")) '1RES)
    ((or (equal sign :1FQ0) (equal sign "1FQ0")) '1FQ0)
    ((or (equal sign :1FOF) (equal sign "1FOF")) '1FOF)
    ((or (equal sign :1CHA) (equal sign "1CHA")) '1CHA)
    ((or (equal sign :1DIS) (equal sign "1DIS")) '1DIS)
    ;;;((or (equal sign :EASM) (equal sign "EASM")) 'EASM)
    ))

(defmethod get-signature ((self SDIF-MTC))
   (string (type-of self)))


(defun predef-sdif-array (classname  numRows argkeys)
   (cons-array (make-instance classname) (list nil numRows)
                   (loop for item in argkeys
                         for slot in (get-all-initargs-of-class classname) 
                         append (list (string2initarg (name slot)) item))))


;;; ex getmatrix ...
(defmethod! GetPredefinedMatrix ((self sdifFile) fNum mNum)
   :icon 639
   :doc "Get a lisp array filled wih values of the aNum array of the fNum Frame of <self>."
   (let ((info (multiple-value-list (MatrixInfo self fNum mNum)))
         collist predefclass)
     (setf predefclass (getclassfromsignature (first info)))
     (if predefclass
       (progn
         (setf collist (loop for i from 0 to (- (third info) 1)
                             collect (getcol self fnum mNum i)))
         (predef-sdif-array predefclass (second info) collist))
       (om-beep-msg "there is not a predifined class for this type of matrix."))))




;-----------------------------------------
;Resonnant Filters
;-----------------------------------------

(defclass! 1RES (SDIF-MTC)
   ((Frequency :initform 440.0 :initarg :Frequency :type number :accessor Frequency)
    (Amplitude :initform 0.8 :initarg :Amplitude :type number :accessor Amplitude)
    (BandWidth :initform 0.5 :initarg :BandWidth :type number :accessor BandWidth)
    (Saliance :initform 1 :initarg :Saliance :type number :accessor Saliance)
    (Correction :initform 0 :initarg :Correction :type number :accessor Correction))
   (:icon 640)
   (:documentation
    "Frequency  : Frequency of the resonant filter.
 Amplitude  : Lineary amplitude of the Filter.
 BandWidth  : Bandwidth of the Filter >0. (Hz).
 Saliance   : percentage of error of the parameters 0. á 100.
 Correction : automatic correction of the amplitude compared to other parameters 0. to 1.")) 


 (defmethod get-slot-in-out-names ((self 1RES))
   (values '("self" "numcols" "signature") '(nil 1 "1RES")
           '("object" "number of components" "matrix type")
           '(nil nil nil)))     
      
  
;-----------------------------------------
;fundamental Frequency or excitation of a bank of fofs
;-----------------------------------------

(defclass! 1FQ0 (SDIF-MTC)
   ((Frequency :initform 440.0 :initarg :Frequency :type number :accessor Frequency)
    (Mode :initform 0 :initarg :Mode :type number :accessor Mode)
    (Hit :initform 0 :initarg :Hit :type number :accessor  Hit))
   (:icon 640)
   (:documentation
    "Frequency  : Fundamental frequency of a bank of fofs >0.(Hz).
 Mode       : Mode of excitation (0: Frequency, 1:Hit, 2:Both).
 Hit        : Excitation (Dirac) over a precise time (0: no excitation, 1: excitation).")) 

 (defmethod get-slot-in-out-names ((self 1FQ0))
   (values '("self" "numcols" "signature") '(nil 1 "1FQ0")
           '("object" "number of components" "matrix type")
           '(nil nil nil)))                   

;-----------------------------------------
;Forme d'Onde Formantique
;-----------------------------------------

(defclass! 1FOF (SDIF-MTC)
   ((Frequency :initform 440.0 :initarg :Frequency :type number :accessor Frequency)
    (Amplitude :initform 0 :initarg :Amplitude :type number :accessor Amplitude)
    (BandWidth :initform 0 :initarg :BandWidth :type number :accessor  BandWidth)
    (Tex :initform 0 :initarg :Tex :type number :accessor  Tex)
    (DebAtt :initform 0 :initarg :DebAtt :type number :accessor  DebAtt)
    (Atten :initform 0 :initarg :Atten :type number :accessor  Atten)
    (FofPhase :initform 0 :initarg :FofPhase :type number :accessor FofPhase))
   (:icon 640)
   
   (:documentation
    " Frequency  : Frequency of the fof >0. (Hz).
 Amplitude  : Lineary amplitude of the envelope of the fof.
 BandWidth  : Bandwidth of Fof >0. (Hz).
 Tex        : Time of exitation >0. (seconds).
 DebAtt     : Moment of beginning of the attenuation of the envelope >0. (seconds).
 Atten      : Duration of the attenuation >0. (seconds).
 FofPhase      : Phase of the sinusoid of the fof 0. with 2pi rad.")) 

(defmethod get-slot-in-out-names ((self 1FOF))
   (values '("self" "numcols" "signature") '(nil 1 "1FOF")
           '("object" "number of components" "matrix type")
           '(nil nil nil)))
 

;-----------------------------------------
;Channels
;-----------------------------------------

(defclass! 1CHA (SDIF-MTC)
   ((Channel1 :initform 0.8 :initarg :Channel1 :type number :accessor Channel1)
    (Channel2 :initform 0.8 :initarg :Channel2 :type number :accessor Channel2)
    (Channel3 :initform 0.8 :initarg :Channel3 :type number :accessor Channel3)
    (Channel4 :initform 0.8 :initarg :Channel4 :type number :accessor Channel4))
   (:icon 640)
   (:documentation
    "Gain factors to distribute signals among several channels with corresponding gains (simple panning). 

 If there is more than 4 channels, it is enough to have types declaration frame
 and to make a completion of 1CHA: 1MTD 1CHA { Channel5, Channel6}.")) 

 (defmethod get-slot-in-out-names ((self 1CHA))
   (values '("self" "numcols" "signature") '(nil 1 "1CHA")
           '("object" "number of components" "matrix type")
           '(nil nil nil)))     

;-----------------------------------------
;Distribution of a random process
;-----------------------------------------

(defclass! 1DIS (SDIF-MTC)
   ((Distribution :initform 1 :initarg :Distribution :type number :accessor Distribution)
    (Amplitude :initform 1 :initarg :Amplitude :type number :accessor Amplitude))
   (:icon 640)
   (:documentation
    "Distribution : type of distribution (not yet definite but 0 means equi-distributed)
 Amplitude    : variance of the random process (amplitude). This type can be completed
  by higher order variances.")) 


 (defmethod get-slot-in-out-names ((self 1DIS))
   (values '("self" "numcols" "signature") '(nil 1 "1DIS")
           '("object" "number of components" "matrix type")
           '(nil nil nil)))     

;-----------------------------------------
;
;-----------------------------------------

(defclass! 1STF (SDIF-MTC)
   ((theReal :initform 0 :initarg :theReal :type number :accessor theReal)
    (theImagiary :initform 0 :initarg :theImagiary :type number :accessor theImagiary))
   (:icon 640)
   (:documentation "
Data coming out of a discrete short-term time-domain to frequency-domain transform such as an FFT.
")) 

 (defmethod get-slot-in-out-names ((self 1STF))
   (values '("self" "numcols" "signature") '(nil 1 "1STF")
           '("object" "number of components" "matrix type")
           '(nil nil nil)))     

