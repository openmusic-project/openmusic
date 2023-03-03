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

;;;===========================
;;; Parameter modulation effects
;;; J. Bresson, M. Stroppa, 2010
;;;===========================


(in-package :om)

(defun train (freqs dur)
 (let ((delta 0.01) ;    if freq<0.01, keep this freq to compute the increment
       (t0 0) (rep (list 0.0))
       (realdur (+ dur 0.0002))) ; to avoid roundup errors
   (loop while (< t0 realdur) do
         (let ((f0 (x-transfer freqs t0)))
           (if (< f0 delta) (incf t0 (/ 1.0 delta))
             (progn 
               (incf t0 (/ 1 (float f0)))
               (pushr t0 rep)))))
   rep))


(defclass processing-function () ())

(defmethod generate-function-bpfs (fun begin end resolution) 
  (om-beep-msg (string+ "Warning: unknown type of processing function: " fun)))

(defmethod generate-function-bpfs ((fun bpf) begin end resolution) (list fun))

(defmethod generate-function-bpfs ((fun list) begin end resolution) 
  (loop for f in fun append (generate-function-bpfs f begin end resolution)))

;(defmethod generate-function-bpfs ((fun bpf) begin end resolution)
;  ; ignore resolution
;  (let* ((pts (point-pairs fun))
;         (xpts (mapcar 'car pts))
;         (bpf (simple-bpf-from-list (om-scale xpts begin end (car xpts) (car (last xpts)))
;                                    (mapcar 'cadr pts)
;                                    'bpf (decimals fun))))
;    (list (if resolution (om-sample bpf (float resolution)) bpf))))


(defmethod! param-process (initval process duration kt op &optional decimals)
  :menuins '((4 (("add" 'a) ("multiply" 'm))))
  :initvals '(nil nil nil nil 'a)
  :indoc '("initial value or BPF" "processing function(s)" "process duration" "control period" "operator" "number of decimals (result)")
  :outdoc '("processed values (BPF)")
  :icon 656
     :doc "Generates a modulated BPF starying from <initval> and <process>.

- <initval> can be either a simple value or a BPF.
- <process> can be either one of VIBRATO or JITTER function, a BPF or a list of several of them.

The resulting BPF is a modulation of <initval> following the curve or functions in <process> applied on a given <duration> and at a given control period <kt>.

- <op> determines whether the effet is applied as a multiplication (Y = X * FACT) of the initial values, or as an addition. The addition actually corresponds to a proportinal modulation Y = X + (X * FACT).

Notes:
- <kt> is not considered in JITTER effects where the period is determined by the jitter frequency(ies). If not specified, it is automatically set for VIBRATO effects at 1/4*f_vib.
- <duration> is not considered in BPF effects where it is determined by the BPF itself (time = x axis in seconds)
"
  (let* ((xinit (if (bpf-p initval) (x-points initval) '(0.0 1.0)))
         (dur (or duration (- (car (last xinit)) (car xinit))))
         (begin (car xinit)) (end (+ begin dur))
         (dec (or decimals (and (bpf-p initval) (decimals initval)) 6))
         (jit-bpfs (generate-function-bpfs process begin end kt))
         (final-jit (if (= 1 (length jit-bpfs)) (car jit-bpfs) (merge-bpfs jit-bpfs)))
         (jit-points (point-pairs final-jit))
         (ope (cond ((equal op 'a) #'(lambda (x a) (+ x (* x a))))
                    ((equal op 'm) #'*)
                    (t op))))
    (if (numberp initval)
        (simple-bpf-from-list (mapcar 'car jit-points)
                              (mapcar #'(lambda (pt) (apply ope (list initval (cadr pt)))) jit-points)
                              'bpf dec)
      (let* ((init-points (point-pairs initval))
             ;(all-points (sort (append init-points jit-points) '< :key 'car))
             (all-points jit-points)
             ; (if resolution (precise-arithm-ser begin end resolution) ...)
             (all-x-points (mapcar 'car all-points)))
        (simple-bpf-from-list
         all-x-points
         (loop for pt in all-points collect
               (let ((val (x-transfer init-points (car pt) dec)))
                 (apply ope (list val (cadr pt)))
                   ))
         'bpf dec)
        ))
    ))

(defun precise-arithm-ser (begin end step)
  (let* ((ndec (length (cadr (multiple-value-list (string-until-char (format nil "~D" step) ".")))))
         (tmpfact (expt 10 ndec)))
    (om/ (arithm-ser (* begin tmpfact) (* end tmpfact) (* step tmpfact)) tmpfact)))


(defun merge-bpfs (bpf-list)
  (let* ((points (mapcar #'point-pairs bpf-list))
        (xpts (sort (remove-duplicates (loop for bp in points append (car (mat-trans bp))) :test '=) '<))
        (dec (list-max (mapcar 'decimals bpf-list))))
    (simple-bpf-from-list xpts
                          (loop for xpt in xpts collect 
                                (apply '+ (mapcar #'(lambda (bpfptlist) (x-transfer bpfptlist xpt dec)) points)))
                          'bpf dec)))  


            
(defclass! jitter-effect (processing-function) 
  ((freq :accessor freq :initarg :freq :initform 20)
   (amp :accessor amp :initarg :amp :initform 1.0))
  (:icon 631))

(defmethod! jitter (freqs amps)
   :initvals '(20 1.0)
   :indoc '("jitter frequency(-ies)" "jitter amplitude(s)")
   :outdoc '("jitter effect")
   :icon 631
   :doc "Generates a JITTER effect (aleatoric pertubation) to be connected to PARAM-PROCESS in order to modulate an input value.

<freqs> and <amps> can be single values or lists of values."
   (make-instance 'jitter-effect :freq freqs :amp amps))

(defmethod generate-function-bpfs ((fun jitter-effect) begin end resolution)
  ;; ignore resolution
  (let* ((freqs (list! (freq fun)))
         (amps (if (consp (amp fun)) (amp fun) (make-list (length freqs) :initial-element (amp fun)))))
    (loop for f in freqs for a in amps collect 
          (let ((x-list (if (bpf-p f)
                            (om+ begin (train f (- end begin))) 
                          (precise-arithm-ser begin end (/ 1.0 f)))))
            (simple-bpf-from-list x-list
                                  (loop for x in x-list collect 
                                        (let ((amp (if (bpf-p a) (x-transfer a x) a)))
                                          (om-random (- amp) amp)))
                                  'bpf 6)))))

(defclass! vibrato-effect (processing-function) 
  ((freq :accessor freq :initarg :freq :initform 6.0)
   (amp :accessor amp :initarg :amp :initform 0.06)
   (ph :accessor ph :initarg :ph :initform 0.0))
  (:icon 632))

(defmethod! vibrato (freqs amps &optional (ph 0.0))
   :initvals '(6.0 0.06 0.0)
   :indoc '("vibrato frequency(-ies)" "vibrato amplitude(s)" "initial phase [rad]")
   :outdoc '("vibrato effect")
   :icon 632
   :doc "Generates a VIBRATO effect (sinusoidal modulation) to be connected to PARAM-PROCESS in order to modulate an input value.

<freqs> and <amps> can be single values or lists of values, <ph> is the initial phase."
   (make-instance 'vibrato-effect :freq freqs :amp amps :ph ph))

(defmethod generate-function-bpfs ((fun vibrato-effect) begin end resolution)
  (let* ((freqs (list! (freq fun)))
         (phase (if (consp (ph fun)) (ph fun) (make-list (length freqs) :initial-element (ph fun))))
         (amps (if (consp (amp fun)) (amp fun) (make-list (length freqs) :initial-element (amp fun))))
         (res (or resolution (om-beep-msg "Warning: no resolution specified for the vibrato. Default= 1 / (4 * f)"))))
    (loop for f in freqs 
          for a in amps
          for p in phase collect 
          (let* ((x-list (precise-arithm-ser begin end (or res (/ 1 (float (* 4 (if (bpf-p f) (list-max (y-points f)) f)))))))
                 (f-vals (if (bpf-p f) 
                            (let* ((fpts (point-pairs f))
                                   (fx (om-scale (mapcar 'car fpts) begin end))
                                   (fy (mapcar 'cadr fpts)))
                              (x-transfer (mat-trans (list fx fy)) x-list))
                           f))
                 (sin-vals (if (listp f-vals)
                               (let ((cycles-beg (train (mat-trans (list x-list f-vals)) (- end begin))))
                                 (loop for c on cycles-beg when (cadr c) append
                                       (let* ((t1 (car c))
                                              (t2 (cadr c))          
                                              (n (length (band-filter x-list (list (list t1 t2)) 'pass))))
                                         (nth 2 (multiple-value-list (om-sample #'(lambda (x) (sin (+ x p))) n 0 (* 2 pi) 3))))))))
                           
                 (a-vals (if (bpf-p a) 
                             (let* ((apts (point-pairs a))
                                    (ax (om-scale (mapcar 'car apts) begin end))
                                    (ay (mapcar 'cadr apts)))
                              (x-transfer (mat-trans (list ax ay)) x-list))
                          a)))

            (simple-bpf-from-list x-list
                                  (loop for x in x-list 
                                        for i = 0 then (+ i 1) 
                                        collect (* (if (listp a-vals) (nth i a-vals) a-vals)
                                                   (if sin-vals (nth i sin-vals)
                                                        (sin (+ p (* 2 pi x (car (list! f-vals))))))
                                                   ))
                                  'bpf 6)))))








