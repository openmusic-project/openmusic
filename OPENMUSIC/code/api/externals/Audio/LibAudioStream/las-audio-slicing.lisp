;===============================================================================================================================================================
;================================================================ LAS AUDIO SLICING API ========================================================================
;===============================================================================================================================================================
(in-package :oa)

(export '(
          las-slice-cut
          las-slice-copy
          las-slice-paste
          las-slice-delete
          las-slice-sample-cut
          las-slice-seq
          ) :om-api)


;===============================================================================================================================================================
;============================================================================ API ==============================================================================
;===============================================================================================================================================================
(defun las-slice-sample-cut (pointer from to)
  (las::makecutsound pointer (max 0 from) (min (las::getlengthsound pointer) to)))

(defun las-slice-seq (pointer1 pointer2 crossfade)
  (las::makeseqsound pointer1 pointer2 crossfade))

(defun las-slice-cut (pointer from to)
  (let* ((sr-factor (/ las-srate 1000.0))
         (begin (round (* from sr-factor)))
         (end (round (* to sr-factor)))
         (size (las::getlengthsound pointer))
         slice
         slice-before
         slice-after
         result)
    (if (<= end begin) (setf end (+ begin 4)))
    (if (<= begin 0) (setf begin 1))
    (if (>= begin size) (setf begin (- size 4)))
    (if (>= end size) (setf end (- size 1)))
    (if (<= end 0) (setf end 4))
    (if (>= size 5)
        (let ()
          (setf slice (las::MakeCutSound pointer (+ begin 1) (- end 1)))
          (setf slice-before (las::MakeCutSound pointer 0 begin))
          (setf slice-after (las::MakeCutSound pointer end size))
          (setf result (las::MakeSeqSound slice-before slice-after 0))))
    (if (not (las::las-null-ptr-p result)) 
        result
      nil)))


(defun las-slice-copy (pointer from to)
  (let* ((sr-factor (/ las-srate 1000.0))
         (begin (round (* from sr-factor)))
         (end (round (* to sr-factor)))
         (size (las::getlengthsound pointer))
         slice)
    (if (<= end begin) (setf end (+ begin 4)))
    (if (<= begin 0) (setf begin 1))
    (if (>= begin size) (setf begin (- size 4)))
    (if (> end size) (setf end size))
    (if (<= end 0) (setf end 4))
    (setf slice (las::MakeCutSound pointer begin end))
    (if (not (las::las-null-ptr-p slice)) 
        slice
      nil)))

(defun las-slice-paste (pointer position slice)
  (let* ((sr-factor (/ las-srate 1000.0))
         (pos (round (* position sr-factor)))
         (size (las::getlengthsound pointer))
         slice-tmp
         slice-before
         slice-after
         result)
    (if (>= pos size) (setf pos (- size 1)))
    (if (<= pos 0) (setf pos 1))
    (progn
      (setf slice-before (las::MakeCutSound pointer 0 pos))
      (setf slice-after (las::MakeCutSound pointer (+ pos 1) size))
      (setf slice-tmp (las::MakeSeqSound slice-before slice 0))
      (setf result (las::MakeSeqSound slice-tmp slice-after 0))
      (if (not (las::las-null-ptr-p result)) 
          result
        nil))))

(defun las-slice-delete (pointer from to)
  (let* ((sr-factor (/ las-srate 1000.0))
         (begin (round (* from sr-factor)))
         (end (round (* to sr-factor)))
         (size (las::getlengthsound pointer))
         slice-before
         slice-after
         result)
    (if (<= end begin) (setf end (+ begin 4)))
    (if (<= begin 0) (setf begin 1))
    (if (>= begin size) (setf begin (- size 4)))
    (if (>= end size) (setf end (- size 1)))
    (if (<= end 0) (setf end 4))
    (if (>= size 5)
        (let ()
          (setf slice-before (las::MakeCutSound pointer 0 begin))
          (setf slice-after (las::MakeCutSound pointer end size))
          (setf result (las::MakeSeqSound slice-before slice-after 0))
          ))
    (if (not (las::las-null-ptr-p result)) 
        result
      nil)))
