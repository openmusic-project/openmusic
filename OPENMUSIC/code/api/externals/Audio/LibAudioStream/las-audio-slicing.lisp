;===============================================================================================================================================================
;================================================================ LAS AUDIO SLICING API ========================================================================
;===============================================================================================================================================================
(in-package :oa)

(export '(
          las-slice-cut
          las-slice-copy
          las-slice-paste
          las-slice-delete
          ) :om-api)

(defconstant *las-slicing-history-size* 5)
(defvar *las-slicing-history* (make-hash-table))

;===============================================================================================================================================================
;============================================================================ API ==============================================================================
;===============================================================================================================================================================
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
      (progn
        (print "ERROR")
        nil))))


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
      (progn
        (print "ERROR")
        nil))))

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
      (progn
        (print "ERROR")
        nil)))))

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
      (progn
        (print "ERROR")
        nil))))







;//////////////////////////TOOLS////////////////////////////////////////////////////////////////////////////////////////////////
(defun save-slicing-command (type position from to)
  (table-push-on-top (list type position from to) *las-slicing-history* *las-slicing-history-size*))

(defun undo-slicing-command ()
  (table-pop-on-top *las-slicing-history* *las-slicing-history-size*))

(defun table-push-on-top (line table size)
  (loop for i from 0 to (- size 1) do
        (setf (gethash i table) (gethash (+ i 1) table)))
  (setf (gethash (- size 1) table) line))

(defun table-pop-on-top (table size)
  (let ((line-pop (gethash (- size 1) table)))
    (loop for i from (- size 1) to 1 do
        (setf (gethash i table) (gethash (- i 1) table)))
    (setf (gethash 0 table) nil)
    line-pop))