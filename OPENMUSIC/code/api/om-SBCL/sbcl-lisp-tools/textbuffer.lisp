(in-package :oa)

(defclass ombuffer ()
  ((buffer  :accessor buffer  :initarg :buffer
            :initform (make-array 64 :element-type 'character
                                     :adjustable t :fill-pointer 0))
   (point   :accessor point   :initform 0)
   (window  :accessor window  :initform nil)
   (killed? :accessor killed? :initform nil)))

(defgeneric om-buffer-mark-render (ombuffer))

(defgeneric %om-buffer-render-in-window (ombuffer window)
  (:documentation "Push OMBUFFER's text into WINDOW's editing view; NIL unless WINDOW edits it."))

(defmethod %om-buffer-render-in-window ((ombuffer ombuffer) (window t))
  nil)

(defmethod om-buffer-mark-render ((ombuffer ombuffer))
  (let ((w (window ombuffer)))
    (when w (%om-buffer-render-in-window ombuffer w))))

(defun om-make-buffer ()
  (make-instance 'ombuffer))

(defun om-kill-buffer (ombuffer)
  (setf (fill-pointer (buffer ombuffer)) 0)
  (setf (point ombuffer) 0)
  (setf (killed? ombuffer) t)
  (when (window ombuffer) (om-buffer-mark-render ombuffer)))

(defun om-copy-buffer (ombuffer)
  (let ((new (om-make-buffer)))
    (when (and ombuffer (buffer ombuffer))
      (loop for c across (buffer ombuffer)
            do (vector-push-extend c (buffer new))))
    new))

(defun om-buffer-insert (ombuffer string &optional (position nil))
  (let* ((s     (buffer ombuffer))
         (at    (or position (point ombuffer)))
         (len   (length string))
         (oldsz (length s))
         (newsz (+ oldsz len)))
    (loop repeat len do (vector-push-extend #\Space s))
    (loop for i from (1- newsz) downto (+ at len)
          do (setf (char s i) (char s (- i len))))
    (loop for i from 0 below len
          do (setf (char s (+ at i)) (char string i)))
    (unless position
      (incf (point ombuffer) len))
    (when (window ombuffer) (om-buffer-mark-render ombuffer))))

(defun %om-buffer-set-contents (ombuffer string)
  "Replace the whole buffer text, point at the end."
  (let ((s (buffer ombuffer)))
    (setf (fill-pointer s) 0)
    (loop for c across string do (vector-push-extend c s))
    (setf (point ombuffer) (length s))
    (when (window ombuffer) (om-buffer-mark-render ombuffer))
    string))

(defun om-buffer-insert-newline (ombuffer &optional (position nil))
  (om-buffer-insert ombuffer (string #\Newline) position))

(defun om-buffer-text (ombuffer)
  ;; a FRESH string (vanilla points-to-string): coerce would alias the live
  ;; adjustable vector, letting consumers see it mutate with no notification
  (copy-seq (coerce (buffer ombuffer) 'string)))

(defun om-buffer-substring (ombuffer from &optional to)
  (let* ((s   (buffer ombuffer))
         (end (or to (length s))))
    (subseq s from end)))

(defun om-buffer-char (ombuffer &optional pos)
  (let ((s (buffer ombuffer)))
    (when (and (plusp (length s))
               (< (or pos (point ombuffer)) (length s)))
      (char s (or pos (point ombuffer))))))

(defun om-buffer-size (ombuffer)
  (length (buffer ombuffer)))

(defun om-lines-in-buffer (ombuffer)
  (let ((s (buffer ombuffer))
        (n 1))
    (loop for c across s when (char= c #\Newline) do (incf n))
    n))

(defun om-buffer-lines (ombuffer)
  (let ((s    (buffer ombuffer))
        (acc  '())
        (line (make-array 16 :element-type 'character :adjustable t :fill-pointer 0)))
    ;; copy-seq: LINE is reused across iterations, coerce alone would alias it
    (loop for c across s
          do (cond ((char= c #\Newline)
                    (push (copy-seq (coerce line 'string)) acc)
                    (setf (fill-pointer line) 0))
                   (t (vector-push-extend c line))))
    (push (copy-seq (coerce line 'string)) acc)
    (nreverse acc)))

(defun om-buffer-delete (ombuffer &optional start end)
  (handler-bind ((error
                   (lambda (c)
                     ;; buffer corruption: rebuild instead of dying
                     ;; (vanilla om-LW lw-lisp-tools/textbuffer.lisp: om-buffer-delete)
                     (format *error-output* "Error while cleaning text buffer :~%~A~%"
                             (princ-to-string c))
                     (setf (buffer ombuffer)
                           (make-array 64 :element-type 'character
                                          :adjustable t :fill-pointer 0))
                     (setf (point ombuffer) 0
                           (killed? ombuffer) nil)
                     (when (window ombuffer) (om-buffer-mark-render ombuffer))
                     (return-from om-buffer-delete nil))))
    (let* ((s (buffer ombuffer))
           (n (length s)))
      (cond
        ((null start)
         (setf (fill-pointer s) 0)
         (setf (point ombuffer) 0))
        (t
         (let* ((start (max 0 (min start n)))
                (e     (max start (min (or end n) n)))
                (gap   (- e start)))
           (when (plusp gap)
             (loop for i from start below (- n gap)
                   do (setf (char s i) (char s (+ i gap))))
             (decf (fill-pointer s) gap)
             (when (> (point ombuffer) start)
               (setf (point ombuffer)
                     (max start (- (point ombuffer) gap))))))))
      (when (window ombuffer) (om-buffer-mark-render ombuffer)))))

(defun om-buffer-line-end (ombuffer &optional pos)
  (let* ((s (buffer ombuffer))
         (p (or pos (point ombuffer)))
         (n (length s)))
    (loop for i from p below n
          when (char= (char s i) #\Newline)
          do (return i)
          finally (return n))))

(defun om-buffer-line-start (ombuffer &optional pos)
  (let* ((s (buffer ombuffer))
         (p (or pos (point ombuffer))))
    (loop for i from (1- (min p (length s))) downto 0
          when (char= (char s i) #\Newline)
          do (return (1+ i))
          finally (return 0))))

(defun om-buffer-skip-forward (ombuffer &optional start end)
  (let* ((s     (buffer ombuffer))
         (p     (or start (point ombuffer)))
         (limit (or end   (length s))))
    (loop while (and (< p limit)
                     (let ((line-end (om-buffer-line-end ombuffer p)))
                       (every (lambda (c) (or (char= c #\Space)
                                              (char= c #\Tab)
                                              (char= c #\Return)))
                              (subseq s p (min line-end limit)))))
          do (setf p (1+ (om-buffer-line-end ombuffer p))))
    p))

(defun om-read-string-from-file (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buf (make-array (file-length s) :element-type 'character :fill-pointer 0)))
      (loop for c = (read-char s nil nil)
            while c do (vector-push-extend c buf))
      (coerce buf 'string))))

(defun om-buffer-insert-file (ombuffer path &optional position)
  ;; no POSITION: vanilla REPLACES the buffer, it does not append
  ;; (om-LW lw-lisp-tools/textbuffer.lisp: om-buffer-insert-file)
  (let ((contents (om-read-string-from-file path)))
    (when contents
      (if position
          (om-buffer-insert ombuffer contents position)
          (%om-buffer-set-contents ombuffer contents)))))

(defun om-buffer-write-file (ombuffer path &key (if-exists :supersede))
  (with-open-file (s path :direction :output :if-exists if-exists
                          :if-does-not-exist :create)
    (write-string (om-buffer-text ombuffer) s)))

(defun om-buffer-eval (ombuffer)
  (eval (read-from-string (concatenate 'string "(progn "
                                       (om-buffer-text ombuffer)
                                       ")"))))

