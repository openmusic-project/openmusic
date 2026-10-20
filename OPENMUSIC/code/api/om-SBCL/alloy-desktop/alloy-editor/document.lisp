(in-package :alloy-editor)

;;; ------------------------------------------------------------------ conditions

(define-condition document-error (error)
  ((pathname :initarg :pathname :initform nil :reader document-error-pathname)
   (text :initarg :text :initform "The document could not be read or written."
         :reader document-error-text))
  (:report (lambda (condition stream)
             (format stream "~A~@[ (~A)~]" (document-error-text condition)
                     (document-error-pathname condition))))
  (:documentation "A document could not be read or written."))

(define-condition document-decoding-error (document-error)
  ((encoding :initarg :encoding :initform nil
             :reader document-decoding-error-encoding))
  (:documentation "The bytes of a file are in an encoding this reader does not take."))

(define-condition document-encoding-error (document-error)
  ((encoding :initarg :encoding :reader document-encoding-error-encoding)
   (index :initarg :index :reader document-encoding-error-index)
   (character :initarg :character :reader document-encoding-error-character))
  (:report (lambda (condition stream)
             (format stream "The character ~S at index ~D has no byte in ~A.~@[ (~A)~]"
                     (document-encoding-error-character condition)
                     (document-encoding-error-index condition)
                     (document-encoding-error-encoding condition)
                     (document-error-pathname condition))))
  (:documentation "One character of the text cannot be written in the encoding the document keeps."))

;;; ------------------------------------------------------------------ the document

(defclass document ()
  ((buffer :initarg :buffer :reader document-buffer)
   (pathname :initarg :pathname :accessor document-pathname
             :documentation "The file this document belongs to, or NIL while it belongs to none.")
   (encoding :initarg :encoding :accessor document-encoding
             :documentation ":UTF-8 or :LATIN-1, the one the characters become bytes in.")
   (bom-p :initarg :bom-p :accessor document-bom-p
          :documentation "True while the file opens with a byte-order mark, which is written again as it was read.")
   (eol-style :initarg :eol-style :accessor document-eol-style
              :documentation ":LF or :CRLF: what every Linefeed of the buffer is written as.")
   (write-date :initform nil :reader document-write-date
               :documentation "When the file was last written, as it stood the moment this document last read it or wrote it.")
   (byte-length :initform nil))
  (:default-initargs :buffer nil :pathname nil :encoding :utf-8 :bom-p nil
                     :eol-style :lf)
  (:documentation "A buffer and the shape of the file it came from."))

(defun make-document (&key buffer pathname (encoding :utf-8) bom-p (eol-style :lf))
  "Fresh document over BUFFER, or over an empty buffer of its own, to be written to PATHNAME as ENCODING with EOL-STYLE terminators and BOM-P a byte-order mark."
  (make-instance 'document :buffer (or buffer (make-buffer)) :pathname pathname
                           :encoding encoding :bom-p bom-p :eol-style eol-style))

;;; ------------------------------------------------------------------ the bytes

(defparameter *byte-order-mark* '(239 187 191)
  "The bytes a UTF-8 file may open with.")

(defparameter *foreign-marks* '((255 254) (254 255))
  "The byte-order marks of the UTF-16 encodings, which this reader does not take.")

(defun %read-octets (pathname)
  "Every byte of PATHNAME, as one vector."
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (let* ((octets (make-array (file-length in) :element-type '(unsigned-byte 8)))
           (read (read-sequence octets in)))
      (if (= read (length octets)) octets (subseq octets 0 read)))))

(defun %put-octets (stream octets)
  "Put OCTETS on STREAM."
  (write-sequence octets stream))

(defun %starts-with (octets bytes)
  (and (<= (length bytes) (length octets))
       (loop for byte in bytes
             for index from 0
             always (eql byte (aref octets index)))))

(defun %decode-utf-8 (octets start)
  "The text OCTETS from START make as UTF-8, or NIL when they are not UTF-8."
  (handler-case (babel:octets-to-string octets :start start :encoding :utf-8)
    (babel-encodings:character-decoding-error () nil)))

(defun %decode (octets pathname)
  "(values TEXT ENCODING BOM-P) of OCTETS."
  (cond ((%starts-with octets *byte-order-mark*)
         (let ((text (%decode-utf-8 octets 3)))
           (unless text
             (error 'document-decoding-error :pathname pathname :encoding :utf-8
                    :text "The file opens with a UTF-8 mark and its bytes are not UTF-8."))
           (values text :utf-8 t)))
        ((some (lambda (mark) (%starts-with octets mark)) *foreign-marks*)
         (error 'document-decoding-error :pathname pathname :encoding :utf-16
                :text "The file opens with a UTF-16 mark, which this reader does not take."))
        (t
         (let ((text (%decode-utf-8 octets 0)))
           (if text
               (values text :utf-8 nil)
               (values (babel:octets-to-string octets :encoding :latin-1) :latin-1 nil))))))

;;; ------------------------------------------------------------- the terminators

(defun %eol-style (text)
  ":CRLF when TEXT has terminators and EVERY one of them is a Return followed by a Linefeed, :LF otherwise."
  (let ((linefeeds 0) (pairs 0))
    (dotimes (index (length text))
      (when (char= (char text index) #\Linefeed)
        (incf linefeeds)
        (when (and (plusp index) (char= (char text (1- index)) #\Return))
          (incf pairs))))
    (if (and (plusp linefeeds) (= linefeeds pairs)) :crlf :lf)))

(defun %drop-returns (text)
  "TEXT with the Return of every Return-Linefeed pair taken out."
  (let ((out (make-string (- (length text) (count #\Linefeed text))))
        (at 0))
    (dotimes (index (length text))
      (let ((character (char text index)))
        (unless (and (char= character #\Return)
                     (< (1+ index) (length text))
                     (char= (char text (1+ index)) #\Linefeed))
          (setf (char out at) character)
          (incf at))))
    out))

(defun %add-returns (text)
  "TEXT with a Return before every Linefeed."
  (let ((out (make-string (+ (length text) (count #\Linefeed text))))
        (at 0))
    (dotimes (index (length text))
      (let ((character (char text index)))
        (when (char= character #\Linefeed)
          (setf (char out at) #\Return)
          (incf at))
        (setf (char out at) character)
        (incf at)))
    out))

;;; ------------------------------------------------------------------ reading

(defun %note-file (document byte-length)
  "Remember the file as it stands now."
  (setf (slot-value document 'write-date) (file-write-date (document-pathname document))
        (slot-value document 'byte-length) byte-length)
  document)

(defun read-document (pathname &key buffer)
  "Read PATHNAME into BUFFER, or into a buffer of its own, and answer the document that remembers how the file is written."
  (let* ((path (merge-pathnames pathname))
         (octets (%read-octets path))
         (target (or buffer (make-buffer))))
    (multiple-value-bind (text encoding bom-p) (%decode octets path)
      (let ((eol (%eol-style text)))
        (buffer-set-text target (if (eq eol :crlf) (%drop-returns text) text))
        (%note-file (make-instance 'document :buffer target :pathname path
                                             :encoding encoding :bom-p bom-p
                                             :eol-style eol)
                    (length octets))))))

(defun document-changed-on-disk-p (document)
  "True when the file DOCUMENT last read or wrote is no longer the one it left there."
  (let ((path (document-pathname document)))
    (when path
      (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
        (cond ((null in) t)
              (t (or (/= (file-length in) (or (slot-value document 'byte-length) -1))
                     (/= (file-write-date path)
                         (or (slot-value document 'write-date) -1)))))))))

;;; ------------------------------------------------------------------ writing

(defun %check-representable (text encoding pathname)
  "Signal, before a single byte is made, when TEXT holds a character ENCODING has no byte for, naming WHERE in the text it stands."
  (dotimes (index (length text))
    (let ((code (char-code (char text index))))
      (when (if (eq encoding :latin-1) (< 255 code) (<= #xD800 code #xDFFF))
        (error 'document-encoding-error :pathname pathname :encoding encoding
                                        :index index :character (char text index))))))

(defun %document-octets (document pathname)
  "The bytes DOCUMENT's text makes under its own encoding, terminator and mark."
  (let ((text (buffer-text (document-buffer document)))
        (encoding (document-encoding document)))
    (%check-representable text encoding pathname)
    (let ((body (babel:string-to-octets
                 (if (eq (document-eol-style document) :crlf) (%add-returns text) text)
                 :encoding (if (eq encoding :latin-1) :latin-1 :utf-8))))
      (if (document-bom-p document)
          (concatenate '(vector (unsigned-byte 8)) *byte-order-mark* body)
          body))))

(defun %open-beside (pathname)
  "(values STREAM PATH) of a file that was not there, in PATHNAME's own folder."
  (loop for attempt from 0 below 1000
        for path = (make-pathname :name (format nil "~A-~36R"
                                                (or (pathname-name pathname) "document")
                                                (+ (* 1000 (get-universal-time)) attempt))
                                  :type "tmp" :defaults pathname)
        for stream = (open path :direction :output :element-type '(unsigned-byte 8)
                                :if-exists nil :if-does-not-exist :create)
        when stream return (values stream path)
        finally (error 'document-error :pathname pathname
                       :text "No free name was left for the file the bytes go to first.")))

(defun write-document (document &key pathname)
  "Write DOCUMENT to PATHNAME, or to the file it belongs to, in the encoding, the terminator and the byte-order mark it remembers."
  (let ((target (merge-pathnames (or pathname (document-pathname document)
                                     (error 'document-error
                                            :text "The document belongs to no file yet."))))
        (stream nil)
        (path nil)
        (placed nil))
    (let ((octets (%document-octets document target)))
      (unwind-protect
           (progn
             (multiple-value-setq (stream path) (%open-beside target))
             (%put-octets stream octets)
             (close stream)
             (setf stream nil)
             (rename-file path target)
             (setf placed t))
        (when stream (close stream :abort t))
        (unless placed
          (when (and path (probe-file path)) (delete-file path))))
      (setf (document-pathname document) target)
      (%note-file document (length octets))
      (setf (buffer-modified-p (document-buffer document)) nil)
      document)))

;;; ------------------------------------------------------------------ searching

(defun %match-at (data edge shift pattern index case-sensitive)
  "True when PATTERN stands at INDEX of the storage DATA, whose characters from EDGE on sit SHIFT further along."
  (declare (type simple-string data pattern)
           (type fixnum edge shift index)
           (optimize (speed 3) (safety 1)))
  (loop for offset of-type fixnum from 0 below (length pattern)
        for at of-type fixnum = (+ index offset)
        always (let ((character (schar data (if (< at edge) at (+ at shift)))))
                 (if case-sensitive
                     (char= character (schar pattern offset))
                     (char-equal character (schar pattern offset))))))

(defun %scan (buffer pattern from to step case-sensitive)
  "First index from FROM to TO, walking by STEP, where PATTERN stands in BUFFER, or NIL when it stands at none of them."
  (let ((data (%data buffer))
        (edge (%gap-start buffer))
        (text (coerce pattern 'simple-string)))
    (let ((shift (- (%gap-end buffer) edge)))
      (declare (type simple-string data) (type fixnum edge shift))
      (loop for index of-type fixnum = from then (+ index step)
            while (if (plusp step) (<= index to) (>= index to))
            do (when (%match-at data edge shift text index case-sensitive)
                 (return index))))))

(defun find-forward (buffer string start &key case-sensitive wrap)
  "(values START END) of the first STRING standing at or after START in BUFFER, or NIL when there is none."
  (let ((size (buffer-size buffer))
        (length (length string)))
    (%check-index start size 'find-forward)
    (when (plusp length)
      (let ((at (or (%scan buffer string start (- size length) 1 case-sensitive)
                    (when wrap
                      (%scan buffer string 0 (- size length) 1 case-sensitive)))))
        (when at (values at (+ at length)))))))

(defun find-backward (buffer string start &key case-sensitive wrap)
  "(values START END) of the last STRING that OPENS before START in BUFFER, or NIL when there is none."
  (let ((size (buffer-size buffer))
        (length (length string)))
    (%check-index start size 'find-backward)
    (when (plusp length)
      (let ((at (or (%scan buffer string (min (1- start) (- size length)) 0 -1
                           case-sensitive)
                    (when wrap
                      (%scan buffer string (- size length) 0 -1 case-sensitive)))))
        (when at (values at (+ at length)))))))
