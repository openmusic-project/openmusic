;; This file is part of yason, a Common Lisp JSON parser/encoder
;;
;; Copyright (c) 2008-2012 Hans Huebner and contributors
;; All rights reserved.
;;
;; Please see the file LICENSE in the distribution.

(in-package :yason)

(defconstant +default-string-length+ 20
  "Default length of strings that are created while reading json input.")

(defvar *parse-object-key-fn* #'identity
  "Function to call to convert a key string in a JSON array to a key
  in the CL hash produced.")

(defvar *parse-json-arrays-as-vectors* nil
  "If set to a true value, JSON arrays will be parsed as vectors, not
  as lists.")

(defvar *parse-json-booleans-as-symbols* nil
  "If set to a true value, JSON booleans will be read as the symbols
  TRUE and FALSE, not as T and NIL, respectively.")

(defvar *parse-json-null-as-keyword* nil
  "If set to a true value, JSON nulls will be read as the keyword :NULL, not as NIL.")

(defvar *parse-object-as* :hash-table
  "Set to either :hash-table, :plist or :alist to determine the data
  structure that objects are parsed to.")

(defvar *parse-object-as-alist* nil
  "DEPRECATED, provided for backward compatibility")

(defun make-adjustable-string ()
  "Return an adjustable empty string, usable as a buffer for parsing strings and numbers."
  (make-array +default-string-length+
              :adjustable t :fill-pointer 0 :element-type 'character))

(defun parse-number (input)
  ;; would be
  ;; (cl-ppcre:scan-to-strings "^-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+|)(?:[eE][-+]?[0-9]+|)" buffer)
  ;; but we want to operate on streams
  (let ((buffer (make-adjustable-string)))
    (loop
       while (position (peek-char nil input nil) ".0123456789+-Ee")
       do (vector-push-extend (read-char input) buffer))
    (values (read-from-string buffer))))

(defun parse-string (input)
  (let ((output (make-adjustable-string)))
    (labels ((outc (c)
               (vector-push-extend c output))
             (next ()
               (read-char input))
             (peek ()
               (peek-char nil input)))
      (let*
       (
        (starting-symbol (next))
        (string-quoted (equal starting-symbol #\"))
        )
       (unless string-quoted (outc starting-symbol))
       (loop
        (cond
         ((eql (peek) #\")
          (next)
          (return-from parse-string output))
         ((eql (peek) #\\)
          (next)
          (ecase (next)
             (#\" (outc #\"))
             (#\\ (outc #\\))
             (#\/ (outc #\/))
             (#\b (outc #\Backspace))
             (#\f (outc #\Page))
             (#\n (outc #\Newline))
             (#\r (outc #\Return))
             (#\t (outc #\Tab))
             (#\u (outc (code-char (let ((buffer (make-string 4)))
                                     (read-sequence buffer input)
                                     (parse-integer buffer :radix 16)))))))
         ((and (or (whitespace-p (peek)) 
                   (eql (peek) #\:)) 
               (not string-quoted)) 
          (return-from parse-string output))
         (t
          (outc (next)))))))))

(defun whitespace-p (char)
  (member char '(#\Space #\Newline #\Tab #\Linefeed #\Return)))

(defun skip-whitespace (input)
  (loop
     while (and (listen input)
                (whitespace-p (peek-char nil input)))
     do (read-char input)))

(defun peek-char-skipping-whitespace (input &optional (eof-error-p t))
  (skip-whitespace input)
  (peek-char nil input eof-error-p))

(defun parse-constant (input)
  (destructuring-bind (expected-string return-value)
      (find (peek-char nil input nil)
            `(("true" ,(if *parse-json-booleans-as-symbols* 'true t))
              ("false" ,(if *parse-json-booleans-as-symbols* 'false nil))
              ("null"  ,(if *parse-json-null-as-keyword* :null nil)))
            :key (lambda (entry) (aref (car entry) 0))
            :test #'eql)
    (loop
       for char across expected-string
       unless (eql (read-char input nil) char)
       do (error "invalid constant"))
    return-value))

(define-condition cannot-convert-key (error)
  ((key-string :initarg :key-string
               :reader key-string))
  (:report (lambda (c stream)
             (format stream "cannot convert key ~S used in JSON object to hash table key"
                     (key-string c)))))

(defun create-container ()
  (ecase *parse-object-as*
    ((:plist :alist)
     nil)
    (:hash-table
     (make-hash-table :test #'equal))))

(defun add-attribute (to key value)
  (ecase *parse-object-as*
    (:plist
     (append to (list key value)))
    (:alist
     (acons key value to))
    (:hash-table
     (setf (gethash key to) value)
     to)))

(defun parse-object (input)
  (let ((return-value (create-container)))
    (read-char input)
    (loop
       (when (eql (peek-char-skipping-whitespace input)
                  #\})
         (return))
       (skip-whitespace input)
       (setf return-value
             (add-attribute return-value
                            (prog1
                                (let ((key-string (parse-string input)))
                                  (or (funcall *parse-object-key-fn* key-string)
                                      (error 'cannot-convert-key :key-string key-string)))
                              (skip-whitespace input)
                              (unless (eql #\: (read-char input))
                                (error 'expected-colon))
                              (skip-whitespace input))
                            (parse input)))
       (ecase (peek-char-skipping-whitespace input)
         (#\, (read-char input))
         (#\} nil)))
    (read-char input)
    return-value))

(defconstant +initial-array-size+ 20
  "Initial size of JSON arrays read, they will grow as needed.")

(defun %parse-array (input add-element-function)
  "Parse JSON array from input, calling ADD-ELEMENT-FUNCTION for each array element parsed."
  (read-char input)
  (loop
     (when (eql (peek-char-skipping-whitespace input)
                #\])
       (return))
     (funcall add-element-function (parse input))
     (ecase (peek-char-skipping-whitespace input)
       (#\, (read-char input))
       (#\] nil)))
  (read-char input))

(defun parse-array (input)
  (if *parse-json-arrays-as-vectors*
      (let ((return-value (make-array +initial-array-size+ :adjustable t :fill-pointer 0)))
        (%parse-array input
                      (lambda (element)
                        (vector-push-extend element return-value)))
        return-value)
      (let (return-value)
        (%parse-array input
                      (lambda (element)
                        (push element return-value)))
        (nreverse return-value))))

(defgeneric parse% (input)
  (:method ((input stream))
    ;; backward compatibility code
    (assert (or (not *parse-object-as-alist*)
                (eq *parse-object-as* :hash-table))
            () "unexpected combination of *parse-object-as* and *parse-object-as-alist*, please use *parse-object-as* exclusively")
    (let ((*parse-object-as* (if *parse-object-as-alist*
                                 :alist
                                 *parse-object-as*)))
      ;; end of backward compatibility code
      (check-type *parse-object-as* (member :hash-table :alist :plist))
      (ecase (peek-char-skipping-whitespace input)
        (#\"
         (parse-string input))
        ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (parse-number input))

        (#\{
         (parse-object input))
        (#\[
         (parse-array input))
        ((#\t #\f #\n)
         (parse-constant input)))))
  (:method ((input pathname))
   (with-open-file (stream input)
     (parse stream)))
  (:method ((input string))
    (parse (make-string-input-stream input))))

(defun parse (input
              &key
                (object-key-fn *parse-object-key-fn*)
                (object-as *parse-object-as*)
                (json-arrays-as-vectors *parse-json-arrays-as-vectors*)
                (json-booleans-as-symbols *parse-json-booleans-as-symbols*)
                (json-nulls-as-keyword *parse-json-null-as-keyword*))
  "Parse INPUT, which needs to be a string or a stream, as JSON.
  Returns the lisp representation of the JSON structure parsed.  The
  keyword arguments can be used to override the parser settings as
  defined by the respective special variables."
  (let ((*parse-object-key-fn* object-key-fn)
        (*parse-object-as* object-as)
        (*parse-json-arrays-as-vectors* json-arrays-as-vectors)
        (*parse-json-booleans-as-symbols* json-booleans-as-symbols)
        (*parse-json-null-as-keyword* json-nulls-as-keyword))
    (parse% input)))
