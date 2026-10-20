(in-package :alloy-desktop)

(defun compile-file-if-needed (input-pathname &key (output-file nil output-file-p)
                                              (load t) &allow-other-keys)
  "Compile INPUT-PATHNAME only when its output file is missing or stale.

Returns OUTPUT-TRUENAME, WARNINGS-P and FAILURE-P, matching the LispWorks
HCL:COMPILE-FILE-IF-NEEDED subset used by OpenMusic and its external loaders."
  (let* ((actual-input (if (pathname-type input-pathname)
                           input-pathname
                           (merge-pathnames (make-pathname :type "lisp") input-pathname)))
         (lisp-file (probe-file actual-input)))
    (unless lisp-file
      (error "Source file ~S not found" actual-input))
    (let* ((expected-fasl (if output-file-p
                              (pathname output-file)
                              (compile-file-pathname lisp-file)))
           (fasl-file (probe-file expected-fasl))
           (needs-compile (or (not fasl-file)
                              (> (file-write-date lisp-file)
                                 (file-write-date fasl-file)))))
      (if needs-compile
          (handler-case
              (multiple-value-bind (out-true warn-p fail-p)
                  (if output-file-p
                      (compile-file lisp-file :output-file output-file :verbose t :print t)
                      (compile-file lisp-file :verbose t :print t))
                (when (and load (not fail-p))
                  (cl:load (or out-true expected-fasl) :verbose t))
                (values out-true warn-p (or fail-p (not out-true))))
            (file-error (condition)
              (format *error-output*
                      "~&compile-file-if-needed: compilation of ~S failed: ~A~%"
                      lisp-file condition)
              (values nil nil t)))
          (progn
            (when load
              (cl:load fasl-file :verbose t))
            (values fasl-file nil nil))))))

(defun detect-cr-only-external-format (source)
  "Return the external format required for a CR-only text SOURCE, or NIL.

SOURCE is inspected as raw octets. If at least one carriage return is present
and no line feed is found, return an external-format designator that treats CR
as the newline convention. Otherwise return NIL, also when SOURCE is NIL."
  (when source
    (with-open-file (stream source :direction :input
                                   :element-type '(unsigned-byte 8))
      (let ((saw-return nil)
            (saw-linefeed nil))
        (loop for octet = (read-byte stream nil nil)
              while octet
              do (case octet
                   (13 (setf saw-return t))
                   (10 (setf saw-linefeed t)
                       (return))))
        (when (and saw-return (not saw-linefeed))
          '(:default :newline :cr))))))

(defun variant-compiled-file-pathname (source variant)
  "Return SOURCE's compiled pathname, optionally distinguished by VARIANT.

When VARIANT is non-NIL, insert it after the source name and before the normal
compiled-file type. This keeps artifacts produced under different compilation
policies from colliding."
  (if variant
      (make-pathname :name (format nil "~A.~A" (pathname-name source) variant)
                     :type (pathname-type (compile-file-pathname source))
                     :defaults source)
      (compile-file-pathname source)))

(defun compile-source-checked (source output-file
                               &key external-format (verbose t))
  "Compile SOURCE to OUTPUT-FILE and require an emitted artifact.

When EXTERNAL-FORMAT is non-NIL, pass it to COMPILE-FILE. Return the emitted
compiled pathname. Signal an error if compilation does not produce an existing
output file."
  (multiple-value-bind (output warnings-p failure-p)
      (if external-format
          (compile-file source :output-file output-file
                               :external-format external-format
                               :verbose verbose)
          (compile-file source :output-file output-file
                               :verbose verbose))
    (declare (ignore warnings-p failure-p))
    (unless (and output (probe-file output))
      (error "Compilation failed for ~S." source))
    output))

(defmacro with-default-external-format ((external-format) &body body)
  "Evaluate BODY with the implementation default external format temporarily set.

This binds the default used by LOAD and related stream operations."
  `(let ((sb-impl::*default-external-format* ,external-format))
     ,@body))
