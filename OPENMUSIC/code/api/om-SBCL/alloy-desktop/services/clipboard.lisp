(in-package :alloy-desktop)

(defvar *default-clipboard-service* nil)

(defun default-clipboard-service ()
  (or *default-clipboard-service*
      (setf *default-clipboard-service* (make-instance 'clipboard-service))))

(defun %clipboard-command-write (text)
  (cond
    ((uiop:os-macosx-p)
     (uiop:run-program '("pbcopy") :input (make-string-input-stream text)))
    ((uiop:os-windows-p)
     (apply #'uiop:run-program '("clip") :input (make-string-input-stream text)
            (hidden-console-options)))
    (t
     (uiop:run-program '("xclip" "-selection" "clipboard")
                       :input (make-string-input-stream text)))))

(defun %clipboard-command-read ()
  (cond
    ((uiop:os-macosx-p)
     (uiop:run-program '("pbpaste") :output :string))
    ((uiop:os-windows-p)
     (apply #'uiop:run-program
            '("powershell" "-NoProfile" "-Command" "Get-Clipboard")
            :output :string
            (hidden-console-options)))
    (t
     (uiop:run-program '("xclip" "-selection" "clipboard" "-o")
                       :output :string))))

(defun %trim-windows-clipboard-newline (text)
  ;; PowerShell Get-Clipboard appends one CRLF to its redirected output.
  (if (and (uiop:os-windows-p)
           (plusp (length text))
           (char= (char text (1- (length text))) #\Newline))
      (subseq text 0 (- (length text)
                        (if (and (>= (length text) 2)
                                 (char= (char text (- (length text) 2)) #\Return))
                            2 1)))
      text))

(defmethod clipboard-write-text ((service clipboard-service) text)
  (declare (ignore service))
  (check-type text string)
  (handler-bind ((error
                   (lambda (condition)
                     (format *error-output* "[clipboard write] ~A~%"
                             (princ-to-string condition))
                     (return-from clipboard-write-text nil))))
    (%clipboard-command-write text)))

(defmethod clipboard-read-text ((service clipboard-service))
  (declare (ignore service))
  (handler-bind ((error
                   (lambda (condition)
                     (format *error-output* "[clipboard read] ~A~%"
                             (princ-to-string condition))
                     (return-from clipboard-read-text ""))))
    (%trim-windows-clipboard-newline (%clipboard-command-read))))
