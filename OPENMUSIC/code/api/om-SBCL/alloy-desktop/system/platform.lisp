(in-package :alloy-desktop)

(defvar *wsl-p*
  (and (uiop:os-unix-p)
       (let ((rel (with-open-file (stream "/proc/sys/kernel/osrelease"
                                          :if-does-not-exist nil)
                    (when stream (read-line stream nil)))))
         (and rel (search "microsoft" rel :test #'char-equal))))
  "Non-NIL when the current host is Windows Subsystem for Linux.")

(defun open-url (url)
  "Open URL with the desktop's native/default browser or URL handler."
  (handler-bind ((error
                   (lambda (condition)
                     (format *error-output* "~&OPEN-URL ~A failed: ~A~%"
                             url (princ-to-string condition))
                     (finish-output *error-output*))))
    (let ((url-string (princ-to-string url)))
      (cond
        (*wsl-p*
         (uiop:run-program (list "cmd.exe" "/c" "start" "" url-string)
                           :output nil :wait nil))
        ((uiop:os-windows-p)
         (uiop:run-program (list "cmd" "/c" "start" "" url-string)
                           :output nil :wait nil))
        ((uiop:os-macosx-p)
         (uiop:run-program (list "open" url-string)
                           :output nil :wait nil))
        (t
         #+linux
         (run-shell-command (list "xdg-open" url-string)
                            :wait nil :output nil :error-output nil)
         #-linux
         (uiop:run-program (list "xdg-open" url-string)
                           :output nil :wait nil)))
      t)))
