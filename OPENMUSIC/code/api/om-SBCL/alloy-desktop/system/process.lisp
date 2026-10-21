(in-package :alloy-desktop)

;; LispWorks associates the opaque stream returned by RUN-SHELL-COMMAND
;; :SAVE-EXIT-STATUS with its child process.  Keep the association private:
;; clients receive streams/PIDs, never SBCL process-info objects.
(defvar *run-shell-command-processes*
  (make-hash-table :test #'eq :weakness :key))

(defvar *run-shell-command-processes-lock*
  (bordeaux-threads:make-lock "RUN-SHELL-COMMAND process table"))

(defun %run-shell-command-command (command)
  (etypecase command
    (null (if (uiop:os-windows-p)
              (list "cmd")
              (list (or (uiop:getenv "SHELL") "/bin/sh"))))
    ;; a string is run as by CALL-SYSTEM (lw/lw-sys-115.htm: run-shell-command)
    (string
     (if (uiop:os-windows-p)
         (shell-command-form command)
         (list (or (uiop:getenv "SHELL") "/bin/sh") "-c" command)))
    (list
     (unless (and command (every #'stringp command))
       (error "A RUN-SHELL-COMMAND list must be a non-empty list of strings: ~S"
              command))
     command)
    (simple-vector
     (let ((arguments (coerce command 'list)))
       (unless (and arguments (every #'stringp arguments))
         (error "A RUN-SHELL-COMMAND vector must contain only strings: ~S"
                command))
       arguments))))

(defun %run-shell-command-environment (environment)
  (when environment
    (let ((result (copy-list (sb-ext:posix-environ))))
      (dolist (binding environment result)
        (unless (and (consp binding)
                     (stringp (car binding))
                     (stringp (cdr binding)))
          (error "RUN-SHELL-COMMAND :ENVIRONMENT entries must be string pairs: ~S"
                 binding))
        (let* ((name (car binding))
               (prefix (concatenate 'string name "="))
               (test (if (uiop:os-windows-p) #'string-equal #'string=)))
          (setf result
                (delete-if (lambda (entry)
                             (and (<= (length prefix) (length entry))
                                  (funcall test prefix entry
                                           :end2 (length prefix))))
                           result))
          (push (concatenate 'string prefix (cdr binding)) result))))))

(defun %run-shell-command-temporary-pathname ()
  (loop for pathname = (merge-pathnames
                        (make-pathname
                         :name (format nil "alld-rsc-~D-~A"
                                       (get-universal-time) (gensym))
                         :type "tmp")
                        (uiop:temporary-directory))
        unless (probe-file pathname) return pathname))

(defun %run-shell-command-delete-if-present (pathname)
  (when (probe-file pathname)
    (delete-file pathname)))

(defun %run-shell-command-copy-overwrite (source target)
  (unwind-protect
       (with-open-file (input source :direction :input
                                     :element-type '(unsigned-byte 8))
         (with-open-file (output target :direction :output
                                        :if-exists :overwrite
                                        :if-does-not-exist :create
                                        :element-type '(unsigned-byte 8))
           (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
             (loop for count = (read-sequence buffer input)
                   while (plusp count)
                   do (write-sequence buffer output :end count)))))
    (%run-shell-command-delete-if-present source)))

(defun %run-shell-command-finish-resources (resources)
  (dolist (resource resources)
    (funcall (car resource))))

(defun %run-shell-command-abort-resources (resources)
  (dolist (resource resources)
    (funcall (cdr resource))))

(defun %run-shell-command-prepare-input (input if-does-not-exist)
  (if (and (or (stringp input) (pathnamep input))
           (null if-does-not-exist)
           (not (probe-file input)))
      (values nil :error)
      (values input (or if-does-not-exist :error))))

(defun %run-shell-command-prepare-output (output if-exists)
  (cond
    ((not (or (stringp output) (pathnamep output)))
     (values output (or if-exists :error) nil))
    ((eq if-exists :overwrite)
     (let ((temporary (%run-shell-command-temporary-pathname)))
       (values temporary :error
               (cons (lambda ()
                       (%run-shell-command-copy-overwrite temporary output))
                     (lambda ()
                       (%run-shell-command-delete-if-present temporary))))))
    ((and (null if-exists) (probe-file output))
     ;; OPEN returns NIL for this policy.  The child still runs, with this
     ;; channel connected to the null device.
     (values nil :error nil))
    (t
     (values output (or if-exists :error) nil))))

(defun %run-shell-command-register (stream process)
  (when (streamp stream)
    (bordeaux-threads:with-lock-held (*run-shell-command-processes-lock*)
      (setf (gethash stream *run-shell-command-processes*) process))))

(defun %run-shell-command-process (stream)
  (bordeaux-threads:with-lock-held (*run-shell-command-processes-lock*)
    (gethash stream *run-shell-command-processes*)))

(defun %run-shell-command-reap (process resources)
  (bordeaux-threads:make-thread
   (lambda ()
     (let ((completed nil))
       (unwind-protect
            (progn
              (uiop:wait-process process)
              (setf completed t))
         (if completed
             (%run-shell-command-finish-resources resources)
             (%run-shell-command-abort-resources resources)))))
   :name (format nil "RUN-SHELL-COMMAND reaper ~D"
                 (uiop:process-info-pid process))))

(defun pipe-exit-status (stream &key (timeout nil timeout-supplied-p)
                                     (wait t wait-supplied-p))
  "Return the saved exit status associated with STREAM.

TIMEOUT is in seconds. NIL waits indefinitely and zero only polls. The
legacy WAIT NIL spelling is treated as TIMEOUT zero when TIMEOUT was not
supplied, matching LispWorks."
  (let ((process (%run-shell-command-process stream)))
    (unless process
      (error "~S is not a RUN-SHELL-COMMAND saved-status stream." stream))
    (when (and (uiop:os-windows-p) (not (open-stream-p stream)))
      (return-from pipe-exit-status (values :closed nil)))
    (let ((effective-timeout
            (if timeout-supplied-p timeout
                (if (and wait-supplied-p (null wait)) 0 nil))))
      (cond
        ((null effective-timeout)
         (multiple-value-bind (status signal)
             (uiop:wait-process process)
           (values status signal)))
        ((minusp effective-timeout)
         (error "PIPE-EXIT-STATUS :TIMEOUT must be non-negative or NIL."))
        (t
         (let ((deadline (+ (get-internal-real-time)
                            (round (* effective-timeout
                                      internal-time-units-per-second)))))
           (loop while (uiop:process-alive-p process)
                 for remaining = (/ (- deadline (get-internal-real-time))
                                    internal-time-units-per-second)
                 do (when (<= remaining 0)
                      (return-from pipe-exit-status (values nil nil)))
                    (sleep (min 0.01 remaining)))
           (multiple-value-bind (status signal)
               (uiop:wait-process process)
             (values status signal))))))))

(defun run-shell-command
    (command &key input output error-output separate-streams (wait t)
                  (if-input-does-not-exist :error)
                  (if-output-exists :error)
                  (if-error-output-exists :error)
                  show-window environment (element-type 'base-char)
                  save-exit-status current-directory
                  (external-format :default))
  "Portable desktop substrate implementation of LispWorks RUN-SHELL-COMMAND for SBCL."
  (when separate-streams
    (error "RUN-SHELL-COMMAND :SEPARATE-STREAMS T is not supported on SBCL."))
  (when show-window
    (error "RUN-SHELL-COMMAND :SHOW-WINDOW T is not supported on SBCL."))
  (when (and wait (member :stream (list input output error-output)))
    (error "RUN-SHELL-COMMAND cannot use :STREAM with :WAIT T."))
  (multiple-value-bind (actual-input input-policy)
      (%run-shell-command-prepare-input input if-input-does-not-exist)
    (multiple-value-bind (actual-output output-policy owned-output)
        (%run-shell-command-prepare-output output if-output-exists)
      (multiple-value-bind (actual-error error-policy owned-error)
          (if (eq error-output :output)
              (values :output :error nil)
              (%run-shell-command-prepare-output
               error-output if-error-output-exists))
        (let* ((owned-resources (remove nil (list owned-output owned-error)))
               (keys (list* :input actual-input
                           :if-input-does-not-exist input-policy
                           :output actual-output
                           :if-output-exists output-policy
                           :error-output actual-error
                           :if-error-output-exists error-policy
                           :element-type element-type
                           :external-format external-format
                           (shell-command-options command)))
               (child-environment
                 (%run-shell-command-environment environment)))
          (when current-directory
            (setf keys (append keys (list :directory current-directory))))
          (when environment
            (setf keys (append keys (list :environment child-environment))))
          (let ((process
                  (handler-case
                      (apply #'uiop:launch-program
                             (%run-shell-command-command command) keys)
                    (error (condition)
                      (%run-shell-command-abort-resources owned-resources)
                      (error condition)))))
            (if wait
                (let ((completed nil))
                  (unwind-protect
                       (multiple-value-bind (status signal)
                           (uiop:wait-process process)
                         (setf completed t)
                         (values status signal))
                    (if completed
                        (%run-shell-command-finish-resources owned-resources)
                        (%run-shell-command-abort-resources owned-resources))))
                (let* ((input-pipe (and (eq input :stream)
                                        (uiop:process-info-input process)))
                       (output-pipe (and (eq output :stream)
                                        (uiop:process-info-output process)))
                       (primary-stream
                         (cond
                           ((and input-pipe output-pipe)
                            (make-two-way-stream output-pipe input-pipe))
                           (output-pipe output-pipe)
                           (input-pipe input-pipe)
                           (save-exit-status (make-string-output-stream))))
                       (error-stream
                         (and (eq error-output :stream)
                              (uiop:process-info-error-output process)))
                       (pid (uiop:process-info-pid process)))
                  (when save-exit-status
                    (%run-shell-command-register primary-stream process)
                    (%run-shell-command-register error-stream process))
                  ;; A non-saved asynchronous process has no later API call
                  ;; that reaps it. The private reaper prevents leaked OS
                  ;; process resources without changing stream ownership.
                  (when (or (not save-exit-status) owned-resources)
                    (%run-shell-command-reap process owned-resources))
                  (if (or primary-stream error-stream save-exit-status)
                      (values primary-stream error-stream pid)
                      pid)))))))))

(defun shell-command-form (command)
  "Return COMMAND in the host shell form used by LispWorks-like process services."
  ;; LispWorks passes the command STRING to cmd.exe /c (lw/lw-sys-13.htm:66,80); UIOP
  ;; re-escapes a Windows argv (inner quotes become \"), which cmd.exe does not parse.
  ;; The extra outer pair is rule 2 of `cmd /?`: /C strips the first and last quote.
  (if (listp command)
      command
      (if (uiop:os-windows-p)
          (format nil "cmd /c \"~A\"" command)
          (list "sh" "-c" command))))

(defun hidden-console-options ()
  "Extra UIOP:RUN-PROGRAM keywords that start a console program without showing a console window: on Windows a GUI process gives every console child a new one."
  (if (uiop:os-windows-p)
      '(:window :hide)
      '()))

(defun shell-command-options (command)
  "Extra UIOP:RUN-PROGRAM keywords for COMMAND run in shell form: on Windows a string command's console window is hidden, as LispWorks does (lw/lw-sys-13.htm: call-system)."
  (if (stringp command)
      (hidden-console-options)
      '()))

(defun %default-process-external-format ()
  ;; A Windows console may emit code-page bytes rather than UTF-8. Elsewhere
  ;; preserve UIOP's normal UTF-8 process decoding policy.
  (if (uiop:os-windows-p)
      '(:latin-1 :replacement #\?)
      uiop:*utf-8-external-format*))

(defun call-system (command &key current-directory (wait t) shell-type)
  "Run COMMAND with LispWorks SYSTEM:CALL-SYSTEM-compatible basic semantics."
  (declare (ignore shell-type))
  (let ((cmd (shell-command-form command))
        (options (shell-command-options command)))
    (if wait
        (nth-value 2
                   (apply #'uiop:run-program cmd
                          :directory current-directory
                          :ignore-error-status t
                          :external-format (%default-process-external-format)
                          :output *standard-output*
                          :error-output *standard-output*
                          options))
        (progn
          (apply #'uiop:launch-program cmd :directory current-directory options)
          nil))))

(defun %write-prefixed-lines (text stream prefix)
  (with-input-from-string (input text)
    (loop for line = (read-line input nil nil)
          while line
          do (format stream "~&~A~A~%" prefix line))))

(defun call-system-showing-output (command
                                   &key current-directory (prefix "; ") (show-cmd t)
                                        (output-stream *standard-output*) (wait t)
                                        (shell-type "/bin/sh")
                                        (external-format :default) (label nil)
                                   &allow-other-keys)
  "Run COMMAND through a shell, write its merged console output line by line with PREFIX to OUTPUT-STREAM followed by an exit-status line naming LABEL when LABEL is given and the status is not zero, and return the exit status and the collected output."
  (let ((args (if (uiop:os-windows-p)
                  (shell-command-form command)
                  (list shell-type "-c" command)))
        (options (shell-command-options command))
        (target (cond ((eq output-stream t) *standard-output*)
                      ((streamp output-stream) output-stream)
                      (t nil)))
        (ef (cond ((not (eq external-format :default)) external-format)
                  ((uiop:os-windows-p) '(:latin-1 :replacement #\?))
                  (t '(:utf-8 :replacement #\?)))))
    (when (and show-cmd target)
      (format target "~&~A~A~%" prefix command))
    (cond
      ((null wait)
       (apply #'uiop:launch-program args #+linux :input #+linux :interactive
              :output nil :error-output nil options)
       (values 0 nil))
      ((eq output-stream :tty)
       (values (nth-value 2
                          (apply #'uiop:run-program args
                                 #+linux :input #+linux :interactive
                                 :output :interactive
                                 :error-output :interactive
                                 :ignore-error-status t
                                 options))
               nil))
      (t
       (multiple-value-bind (out err status)
           (apply #'uiop:run-program args
                  #+linux :input #+linux :interactive
                  :output :string
                  :error-output :string
                  :directory current-directory
                  :external-format ef
                  :ignore-error-status t
                  options)
         (let ((text (concatenate 'string (or out "") (or err ""))))
           (when target
             (%write-prefixed-lines text target prefix)
             (when (and label (not (eql status 0)))
               (format target "~&~A[exit status ~A] ~A~%" prefix status label)))
           (values status text)))))))

(defvar *system-open-command*
  (cond ((uiop:os-macosx-p) "open")
        ((uiop:os-windows-p) "start")
        (t "xdg-open"))
  "The host command that opens a file or application with its default handler.")

(defun open-with-system (path &key wait)
  "Open PATH with *SYSTEM-OPEN-COMMAND*, waiting for the opener when WAIT is true, and return the namestring of PATH."
  (let* ((name (namestring path))
         ;; start is a cmd builtin, so Windows goes through the shell form
         (line (and (uiop:os-windows-p)
                    (format nil "~A \"\" ~S" *system-open-command* name)))
         (command (if line
                      (shell-command-form line)
                      (list *system-open-command* name)))
         (options (shell-command-options line)))
    (if wait
        (apply #'uiop:run-program command :ignore-error-status t options)
        (apply #'uiop:launch-program command options))
    name))

(defun run-application (path &key after name initial-bindings armed-traps)
  "Run the executable PATH, or the main executable of a macOS bundle directory PATH, on a thread from MAKE-DESKTOP-THREAD, call AFTER when it exits, and return the namestring of PATH."
  (let ((pathstr (namestring path)))
    (when (equal (elt pathstr (- (length pathstr) 1)) #\/)
      (setf pathstr (subseq pathstr 0 (- (length pathstr) 1)))
      (let ((appname (pathname-name (pathname (subseq pathstr 0 (- (length pathstr) 1))))))
        (setf pathstr (namestring (make-pathname :directory (append (pathname-directory path)
                                                                    (list "Contents" "MacOS"))
                                                 :name appname)))))
    (make-desktop-thread
     (lambda ()
       ;; Windows runs it through the shell with its console hidden, as om-LW does
       ;; (om-LW system.lisp: om-run-program, call-system of the quoted path)
       (if (uiop:os-windows-p)
           (call-system (format nil "~S" pathstr))
           (uiop:run-program (list pathstr) :ignore-error-status t))
       (when after (funcall after)))
     :name (or name (namestring path))
     :initial-bindings initial-bindings
     :armed-traps armed-traps)
    (namestring path)))

;;; Application windows

(defun %decode-tab-application-windows (string)
  (loop for raw-line in (uiop:split-string string :separator '(#\Newline))
        for line = (string-right-trim '(#\Return) raw-line)
        unless (zerop (length line))
          collect
          (let* ((first-tab (position #\Tab line))
                 (second-tab (and first-tab
                                  (position #\Tab line :start (1+ first-tab)))))
            (unless (and first-tab second-tab)
              (error "Invalid window-list line: ~S" line))
            (list :id (parse-integer (subseq line 0 first-tab))
                  :process-name (subseq line (1+ first-tab) second-tab)
                  :main-window-title (subseq line (1+ second-tab))))))

#+win32
(defun %application-window-list ()
  (uiop:run-program
   '("powershell.exe"
     "-NoProfile"
     "-Command"
     "[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false); Get-Process | Where-Object {$_.MainWindowHandle -ne 0} | ForEach-Object { $title = $_.MainWindowTitle -replace '[\\t\\r\\n]', ' '; Write-Output (\"{0}`t{1}`t{2}\" -f $_.Id, $_.ProcessName, $title) }")
   :output :string
   :error-output :output
   :external-format :utf-8
   :window :hide))

#+linux
(defun %application-window-list ()
  (uiop:run-program
   '("wmctrl" "-lp" "-u")
   :output :string
   :error-output :output
   :external-format :utf-8))

#+darwin
(defun %application-window-list ()
  (uiop:run-program
   '("osascript"
     "-e"
     "on replaceText(theText, oldText, newText)
  set savedDelimiters to AppleScript's text item delimiters
  set AppleScript's text item delimiters to oldText
  set textItems to text items of theText
  set AppleScript's text item delimiters to newText
  set theText to textItems as text
  set AppleScript's text item delimiters to savedDelimiters
  return theText
end replaceText

on sanitizeField(theText)
  set theText to my replaceText(theText as text, AppleScript's tab, \" \")
  set theText to my replaceText(theText, AppleScript's return, \" \")
  set theText to my replaceText(theText, AppleScript's linefeed, \" \")
  return theText
end sanitizeField

set outText to \"\"
tell application \"System Events\"
  repeat with p in application processes
    try
      set pidText to (unix id of p) as text
      set pname to my sanitizeField(name of p)
      repeat with w in windows of p
        try
          set wtitle to name of w
          if wtitle is missing value then
            set wtitle to \"\"
          else
            set wtitle to my sanitizeField(wtitle)
          end if
          set outText to outText & pidText & AppleScript's tab & pname & AppleScript's tab & wtitle & AppleScript's linefeed
        end try
      end repeat
    on error errorMessage number errorNumber
      if errorNumber is -1719 then error errorMessage number errorNumber
    end try
  end repeat
end tell
return outText")
   :output :string
   :error-output :output
   :external-format :utf-8))

#+(or win32 darwin)
(defun %decode-application-windows (string)
  (%decode-tab-application-windows string))

#+linux
(defun %decode-application-windows (string)
  (labels ((whitespacep (char)
             (or (char= char #\Space)
                 (char= char #\Tab)))
           (next-token (line start)
             (let ((begin (position-if-not #'whitespacep line :start start)))
               (when begin
                 (let ((end (or (position-if #'whitespacep line :start begin)
                                (length line))))
                   (values (subseq line begin end) end)))))
           (process-name (pid)
             (handler-case
                 (file-namestring (truename (format nil "/proc/~D/exe" pid)))
               (file-error ()
                 (with-open-file (stream (format nil "/proc/~D/comm" pid))
                   (string-trim '(#\Space #\Tab #\Return #\Newline)
                                (read-line stream nil "")))))))
    (loop for raw-line in (uiop:split-string string :separator '(#\Newline))
          for line = (string-right-trim '(#\Return) raw-line)
          for entry =
            (unless (zerop (length line))
              (multiple-value-bind (window-id p1) (next-token line 0)
                (declare (ignore window-id))
                (when p1
                  (multiple-value-bind (desktop p2) (next-token line p1)
                    (declare (ignore desktop))
                    (when p2
                      (multiple-value-bind (pid-string p3) (next-token line p2)
                        (when p3
                          (multiple-value-bind (host p4) (next-token line p3)
                            (declare (ignore host))
                            (when p4
                              (let* ((pid (parse-integer pid-string :junk-allowed t))
                                     (name (and pid (plusp pid)
                                                (process-name pid)))
                                     (title (string-left-trim
                                             '(#\Space #\Tab)
                                             (subseq line p4))))
                                (when (and pid (plusp pid) name)
                                  (list :id pid
                                        :process-name name
                                        :main-window-title title))))))))))))
          when entry collect entry)))

(defun list-application-windows ()
  "The top-level windows of the running applications, as plists of :ID, :PROCESS-NAME and :MAIN-WINDOW-TITLE."
  (%decode-application-windows (%application-window-list)))

(defun find-application-window (application)
  "The first application window whose process id is the integer APPLICATION or whose process name contains the string APPLICATION, ignoring case."
  (find-if (lambda (window)
             (if (integerp application)
                 (= application (getf window :id))
                 (search application (getf window :process-name) :test #'char-equal)))
           (list-application-windows)))

#+win32
(defun bring-process-to-front (pid)
  "Bring the process PID to the foreground."
  (string-equal "true"
                (string-trim '(#\Space #\Tab #\Return #\Newline)
                             (uiop:run-program
                              (list "powershell.exe"
                                    "-NoProfile"
                                    "-Command"
                                    (format nil
                                            "$ws = New-Object -ComObject WScript.Shell; $ws.AppActivate(~D)"
                                            pid))
                              :output :string
                              :error-output :output
                              :external-format :utf-8
                              :ignore-error-status t
                              :window :hide))))

#+linux
(defun bring-process-to-front (pid)
  "Bring the X11/EWMH window of the process PID to the foreground."
  (let ((window-id
          (loop for raw-line in (uiop:split-string (%application-window-list)
                                                   :separator '(#\Newline))
                for fields = (remove ""
                                     (uiop:split-string raw-line
                                                        :separator '(#\Space #\Tab))
                                     :test #'string=)
                when (and (>= (length fields) 3)
                          (= pid (or (parse-integer (third fields)
                                                    :junk-allowed t)
                                     -1)))
                  return (first fields))))
    (when window-id
      (uiop:run-program
       (list "wmctrl" "-i" "-a" window-id)
       :output :string
       :error-output :output
       :external-format :utf-8)
      t)))

#+darwin
(defun bring-process-to-front (pid)
  "Bring the process PID to the foreground."
  (uiop:run-program
   (list "osascript"
         "-e"
         "on run argv
  set targetPID to (item 1 of argv) as integer
  tell application \"System Events\"
    set p to first application process whose unix id is targetPID
    set frontmost of p to true
  end tell
end run"
         (princ-to-string pid))
   :output :string
   :error-output :output
   :external-format :utf-8)
  t)

#+win32
(defun bring-application-to-front (application)
  "Bring APPLICATION, a process id or part of a process name, to the foreground."
  (let ((window (find-application-window application)))
    (when window
      (let* ((pid (getf window :id))
             (result
               (uiop:run-program
                (list "powershell.exe"
                      "-NoProfile"
                      "-Command"
                      (format nil
                              "$ws = New-Object -ComObject WScript.Shell; $ws.AppActivate(~D)"
                              pid))
                :output :string
                :error-output :output
                :external-format :utf-8
                :ignore-error-status t
                :window :hide)))
        (string-equal "true"
                      (string-trim '(#\Space #\Tab #\Return #\Newline)
                                   result))))))

#+linux
(defun bring-application-to-front (application)
  "Bring the X11/EWMH APPLICATION, a process id or part of a process name, to the foreground."
  (let ((window (find-application-window application)))
    (when window
      (let* ((pid (getf window :id))
             (window-id
               (loop for raw-line in (uiop:split-string (%application-window-list)
                                                        :separator '(#\Newline))
                     for fields = (remove ""
                                          (uiop:split-string raw-line
                                                             :separator '(#\Space #\Tab))
                                          :test #'string=)
                     when (and (>= (length fields) 3)
                               (= pid (or (parse-integer (third fields)
                                                         :junk-allowed t)
                                          -1)))
                       return (first fields))))
        (when window-id
          (uiop:run-program
           (list "wmctrl" "-i" "-a" window-id)
           :output :string
           :error-output :output
           :external-format :utf-8)
          t)))))

#+darwin
(defun bring-application-to-front (application)
  "Bring APPLICATION, a process id or part of a process name, to the foreground."
  (let ((window (find-application-window application)))
    (when window
      (uiop:run-program
       (list "osascript"
             "-e"
             "on run argv
  set targetPID to (item 1 of argv) as integer
  tell application \"System Events\"
    set p to first application process whose unix id is targetPID
    set frontmost of p to true
  end tell
end run"
             (princ-to-string (getf window :id)))
       :output :string
       :error-output :output
       :external-format :utf-8)
      t)))
