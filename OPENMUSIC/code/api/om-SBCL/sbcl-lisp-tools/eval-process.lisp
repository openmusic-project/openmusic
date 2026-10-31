(in-package :oa)

;; the defparameters live further down this file; forward-declare them here
(declaim (special *om-listener* *listener-input* *om-stream*))

;;; ============================================================
;;; EVAL PROCESS
;;; ============================================================

(defvar *om-default-package* (find-package :common-lisp-user)
  "Default package for evaluating user input in the listener/editor.")

(defparameter *eval-process-name* "OM EVAL PROCESS")

(defvar *om-eval-worker*
  (alld:make-serial-worker
   :lock (bordeaux-threads:make-lock "om-eval-lock")
   :condition (bordeaux-threads:make-condition-variable :name "om-eval-cv")
   :armed-traps '(:divide-by-zero)
   ;; one binding per generation, like a LispWorks process gets from
   ;; mp:*process-initial-bindings*: an (in-package X) run by a box persists
   :loop-bindings (lambda () (list (cons '*package* (find-package :om))))
   :job-function (lambda (event)
                   (%honoring-redefinition-warnings
                     (if (functionp event) (funcall event) (eval event))))
   ;; an error no box handled (Handle Error Messages off) reaches the debugger hook of a
   ;; delivered image, om-error-handler, at the point where it is signalled
   :error-observer (lambda (condition)
                     (if (om-standalone-p)
                         (%om-report-error condition)
                         (%capture-error-backtrace condition)))
   ;; the generation ends after the failure, as the process does on the hook's ABORT
   :fatal-error-handler (lambda (condition)
                          (report-ae-error condition)
                          (format *error-output* "[om-eval] ~A~%"
                                  (princ-to-string condition)))
   :generation-end (lambda ()
                     (setf (alld:serial-worker-queue *om-eval-worker*) nil)
                     (when (eq (alld:serial-worker-thread *om-eval-worker*)
                               (bordeaux-threads:current-thread))
                       (setf (alld:serial-worker-thread *om-eval-worker*) nil
                             (alld:serial-worker-stop-p *om-eval-worker*) nil))))
  "The visual evaluator: one thread serving the jobs of the patch editors and of the
workspace save, one at a time.")

(defun om-work-function ()
  "Run one generation of the visual evaluator.

LispWorks gives OM EVAL PROCESS a process-level ABORT restart.  Consequently an
uncaught error or OM:OM-ABORT terminates that process; the next evaluation starts
a fresh one.  The cleanup also discards that generation's queued work, so forms
which were waiting behind the failed operation cannot leak into its replacement."
  (alld:serial-worker-run *om-eval-worker*))

(defun %ensure-om-eval-process-locked ()
  "Return the live visual evaluator, starting it while the lock of *OM-EVAL-WORKER* is held.  The name and the initial bindings are read at each start."
  (unless (alld:serial-worker-live-p *om-eval-worker*)
    (setf (alld:serial-worker-name *om-eval-worker*) *eval-process-name*
          (alld:serial-worker-initial-bindings *om-eval-worker*) *process-initial-bindings*)
    (alld:serial-worker-start *om-eval-worker* :entry #'om-work-function))
  (alld:serial-worker-thread *om-eval-worker*))

(defun init-om-eval-process ()
  (alld:with-serial-worker-lock (*om-eval-worker*)
    (%ensure-om-eval-process-locked)))

(defun %enqueue-on-eval-process (task)
  "Queue TASK, a function of no arguments, on the visual evaluator, inheriting the worker generation's package; the worker starts every job with its armed traps."
  (alld:serial-worker-enqueue
   *om-eval-worker*
   task
   :ensure (lambda (worker)
             (declare (ignore worker))
             (%ensure-om-eval-process-locked))))

(defun om-eval-on-process (expression)
  (%enqueue-on-eval-process
   (lambda ()
     ;; kernel door: the workspace save comes through here and reads headers with
     ;; no package binding of its own (kernel tools/saveheader.lisp: get-pathname-wsparams, kernel ommodele/persistant/save.lisp: omNG-save-ws for OMFolder)
     (let ((*package* (find-package :om)))
       (if (functionp expression) (funcall expression) (eval expression))))))

(defun om-abort-eval-process ()
  "End the current visual-evaluator generation; called ON the worker, let the
running job return into the stop flag instead of destroying the calling thread."
  ;; on the worker itself (a quit evaluated there: system.lisp: om-confirmed-quit) the
  ;; stop flag ends this generation when the job returns and the generation-end function
  ;; of *om-eval-worker* forgets it; forgetting it here would leave this worker alive
  ;; beside a second one.  A finished thread is never destroyed: vanilla's abort cannot
  ;; fail (om-LW lw-lisp-tools/eval-process.lisp: om-abort-eval-process)
  (alld:serial-worker-abort *om-eval-worker*))

