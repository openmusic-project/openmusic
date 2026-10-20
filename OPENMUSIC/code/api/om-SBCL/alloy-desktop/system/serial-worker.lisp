(in-package :alloy-desktop)

(defstruct (serial-worker (:copier nil))
  "A long-lived named thread serving a FIFO queue of jobs one at a time. The thread is created lazily by SERIAL-WORKER-ENSURE-LOCKED, waits on CONDITION while the queue is empty, and ends when STOP-P is set, when a FATAL-ERROR-HANDLER takes an error, or when it is destroyed. Other structures may (:include serial-worker) and keep their own constructors and accessors."
  (name "serial-worker")
  ;; The thread of the current generation, or NIL.
  (thread nil)
  ;; Pending jobs, oldest first, guarded by LOCK.
  (queue nil)
  (lock (bordeaux-threads:make-lock "serial-worker"))
  (condition (bordeaux-threads:make-condition-variable :name "serial-worker"))
  (stop-p nil)
  ;; Specials the thread receives a binding of, valued at SERIAL-WORKER-START (see MAKE-DESKTOP-THREAD).
  (initial-bindings nil)
  ;; The float traps enabled in the thread at its birth and again at the start of every job: handling a signalled trap can leave the thread with another set.
  (armed-traps nil)
  ;; NIL or a function of no arguments returning a list of (symbol . value), bound once by PROGV on entering the loop, in the thread.
  (loop-bindings nil)
  ;; A function (or function name) of one argument called with each job, outside the lock.
  (job-function nil)
  ;; NIL or a function of one condition, bound by HANDLER-BIND for ERROR around the loop: it runs where the error is signalled.
  (error-observer nil)
  ;; NIL or a function of one condition: an ERROR leaving the loop ends the generation after calling it.
  (fatal-error-handler nil)
  ;; NIL or a function of no arguments called under the lock when the generation ends, however it ends.
  (generation-end nil))

(defmethod print-object ((worker serial-worker) stream)
  "Print WORKER by type, name and identity, never its queue or functions: a worker is an argument of the frames of its own thread and so appears in every backtrace taken there."
  (print-unreadable-object (worker stream :type t :identity t)
    (prin1 (serial-worker-name worker) stream)))

(defmacro with-serial-worker-lock ((worker) &body body)
  "Run BODY holding the lock of WORKER."
  `(bordeaux-threads:with-lock-held ((serial-worker-lock ,worker))
     ,@body))

(defun serial-worker-live-p (worker)
  "True when WORKER has a thread and that thread is alive."
  (let ((thread (serial-worker-thread worker)))
    (and thread (bordeaux-threads:thread-alive-p thread))))

(defun %serial-worker-loop (worker)
  "Serve the queue of WORKER until STOP-P is seen, starting every job with exactly the armed traps of WORKER enabled, and running the generation-end function on every way out."
  (let ((bindings (and (serial-worker-loop-bindings worker)
                       (funcall (serial-worker-loop-bindings worker)))))
    (progv (mapcar #'car bindings) (mapcar #'cdr bindings)
      (unwind-protect
           (labels ((serve ()
                    (loop
                      (let ((job (with-serial-worker-lock (worker)
                                   (loop while (and (null (serial-worker-queue worker))
                                                    (not (serial-worker-stop-p worker)))
                                         do (bordeaux-threads:condition-wait
                                             (serial-worker-condition worker)
                                             (serial-worker-lock worker)))
                                   (when (serial-worker-stop-p worker)
                                     (return-from %serial-worker-loop nil))
                                   (pop (serial-worker-queue worker)))))
                        (set-float-traps (serial-worker-armed-traps worker))
                        (funcall (serial-worker-job-function worker) job))))
                  (observed ()
                    (let ((observer (serial-worker-error-observer worker)))
                      (if observer
                          (handler-bind ((error observer)) (serve))
                          (serve)))))
             (let ((fatal (serial-worker-fatal-error-handler worker)))
               (if fatal
                   (handler-case (observed)
                     (error (condition) (funcall fatal condition)))
                   (observed))))
        (let ((end (serial-worker-generation-end worker)))
          (when end
            (with-serial-worker-lock (worker)
              (funcall end))))))))

(defun serial-worker-run (worker)
  "Run the serving loop of WORKER in the calling thread: wait for jobs, call the job function on each, return when stop is requested. It is the body of the thread that SERIAL-WORKER-START creates."
  (%serial-worker-loop worker))

(defun serial-worker-start (worker &key entry)
  "Start a new generation of WORKER and return its thread. The lock of WORKER is already held by the caller. The stop flag and the queue are cleared; the thread is made by MAKE-DESKTOP-THREAD with the name, initial bindings and armed traps of WORKER and runs ENTRY, a function of no arguments, or SERIAL-WORKER-RUN on WORKER."
  (setf (serial-worker-stop-p worker) nil
        (serial-worker-queue worker) nil
        (serial-worker-thread worker)
        (make-desktop-thread (or entry (lambda () (serial-worker-run worker)))
                             :name (serial-worker-name worker)
                             :initial-bindings (serial-worker-initial-bindings worker)
                             :armed-traps (serial-worker-armed-traps worker))))

(defun serial-worker-ensure-locked (worker)
  "Return the live thread of WORKER, starting a generation when there is none. The lock of WORKER is already held by the caller."
  (if (serial-worker-live-p worker)
      (serial-worker-thread worker)
      (serial-worker-start worker)))

(defun serial-worker-enqueue (worker job &key ensure)
  "Append JOB to the queue of WORKER and wake it, in one critical section; with ENSURE, make sure first that a thread is alive. ENSURE is T for SERIAL-WORKER-ENSURE-LOCKED, or a function of WORKER called instead of it, with the lock held: it owns the decision to start a generation, for a worker whose name or bindings are read at each start. Return the value of the wake-up call of BORDEAUX-THREADS:CONDITION-NOTIFY."
  (with-serial-worker-lock (worker)
    (cond ((functionp ensure) (funcall ensure worker))
          (ensure (serial-worker-ensure-locked worker)))
    (setf (serial-worker-queue worker)
          (append (serial-worker-queue worker) (list job)))
    (bordeaux-threads:condition-notify (serial-worker-condition worker))))

(defun serial-worker-request-stop (worker)
  "Set the stop flag of WORKER, discard its queue and wake it, in one critical section. Return the thread read in that section."
  (with-serial-worker-lock (worker)
    (setf (serial-worker-stop-p worker) t
          (serial-worker-queue worker) nil)
    (bordeaux-threads:condition-notify (serial-worker-condition worker))
    (serial-worker-thread worker)))

(defun serial-worker-destroy-thread (worker &optional (thread (serial-worker-thread worker)))
  "Terminate THREAD when it is alive; it defaults to the current thread of WORKER, read when the call starts. The caller guarantees it is not the calling thread."
  (when (and thread (bordeaux-threads:thread-alive-p thread))
    ;; The thread can end between the test and the call; destroying a finished thread signals INTERRUPT-THREAD-ERROR, the one condition ignored here.
    (handler-case
        (when (bordeaux-threads:thread-alive-p thread)
          (bordeaux-threads:destroy-thread thread))
      (sb-thread:interrupt-thread-error () nil))))

(defun serial-worker-abort (worker)
  "End the current generation of WORKER and return NIL. Called from another thread it requests the stop, destroys the thread that request returned, never a generation started after it, and then forgets the thread and clears the stop flag, so that the next ensure starts a new generation. Called from the thread of WORKER itself it only requests the stop: the job in progress returns and the loop ends the generation."
  (let ((thread (serial-worker-request-stop worker)))
    (unless (eq thread (bordeaux-threads:current-thread))
      (serial-worker-destroy-thread worker thread)
      (with-serial-worker-lock (worker)
        (when (eq (serial-worker-thread worker) thread)
          (setf (serial-worker-thread worker) nil))
        (setf (serial-worker-stop-p worker) nil))))
  nil)
