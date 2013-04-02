;----------------------------------------------------------------------------------;
; New scheduler for OM - OO implementation - J. Bresson 2013              ;
;----------------------------------------------------------------------------------;
; Scheduler -- CLPF V1.0  (after Lee Boynton) © 1991 IRCAM                         ;
;----------------------------------------------------------------------------------;
; preFORM 3.0 Scheduler © 1988 Lee Boynton, MIT Media Lab. For non-profit use only ;
;----------------------------------------------------------------------------------;


(in-package :om)

(proclaim '(optimize (speed 3) (safety 0) (space 1)))

(defconstant *highest-priority* 3 "This constant is the highest priority available. It cannot be changed, except in the source code.")

;; =============================================================================-======
;; Task objects (structure)
;; =============================================================================-======

(defstruct
  (task
   (:constructor
    make-task (exectime logtime advance priority function arguments))
   (:print-function
    (lambda (task stream depth)
      (declare (ignore depth))
      (format stream "#<task @ time ~a>" (task-exectime task)))))
  link
  exectime
  logtime
  advance
  priority
  function
  arguments)

;; =============================================================================-======
;;;; 4-state Scheduler
;; =============================================================================-======

(defclass omscheduler ()
  ((internal-error? :accessor internal-error? :initform nil :documentation "contains nil or the last condition which occured") ;; The scheduler is stopped when an internal error occurs
   (scheduler-state :accessor scheduler-state :initform nil :documentation "current state of the scheduler")
   (scheduler-initial-state :accessor scheduler-initial-state :initform :RT :documentation "scheduler's state set by init-scheduler")
   (scheduler-old-state :accessor scheduler-old-state :initform nil :documentation "state which will be restored when nb-waiting-tasks is 0 (in :OOT1 mode)")
   (scheduler-RT? :accessor scheduler-RT? :initform t :documentation "indicates whether the scheduler runs in real time" :initarg scheduler-RT?)
   (scheduler-steps :accessor scheduler-steps :initform 0 :documentation "number of steps the scheduler will execute in :STEP mode")
   (schedulertime--clocktime :accessor schedulertime--clocktime :initform 0 :documentation "difference between the scheduler time and the clock time")
   (scheduler-time :accessor scheduler-time :initform nil :documentation "current scheduler time")
   
   (free-tasks :accessor free-tasks :initform nil)
   (default-task :accessor default-task :initform nil)
   (current-task :accessor current-task :initform nil)
   (current-priority :accessor current-priority :initform -1)
   
   ;;; The wait and ready queues are vectors containing chained tasks (see tcdr)
   ;;; For efficient insertion, there are pointers to the head and queue of each chain.
   ;;; The wait queue is indexed by the exectime of tasks (modulo its size).
   ;;; The ready queue is indexed by the priority of the tasks.
   
   (wait-queue-size :accessor wait-queue-size :initform 2000 :initarg :wait-queue-size)
   (wait-queue-heads :accessor wait-queue-heads :initform nil)
   (wait-queue-tails :accessor wait-queue-tails :initform nil)
   (ready-queue-size :accessor ready-queue-size :initform (+ *highest-priority* 1) :initarg :ready-queue-size)
   (ready-queue-heads :accessor ready-queue-heads :initform nil)
   (ready-queue-tails :accessor ready-queue-tails :initform nil)
   (nb-waiting-tasks :accessor nb-waiting-tasks :initform 0)
   
   (scheduler-process :accessor scheduler-process :initform nil)
   
   ))

(defmethod initialize-instance :after ((self omscheduler) &rest initargs)
  (setf (wait-queue-heads self) (make-array (list (wait-queue-size self))))
  (setf (wait-queue-tails self) (make-array (list (wait-queue-size self))))
  (setf (ready-queue-heads self) (make-array (list (ready-queue-size self))))
  (setf (ready-queue-tails self) (make-array (list (ready-queue-size self)))))

(defun clock-time () (get-internal-real-time))

;; Returns the current real-time.
;; Any task with an exectime smaller than the current real-time is ready to execute.
(defmacro real-time (scheduler) `(+ (clock-time) (schedulertime--clocktime ,scheduler)))

;; =============================================================================-======
;;; User task context (read/write macros) and schedule macros

(defmacro advance (scheduler)
  "Returns the advance of the current scheduler task."
  `(task-advance (current-task ,scheduler)))

(defmacro priority (scheduler)
  "Returns the priority of the current scheduler task."
  `(task-priority (current-task ,scheduler)))

(defmacro logtime (scheduler)
  "Returns the logical time of the current scheduler task."
  `(task-logtime (current-task ,scheduler)))

(defvar *error-when-extra-start?* t
  "Calling \"start\" from within a scheduler task signals an error when this variable
is T (the default).")

(defmacro start (scheduler &body body)
  "Initiates a scheduler pseudo-task which can create new tasks and returns the last
value of its body."
  (let ((start?-var (gensym "START?-")))
    `(let ((,start?-var (null (current-task ,scheduler)))
           (current-task (current-task ,scheduler))
           (current-priority (current-priority ,scheduler))
           rep-value)
       (setf (current-task ,scheduler) (default-task ,scheduler)
             (current-priority ,scheduler) *highest-priority*)
       (if ,start?-var
           (setf (logtime ,scheduler) (if (scheduler-RT? ,scheduler) (real-time ,scheduler) (scheduler-time ,scheduler)))
         (when *error-when-extra-start?*
           (error "~S should not be called from within a scheduler task, with body~%~S"
                  'start '(start ,.body))))
       (setf rep-value (progn ,.body))
       (setf (current-task ,scheduler) current-task
             (current-priority ,scheduler) current-priority)
       rep-value
       )))



(defmacro apdfuncall (scheduler advance priority delay function . arguments)
  "Evaluates immediately all its arguments (producing garbage with the <arguments>
list) and creates a scheduler task with the given <advance> and <priority>, which
will apply <function> to <arguments> after the given <delay>."
  `(when (check-start ,scheduler '(apdfuncall ,scheduler ,advance ,priority ,delay ,function ,.arguments))
     (create-task ,scheduler
      (logtime ,scheduler) ,advance ,priority ,delay ,function (list ,.arguments))))  ;GARBAGE

(defmacro dfuncall (scheduler delay function . arguments)
  "Evaluates immediately all its arguments (producing garbage with the <arguments>
list) and creates a scheduler task with the current advance and priority, which
will apply <function> to <arguments> after the given <delay>."
  `(apdfuncall ,scheduler (advance ,scheduler) (priority ,scheduler) ,delay ,function ,.arguments))


;; This routine does not allocate any new argument list.

(defmacro re-dfuncall (scheduler delay . arguments)
  "Reuses efficiently the current scheduler task with the same function, advance and
priority.  It must not be called more than once from the same task."
  `(let* ((task (current-task ,scheduler))
          (arg-list (task-arguments task)))
     ;; HACK! I need a place to store a flag and a value
     ;; HACK! the delay is stored in (task-exectime task) (see execute-task)
     (setf (task-link task) :re-dfuncall (task-exectime task) ,delay)
     ,.(mapcan
        #'(lambda (arg) (list `(rplaca arg-list ,arg) '(pop arg-list)))
        arguments)
     task))

(defmacro with-more-priority (scheduler &rest body)
  "Continues the current task with the same priority, but the \"dfuncall\"s which
occur inside <body> will create new tasks with the next higher priority level which
is (1+ (priority)) unless *highest-priority* is achieved."
  `(with (((priority ,scheduler) (min *highest-priority* (1+ (priority ,scheduler))))) ,.body))

(defmacro with-less-priority (scheduler &rest body)
  "Continues the current task with the same priority, but the \"dfuncall\"s which
occur inside <body> will create new tasks with the next lower priority level which
is (1- (priority)) unless 0 is achieved."
  `(with (((priority ,scheduler) (max 0 (1- (priority ,scheduler))))) ,.body))

;; =============================================================================-======
;;; The scheduler state and user functions

; (defun scheduler-state () "Returns the state of the scheduler." *scheduler-state*)

(defun set-scheduler-state (scheduler state)
  "Changes the state of the scheduler to <state> which must be one of the
keywords: :RT :STEP :OOT :OOT1."
  (if (eq state :RT)
    (unless (scheduler-RT? scheduler)
      (setf (schedulertime--clocktime scheduler) (- (scheduler-time scheduler) (clock-time))
            (scheduler-RT? scheduler) t))
    (progn
      (setf (scheduler-RT? scheduler) nil)
      (ecase state
             (:OOT1 (unless (eq (scheduler-state scheduler) :OOT1)
                      (setf (scheduler-old-state scheduler) (scheduler-state scheduler))))
             (:STEP
              (format *error-output*
                  ";Warning: The scheduler is stopped. To restart it, try:~%~S"
                  '(set-scheduler-state :rt))
              (setf (scheduler-steps scheduler) 0))
             (:OOT))))
  (setf (internal-error? scheduler) nil 
        (scheduler-state scheduler) state))

(defmacro with-scheduler-OOT1 (scheduler &body body)
  "Starts and evaluates the <body> with the scheduler in the mode \"Out-Of-Time-Once\",
and reverts to the previous mode after evaluation.  This is indispensable when wishing
to use the scheduler mode :OOT1 and starting with an empty scheduler queue."
  `(start ,scheduler (set-scheduler-state ,scheduler :OOT1) ,.body))

(defun scheduler-step (scheduler &optional (nb-step 1))
  "Signals an error if the scheduler was not in the :STEP mode.  Otherwise, the clock
was currently stopped and when the next interrupt comes (probably just after the
evaluation of the current form), the scheduler will execute the <nb-step> (1, by default)
next tasks.  It will first execute the ready ones if any (those which exectime is smaller
than the current scheduler time) or the clock will jump to the next waiting task."
  (unless (eq (scheduler-state scheduler) :STEP)
    (error "The scheduler must be in the :STEP mode. Use the function ~S to stop it."
           'set-scheduler-state))
  (setf (scheduler-steps scheduler) nb-step))

;; =============================================================================-======

;; Signals an error if the function is not embedded in a start

(defun check-start (scheduler form)
  (or (current-task scheduler)
    (error "The form ~S must be called from within a call to the macro ~S."
           form 'start)))

(defmacro tcdr (task) `(task-link ,task))

;; Remakes a task from the free pool.

(defun remake-task (scheduler exectime logtime advance priority function arguments
                             &aux (task (free-tasks scheduler)))
  (setf (free-tasks scheduler) (tcdr task)
        (task-exectime task) exectime
        (task-logtime task) logtime
        (task-advance task) advance
        (task-priority task) priority
        (task-function task) function
        (task-arguments task) arguments)
  task)

;; Creates a new task.
;; Tries to get one from the free pool, otherwise physically creates a new one.

(defun create-task (scheduler logtime adv pri delay fun args)
  (assert
   (or (typep fun 'function) (and (typep fun 'symbol) (fboundp fun)))
   (fun) "Cannot delay the undefined function ~S." fun)
  (om-without-interrupts
   (incf logtime delay)
   (wait-task scheduler
    (if (free-tasks scheduler)
      (remake-task scheduler (- logtime adv) logtime adv pri fun args)
      (make-task (- logtime adv) logtime adv pri fun args)))))

;; Aborts a scheduled task by replacing its function with a null one.

(defun no-op (&rest args) args)

(defun abort-task (task)
  "If the <task> has not begun yet, it makes it inoperant when its exectime comes
by replacing its function with a null one."
  (setf (task-function task) #'no-op)
  task)

;; =============================================================================-======
;;;; Queues

(defun print-scheduler-queue (scheduler &optional (stream t))
  "Prints the current state of the scheduler queues (useful for debug when the scheduler
is stopped in the :STEP mode)."
  (let (task (tmax (+ (scheduler-time scheduler) (wait-queue-size scheduler))))
    (format stream "; At current exec-time ~D (clock=~D) [~S mode]~%"
            (scheduler-time scheduler) (clock-time) (scheduler-state scheduler))
    (format stream "; Ready   |  lat |  exec  |   log  | adv | pri| form~%")
    (do ((index (1- (ready-queue-size scheduler)) (1- index)))
        ((<= index 0))
      (when (setq task (svref (ready-queue-heads scheduler) index))
        (pretty-print-task scheduler task stream)))
    (unless (zerop (nb-waiting-tasks scheduler))
      (format stream "; Waiting |  lat |  exec  |   log  | adv | pri| form~%")
      (do ((the-time (scheduler-time scheduler) (1+ the-time)))
          ((>= the-time tmax))
        (when (setq task (svref (wait-queue-heads scheduler) (mod the-time (wait-queue-size scheduler))))
          (pretty-print-task scheduler task stream))))))

(defun pretty-print-task (scheduler task &optional (stream t))
  (let ((next-task (task-link task)))
    (format stream "; #<task  |~5D |~7D |~7D |~4D |~3D | ~S>~%"
            (- (scheduler-time scheduler) (task-exectime task))
            (task-exectime task)
            (task-logtime task)
            (task-advance task)
            (task-priority task)
            `(,(task-function task)
              ,.(mapcar #'(lambda (arg) `',arg) (task-arguments task))))
    (when next-task (pretty-print-task scheduler next-task stream))))

; At current exec-time 219954 (clock=310151) [:step mode]
; Ready   |  lat |  exec  |   log  | adv | pri| form
; Waiting |  lat |  exec  |   log  | adv | pri| form
; #<task  | -200 | 220154 | 220159 |   5 |  1 | (t-sched::dlooprint '8 '200)>
; #<task  | -350 | 220304 | 220309 |   5 |  1 | (set-scheduler-state :step)>
; #<task  | -850 | 220804 | 220809 |   5 |  1 | (set-scheduler-state :rt)>

;; Inserts a task in the wait-queue.

(defun wait-task (scheduler task &aux (exectime (task-exectime task)))
  (if (< exectime (scheduler-time scheduler))
    (ready-task scheduler task)
    (let* ((index (mod exectime (wait-queue-size scheduler)))
           (head (svref (wait-queue-heads scheduler) index))
           tail)
      (print (nb-waiting-tasks scheduler))
      (incf (nb-waiting-tasks scheduler))
      (print (nb-waiting-tasks scheduler)) 
      (print "===")
      (cond
       ((null head)
        (setf (svref (wait-queue-heads scheduler) index) task
              (svref (wait-queue-tails scheduler) index) task
              (tcdr task) nil))
       ((<= (task-exectime (setq tail (svref (wait-queue-tails scheduler) index))) exectime)
        (setf (tcdr task) nil
              (tcdr tail) task
              (svref (wait-queue-tails scheduler) index) task))
       ((> (task-exectime head) exectime)
        (setf (tcdr task) head
              (svref (wait-queue-heads scheduler) index) task))
       (t (insert-by-time head exectime task)))))
  task)

;; [in :RT mode] Finds the tasks which exectimes are between
;; the current *scheduler-time* and the new *scheduler-time*.
#|
(defun ready-tasks ()
  (om-without-interrupts
   (let* ((real-time (real-time))
          (lateness (- real-time *scheduler-time*))
          index task)
     (when (> lateness 0)
       (if (and (> lateness *wait-queue-size*)
                (> lateness (max (* 1.5 *wait-queue-size*) *highest-latency*)))
         (progn
           (reset-scheduler) ; This is not the right place - should be at execute-task
           (format t "Scheduler late.~%"))
         (if (<= *nb-waiting-tasks* 0)
           (setf *scheduler-time* real-time)
           (do ((the-time *scheduler-time* (1+ the-time)))
               ((> the-time real-time)
                (setq *scheduler-time* real-time))
             (setq index (mod the-time *wait-queue-size*))
             (when (and (setq task (svref *wait-queue-heads* index))
                        (<= (task-exectime task) the-time))
               (ready-tasks-time the-time index task)))))))))
|#

;; [in :RT mode] Finds the tasks which exectimes are between
;; the current *scheduler-time* and the new *scheduler-time*.

(defun ready-tasks (scheduler &aux (real-time (real-time scheduler)))
  (om-without-interrupts
   (when (> (print (nb-waiting-tasks scheduler)) 0)
     (let ((lateness (- real-time (scheduler-time scheduler))) index task tmax)
       (unless (>= lateness 0)
         (error "The real time ~S is smaller than the scheduler time ~S."
                real-time (scheduler-time scheduler))
         )
       (setq tmax (if (<= lateness (wait-queue-size scheduler)) real-time
                      (+ (scheduler-time scheduler) (wait-queue-size scheduler))))
       (do ((the-time (scheduler-time scheduler) (1+ the-time)))
           ((> the-time tmax))
         (setq index (mod the-time (wait-queue-size scheduler)))
         (when (and (setq task (svref (wait-queue-heads scheduler) index))
                    (<= (task-exectime task) real-time))
           (ready-tasks-time scheduler real-time index task)))))
   (setf (scheduler-time scheduler) real-time)
   )
)

;; [in :STEP mode] Increments the scheduler-time until the next ready tasks if any
;; and returns a non-nil value.

(defun ready-tasks? (scheduler)
  (om-without-interrupts
   (when (> (nb-waiting-tasks scheduler) 0)
     (let ((tmin nil)
           (tmax (+ (scheduler-time scheduler) (wait-queue-size scheduler)))
           index task exectime)
       (or
        (do ((the-time (scheduler-time scheduler) (1+ the-time)))
            ((>= the-time tmax) nil)
          (setq index (mod the-time (wait-queue-size scheduler)))
          (when (setq task (svref (wait-queue-heads scheduler) index))
            (if (= (setq exectime (task-exectime task)) the-time)
              (return (ready-tasks-time scheduler the-time index task))
              (when (or (null tmin) (< exectime tmin))
                (setq tmin exectime)))))
        (when tmin
          (ready-tasks-time scheduler
           tmin (setq index (mod tmin (wait-queue-size scheduler)))
           (svref (wait-queue-heads scheduler) index))))))))

;; Extracts the chain of waiting tasks and puts them into the ready queue.

(defun ready-tasks-time (scheduler the-time index task &aux next-task)
  (loop
    (setq next-task (tcdr task))
    (decf (nb-waiting-tasks scheduler))
    (ready-task scheduler task)
    (if (not (and next-task (<= (task-exectime next-task) the-time)))
      (return)
      (setq task next-task)))
  (unless (setf (svref (wait-queue-heads scheduler) index) next-task)
    (setf (svref (wait-queue-tails scheduler) index) nil))
  (setf (scheduler-time scheduler) the-time))

;; Puts the given task into the ready queue.

(defun ready-task (scheduler task)
  (let* ((exectime (task-exectime task))
         (index (task-priority task))
         (head (svref (ready-queue-heads scheduler) index))
         (tail (svref (ready-queue-tails scheduler) index)))
    (cond
     ((null head)
      (setf (svref (ready-queue-heads scheduler) index) task
            (svref (ready-queue-tails scheduler) index) task
            (tcdr task) nil))
     ((<= (task-exectime tail) exectime)
      (setf (tcdr task) nil
            (tcdr tail) task
            (svref (ready-queue-tails scheduler) index) task))
     ((> (task-exectime head) exectime)
      (setf (tcdr task) head
            (svref (ready-queue-heads scheduler) index) task))
     (t (insert-by-time head exectime task))))
  task)

;; Extracts the next task that is ready with respect to the given priority level.

(defun next-ready-task (scheduler priority &aux task)
  (print (ready-queue-heads scheduler))
  (do ((i (1- (ready-queue-size scheduler)) (1- i)))
      ((<= i priority) nil)
    (when (setq task (svref (ready-queue-heads scheduler) i))
      (unless (setf (svref (ready-queue-heads scheduler) i) (tcdr task))
        (setf (svref (ready-queue-tails scheduler) i) nil))
      (return task))))

;;; Debugging the scheduler tasks

(defvar *highest-latency* 300
  "When the difference between the real-time and a task's exectime is greater than the
value of this variable, the task is considered to be very late.")

(defvar *print-on-late?* nil
  "If T (not the default), the task which is late is printed out.")
(defvar *step-on-late?* nil
  "If T (not the default), the task which is late stops the scheduler at the current
time with the :STEP mode.")
(defvar *reset-on-late?* nil
  "If T (not the default), the task which is late resets the scheduler.")
(defvar *eval-on-late?* t
  "If T (the default), the task which is late is executed anyway.")
(defvar *late-task* nil
  "The last task which was late.")

(defvar *print-on-error?* t
  "If T (the default), the task which produced an error is printed out.")
(defvar *step-on-error?* t
  "If T (the default), the task which produced an error stops the scheduler at the current time.")
(defvar *reset-on-error?* nil
  "If T (not the default), the task which produced an error resets the scheduler.")
(defvar *error-task* nil
  "The last task which produced an error.")
(defvar *condition* nil
  "The condition produced by the last task which produced an error.")

;; Executes the task.

(defun execute-task (scheduler task)
  (if (< (- (real-time scheduler) (task-exectime task)) *highest-latency*)
    (eval-task scheduler task)
    (progn
      (setf *late-task* task)
      ;;(when *print-on-late?* (format *error-output* "Late ~S" task))
      ;;Camilo [090593] late tasks printed only once
      (when *print-on-late?* (format *error-output* "Late ~S" task) (setf *print-on-late?* nil))
      (when *step-on-late?* (set-scheduler-state scheduler :STEP))
      (when *reset-on-late?* (reset-scheduler scheduler))
      (when *eval-on-late?* (eval-task scheduler task))))
  nil)

;; Evaluates a task in its dynamic context,
;; disposes of its argument list and puts it into the free pool.

;; [jack] 910905
;; This version works fine.
;; When an error occurs, the user entries the debugger and can inspect whatever,
;; but, when he continues, the scheduler is late.
;; It would be much better to handle the user's errors in a way which stops the
;; scheduler (:STEP mode) and invokes the debugger, with the options to reset or
;; restart the scheduler after debug.

(defun eval-task (scheduler task)
  (let ((current-task (current-task scheduler))
        (current-priority (current-priority scheduler))
        logtime 
        rep)
    
    (setf (current-task scheduler) task
          (current-priority scheduler) (task-priority task))
    
    (apply (task-function task) (task-arguments task))
    (setf rep 
          (cond
           ((eq (task-link task) :re-dfuncall)
            (setf (task-logtime task)
                  ;; hack! the delay is stored in (task-exectime task) (see re-dfuncall)
                  (setq logtime (+ (task-logtime task) (task-exectime task)))
                  (task-exectime task) (- logtime (task-advance task)))
            (wait-task task))
           (t
            ;;; (freelist% (task-arguments task)) ;GARBAGE
            (setf (tcdr task) (free-tasks scheduler)
                  (free-tasks scheduler) task
                  (task-arguments task) nil))))
    
    (setf (current-task scheduler) current-task
          (current-priority scheduler) current-priority)
    
    rep))


;; Inserts a task in a chain according to its exectime.

(defun insert-by-time (queue exectime task &aux (next-task (tcdr queue)))
  (cond
   ((null next-task)
    (setf (tcdr queue) task
          (tcdr task) nil))
   ((> (task-exectime next-task) exectime)
    (setf (tcdr task) next-task
          (tcdr queue) task))
   (t (insert-by-time next-task exectime task))))

;; =============================================================================-======

;; This routine needs to be called periodically to make the scheduler run.

(defvar *nbmax-ticks-per-event* 0)
(defvar *nbmax-tasks-per-event* 100)

;(setf *nbmax-tasks-per-event* 1000)

(defun set-tasks-duration (ms)
  "Sets the maximum duration of time spent in evaluating tasks."
  (setf *nbmax-ticks-per-event*
        (max 0 (ceiling (* internal-time-units-per-second ms) 1000))))

(eval-when (eval load)
  (set-tasks-duration 1000))


;; Fast version (should replace the safe one, if it's safe enough!)

(defun check-scheduler (scheduler timer &aux task tmax nb-tasks)
  (declare (ignore timer))
  ;(print-scheduler-queue scheduler *om-stream*)
  (cond
   ((scheduler-RT? scheduler)
    (setq tmax (+ (clock-time) *nbmax-ticks-per-event*)
          nb-tasks *nbmax-tasks-per-event*)
    (loop
      (ready-tasks scheduler)
      (when (setq task (print (next-ready-task scheduler (current-priority scheduler))))
        (execute-task scheduler task))
      (when (or (null task)
                (<= (decf nb-tasks) 0)
                (>= (get-internal-real-time) tmax))
        (return))))
   ((eq (scheduler-state scheduler) :STEP)
    (when (and (> (scheduler-steps scheduler) 0)
               (setq task (next-ready-task scheduler (current-priority scheduler))))
      (decf (scheduler-steps scheduler))
      (execute-task scheduler task)))
   (t
    (setq tmax (+ (clock-time) *nbmax-ticks-per-event*)
          nb-tasks *nbmax-tasks-per-event*)
    (loop
      (when (setq task (next-ready-task scheduler (current-priority scheduler)))
        (execute-task scheduler task))
      (unless (ready-tasks? scheduler)
        (return
         (when (eq (scheduler-state scheduler) :OOT1)
           (set-scheduler-state scheduler (scheduler-old-state scheduler)))))
      (when (or (<= (decf nb-tasks) 0)
                (>= (clock-time) tmax))
        (return)))))
  nil)


;; Aborts activity past due.
(defun reset-scheduler (scheduler)
  "Erases the scheduler queues.  All the ready and waiting tasks are forgotten."
  (om-without-interrupts
   (setf (nb-waiting-tasks scheduler) 0)
   (dotimes (i (ready-queue-size scheduler))
     (setf (svref (ready-queue-heads scheduler) i) nil
           (svref (ready-queue-tails scheduler) i) nil))
   (dotimes (i (wait-queue-size scheduler))
     (setf (svref (wait-queue-heads scheduler) i) nil
           (svref (wait-queue-tails scheduler) i) nil))
   (setf (free-tasks scheduler) (make-task 0 0 0 0 #'no-op nil)
         (schedulertime--clocktime scheduler) 0
         (scheduler-time scheduler) (clock-time))
   nil))



;;;================
;;; INTERFACE
;;;================

(defmethod init-scheduler ((self omscheduler))
   (setf (default-task self) (make-task 0 0 5 1 #'no-op nil))
   (reset-scheduler self)
   ;; test 
   ;(setf (current-task self) nil)
   (set-scheduler-state self (scheduler-initial-state self))
   (start-scheduler self 'check-scheduler))

(defun start-scheduler (scheduler check-fun)
  (kill-scheduler scheduler) ;;; just in case...
  (om-with-priority 80000000
    (setf (scheduler-process scheduler)
          (om-run-process "OM-SCHEDULER"
                           #'(lambda ()
                              (loop while t do
                                    (sleep 1)
                                    (funcall check-fun scheduler nil)))))))

(defmethod stop-scheduler ((self omscheduler))
  (when (scheduler-process self) 
    (om-kill-process (scheduler-process self))))

(proclaim '(optimize (speed 1) (safety 1) (space 1)))


#|

(setf *sch* (make-instance 'omscheduler))
(init-scheduler *sch*)
; (scheduler-process *sch*)
; (stop-scheduler *sch*)
(defun bar (n) (repeat n (print 'bar)))
(start *sch*
  (dfuncall *sch* 0 'bar 1)
  (dfuncall *sch* 1000 'om-beep)  
  (dfuncall *sch* 6000 'bar 3))
  (dfuncall *sch* 5000 'bar 10)
  (dfuncall *sch* 10000 'bar 1)
 (with-more-priority *sch*
   (setq s1 (dfuncall *sch* 60 'print 'foo))))
|#
