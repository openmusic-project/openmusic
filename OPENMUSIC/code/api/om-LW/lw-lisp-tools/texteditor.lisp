;;===========================================================================
;LW Lisp Tools 
;Lisp programming tools for LispWorks delivered applications
;;===========================================================================

;;===========================================================================
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Jean Bresson, Sheldon Ball, Nicholas Ellis
;Initial code from S. Ball's ANVITA editor
;;===========================================================================

;;; (om-lisp::om-open-new-text-editor)

;;;======================================
;;; OM TEXT EDITOR GENERAL
;;;======================================
;;; J. Bresson
;;; Initial code from S. Ball's Anvita Editor
;;; Contributions by N. Ellis

(in-package :om-lisp)

(export '(
          om-text-editor
          om-open-new-text-editor
          om-open-new-text-file
          om-window-class-menubar
          om-set-window-title
          om-destroy-callback
         ) :om-lisp)

;; ERRORS HANDLING from S. Ball
;; this causes editor errors to be reported in echo area
;;(setf editor::*dont-catch-errors* nil)

;; (compile&load (make-pathname :directory *api-directory* :name "editor-find-dialogs"))

(defvar *ae-error-log* 
  (make-pathname :name "OM Text Editor" 
                 :type "log" 
                 #+(or win32 macozsx) :defaults #+(or win32 macozsx) (sys:get-folder-path :local-appdata)
                 ))

;; this causes errors in the interface process to be displayed in a message box
(defun report-ae-error (cc)
   (display-message "Error ~S in OM Text Editor Process" cc)
   (with-open-file (log *ae-error-log* 
                        :direction :output 
                        :if-exists :append 
                        :if-does-not-exist :create)
     (dbg:output-backtrace :verbose :stream log))
   (abort))

;; just to distinguish editor-error
(defun report-ae-editor-error (cc)
   (display-message "Editor Error ~S in OM Text Editor Process" cc)
   (with-open-file (log *ae-error-log* 
                        :direction :output 
                        :if-exists :append 
                        :if-does-not-exist :create)
     (dbg:output-backtrace :verbose :stream log))
   (abort))

(defun handle-ae-error (func interface)
  (declare (ignore interface))
  (handler-bind 
      ((editor:editor-error #'report-ae-editor-error)
       (error #'report-ae-error))
    (funcall func)))

(defun echo-string (interface string)
  (with-slots (ep) interface
    (ignore-errors 
      (when (editor-window ep)
        (editor:process-character 
         (list 'editor:message string) 
         (editor-window ep))))))
  
(defun report-file-operation (interface op file)
  (when op
    (echo-string interface (format nil "~A file ~A" op file))))

(defun report-failed-file-operation (interface op file)
  (declare (ignore interface))
  (display-message "Failed to ~A file ~A" op file))

(defmacro with-safe-file-operation (interface path present past &body forms)
 `(restart-case
      (prog1 ,@forms (report-file-operation ,interface ,past ,path))
    (abort ()
           (report-failed-file-operation ,interface ,present ,path))))

;;;======================================
;;; OM Text Editor peut editer un BUFFER ou un FICHIER (TEXTE ou LISP)
;;;======================================

(defvar *om-text-editor-initial-xy*)
(setq *om-text-editor-initial-xy* #(100 100))
(setq *om-text-editor-count* 0)
(defvar *om-text-editor-creator-code*)
(setq *om-text-editor-creator-code* "")
(defvar *om-text-editor-type-code*)
(setq *om-text-editor-type-code* "")


(defclass om-text-editor (capi::interface)
  ((editor-buffer :initform nil :accessor editor-buffer :initarg :editor-buffer)
   (ep :initform nil :accessor ep :initarg :ep)
   (editor-file :initform nil :accessor editor-file :initarg :editor-file)
   (lisp-editor? :initform nil :accessor lisp-editor? :initarg :lisp-editor?))
  ;;; editor-buffer = ombuffer
  ;;; editor-file = pathname (optional)
  ;;; lisp-editor? = t or nil (if t : syntax color, load, etc.)
  (:default-initargs
   :best-x (+ (aref *om-text-editor-initial-xy* 0) (* 15 (mod (incf *om-text-editor-count*) 10))
              (mod (* 200 (floor (/ *om-text-editor-count* 10))) 800))
   :best-y (+ (aref *om-text-editor-initial-xy* 1) (* 15 (mod *om-text-editor-count* 10)))
   :best-height 400
   :best-width 500
   :external-min-width 150 :external-min-height 150
   :layout (make-instance 'capi:simple-layout)
   :message-area t  
   :create-callback 'init-text-editor
   ;:display-callback 'init-display
   :destroy-callback 'destroy-text-editor
   :confirm-destroy-function 'check-close-buffer
   ;;;:top-level-hook 'handle-ae-error 
   :title "Text Editor"))

;(defun init-display (texteditor)
;  (print "hg"))

;;; text edit menubar
(defmethod internal-window-class-menubar ((self om-text-editor)) 
  (list (make-instance 'capi::menu :title "File"
                               :items 
                               (list (make-instance 'capi::menu-component 
                                                                :items (list 
                                                                        (make-instance 'capi::menu-item :title "New..."
                                                                                     :callback-type :none
                                                                                     :callback 'open-new-text-editor
                                                                                     :accelerator #\n
                                                                                     :enabled-function 'file-operations-enabled)
                                                                      (make-instance 'capi::menu-item :title "Open..."
                                                                                     :callback-type :none
                                                                                  :callback 'open-text-file
                                                                                  :accelerator #\o
                                                                                  :enabled-function 'file-operations-enabled)))
                                        (make-instance 'capi::menu-component 
                                                                :items (list 
                                                                        (make-instance 'capi::menu-item :title "Import From..."
                                                                                     :callback-type :interface
                                                                                     :callback 'import-text-from-file)
                                                                      (make-instance 'capi::menu-item :title "Revert to Saved"
                                                                                     :callback-type :interface
                                                                                  :callback 'revert-text-file
                                                                                  :accelerator nil
                                                                                  :enabled-function #'(lambda (item) 
                                                                                                        (editor-file item)))))
                                        (make-instance 'capi::menu-component 
                                                                :items (list 
                                                                        (make-instance 'capi::menu-item :title "Save"
                                                                                     :callback-type :interface
                                                                                     :callback 'save-text-file
                                                                                     :accelerator #\s
                                                                                     :enabled-function 'save-operation-enabled)
                                                                      (make-instance 'capi::menu-item :title "Save As..."
                                                                                     :callback-type :interface
                                                                                  :callback 'save-as-text-file)))
                                        (make-instance 'capi::menu-item :title "Close"
                                                            :callback-type :interface
                                                            :callback 'close-text-editor-window
                                                            :accelerator #\w)
                                       
                                       ))
                       (make-instance 'capi::menu :title "Edit"
                                      :items (list 
                                              (make-instance 'capi::menu-item :title "Undo"
                                                            :callback-type :interface
                                                            :callback 'text-edit-undo
                                                            :accelerator #\z)
                                              (make-instance 'capi::menu-component 
                                                                  :items (list 
                                                                               (make-instance 'capi::menu-item :title "Cut"
                                                                                              :callback-type :interface
                                                                                              :callback 'text-edit-cut
                                                                                              :accelerator #\x)
                                                                               (make-instance 'capi::menu-item :title "Copy"
                                                                                              :callback-type :interface
                                                                                              :callback 'text-edit-copy
                                                                                              :accelerator #\c)
                                                                               (make-instance 'capi::menu-item :title "Paste"
                                                                                              :callback-type :interface
                                                                                              :callback 'text-edit-paste
                                                                                              :accelerator #\v)))
                                              (make-instance 'capi::menu-item :title "Select All" 
                                                             :callback 'text-select-all 
                                                             :accelerator #\a
                                                             :callback-type :interface)
                                              (make-instance 'capi::menu-component 
                                                                  :items (list (make-instance 'capi::menu-item :title "Text Font"
                                                                                              :callback-type :interface
                                                                                              :callback 'change-text-edit-font
                                                                                              :accelerator nil)
                                                                               ))
                                               (make-instance 'capi::menu-component 
                                                                  :items (list 
                                                                               (make-instance 'capi::menu-item :title "Find..."
                                                                                              :callback-type :interface
                                                                                              :callback 'find-in-file
                                                                                              :accelerator #\f)
                                                                               (make-instance 'capi::menu-item :title "Replace..."
                                                                                              :callback-type :interface
                                                                                              :callback 'replace-in-file
                                                                                              :accelerator #\r)
                                                                               ))
                                              (make-instance 'capi::menu-item :title "Search..." 
                                                             :callback 'search-files 
                                                             :accelerator nil
                                                             :enabled-function 'disabled
                                                             :callback-type :interface)))
                       (make-instance 'capi::menu :title "Lisp"
                                      :items (list 
                                              (make-instance 'capi::menu-component 
                                                                  :items (list 
                                                                               (make-instance 'capi::menu-item :title "Eval All"
                                                                                              :callback-type :interface
                                                                                              :callback 'eval-lisp-buffer
                                                                                              :enabled-function 'lisp-operations-enabled
                                                                                              :accelerator #\y)
                                                                               (make-instance 'capi::menu-item :title "Eval Region"
                                                                                              :callback-type :interface
                                                                                              :callback 'eval-lisp-region
                                                                                              :enabled-function 'lisp-operations-enabled
                                                                                              :accelerator #\e)))
                                              (make-instance 'capi::menu-component 
                                                                  :items (list 
                                                                               (make-instance 'capi::menu-item :title "Find Definition"
                                                                                              :callback-type :interface
                                                                                              :callback 'find-definition
                                                                                              :enabled-function 'lisp-operations-enabled
                                                                                              :accelerator #\.)))
                                              
                                               (make-instance 'capi::menu-component 
                                                                  :items (list 
                                                                               (make-instance 'capi::menu-item :title "Load File..."
                                                                                              :callback-type :interface
                                                                                              :callback 'load-lisp-file
                                                                                              :enabled-function 'save-operation-enabled
                                                                                              :accelerator nil)
                                                                              ))
                                              ;(make-instance 'capi::menu-component 
                                              ;                    :items (list 
                                              ;                                 (make-instance 'capi::menu-item :title "Abort"
                                              ;                                                :callback-type :interface
                                              ;                                                :callback 'text-edit-abort
                                              ;                                                :enabled-function 'lisp-operations-enabled
                                              ;                                                :accelerator #\.)
                                              ;                                ))
                                              ))

                       )
                )


;;; additional specific app text edit menu bar items
;;; possibly redefined/extended by om-text-editor subclasses
(defmethod om-window-class-menubar ((self om-text-editor))  
  (list (make-instance 'capi::menu 
                       :title "Windows" 
                       :callback-type :none
                       :items (list
                               (make-instance 'capi::menu-item 
                                              :title "Listener" 
                                              :accelerator "accelerator-L"
                                              :setup-callback-argument :item
                                              :callback-type :none
                                              :callback #'(lambda () (om-make-new-listener :initial-lambda #'(lambda () (in-package :om)))))
                               (make-instance 
                                      'capi:menu-component
                                      :items (mapcar 
                                              #'(lambda (w) 
                                                  (make-instance 'capi::menu-item 
                                                                 :title (interface-title w)
                                                                 :setup-callback-argument :item
                                                                 :callback-type :none
                                                                 :callback #'(lambda () (capi::find-interface 
                                                                                         (type-of w) 
                                                                                         :name (capi::capi-object-name w)))))
                                              (capi::collect-interfaces 'om-text-editor))
                                      :callback-type :item
                                      :interaction :no-selection)
                               )
                       )))

;; used for finding windows by name
(defmethod capi::interface-match-p ((self om-text-editor) &rest initargs  &key name)
  (string-equal (capi::capi-object-name self) name))

;;; A record for open files
(defvar *editor-files-open* nil)

(defun find-open-file (path list)
  (let ((found nil))
    (loop for item in list
          while (not found) do        
          (when (and (editor::buffer-pathname (buffer (editor-buffer item)))
                     (string-equal (namestring path) (namestring (editor::buffer-pathname (buffer (editor-buffer item))))))
            (setf found item)))
    found))

; (setf *editor-files-open* nil)

(defmethod om-window-class-menubar ((self t)) nil)

(defun disabled (win) nil)

;;; buffer has been modified and not saved
(defmethod buffer-modified-p ((self om-text-editor))
  (when (editor-buffer self)
    (editor:buffer-modified (buffer (editor-buffer self)))))

(defmethod lisp-operations-enabled ((self t)) t)
(defmethod lisp-operations-enabled ((self om-text-editor)) (lisp-editor? self))

(defmethod file-operations-enabled ((self t)) t)
(defmethod file-operations-enabled ((self om-text-editor)) t)

(defmethod save-operation-enabled ((self om-text-editor)) (buffer-modified-p self))


;;;=================
;;; OPEN FILE / BUFFER
;;;=================

;;;
;;; SET A BUFFER TO EDIT
;;; text file
(defun display-text-file (interface buffer path)
  (with-slots (ep) interface
    (setf (editor-pane-buffer ep) buffer))
  (echo-string interface ""))

;;; buffer only
(defun display-buffer (interface buffer)
  (with-slots (ep) interface
    (setf (editor-pane-buffer ep) buffer))
  (echo-string interface "")
  )

;;; OPEN A BUFFER given or created
;;; Previous buffer is not killed
(defun open-new-buffer (interface &optional buffer)
  (with-slots (ep) interface
    (let ((new-buffer (or buffer (editor::make-buffer (string (gensym))))))
      (display-buffer interface new-buffer)
      ;(if (lisp-editor? interface) 
       ;   (call-editor ep (list 'editor:lisp-mode-command (editor-pane-buffer ep))))
      (echo-string interface "")
      )))

(defvar *last-open-directory* nil)

;;; OPEN A FILE given or prompted
;;; Previous buffer is not killed
(defun open-new-file (interface &optional path)
  (with-slots (ep) interface
    (when-let* ((path (or path 
                          (prompt-for-file "Open File:" 
                                           :pathname *last-open-directory*
                                           :if-does-not-exist :error 
                                          :filter "*.*" 
                                          :filters '("Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*"))))
                (buffer (with-safe-file-operation interface path "open" nil (editor:find-file-buffer path))))
      (display-text-file interface buffer path)
    ;(if (lisp-editor? interface) 
     ;   (call-editor ep (list 'editor:lisp-mode-command (editor-pane-buffer ep))))
    (setf *last-open-directory* (make-pathname :directory (pathname-directory path)))
    (echo-string interface ""))))
  
;;;================================
;;; INIT FUNC : depends on the attributes of the window
;;; if file -> open file
;;; if buffer --> edit buffer
;;; else --> create and edit new buffer
(defmethod init-text-editor ((self om-text-editor))
  (capi::execute-with-interface self
       #'(lambda () 
           (with-slots (ep) self
             (cond ((and (editor-file self) (editor-buffer self))
           (open-new-file self (editor-file self))
           (setf (buffer (editor-buffer self)) (capi::editor-pane-buffer ep))
           (setf (window (editor-buffer self)) self))
          ((editor-file self) 
           (open-new-file self (editor-file self))
           (setf (editor-buffer self) (make-instance 'ombuffer :buffer (capi::editor-pane-buffer ep)))
           (setf (window (editor-buffer self)) self))
          ((editor-buffer self)
           (open-new-buffer self (buffer (editor-buffer self)))
           (setf (window (editor-buffer self)) self))
          (t (open-new-buffer self)
             (setf (editor-buffer self) (make-instance 'ombuffer :buffer (capi::editor-pane-buffer ep)))
             (setf (window (editor-buffer self)) self)
             )
          )))
))

;;; CHANGE EDITOR-BUFFER DELETE SETS THE WINDOW ATTRIBUTE FOR BUFFER
;;; Previous buffer is not killed
(defmethod (setf editor-buffer) :before ((buffer ombuffer) (self om-text-editor))
  (when (editor-buffer self)
    (setf (window (editor-buffer self)) nil)))

(defmethod (setf editor-buffer) :after ((buffer ombuffer) (self om-text-editor))
  (setf (window (editor-buffer self)) self))

(defmethod (setf editor-file) :after (path (self om-text-editor))
  (if path
      (setf (capi::interface-title self) (namestring path))
    (setf (capi::interface-title self) "New Buffer")))


;;; IMPORT : imports text from a file
;;; keeps the same ombuffers and change its internal buffer attribute 
;;; (previous is killed)
(defmethod import-text-from-file ((self om-text-editor) &optional path)
  (with-slots (ep) self
    (when-let* ((path (or path (prompt-for-file "Open File:" 
                                                :if-does-not-exist :error 
                                                :filter "*.*"
                                                :filters '("Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*"))))
                (current (editor-pane-buffer ep))
                (newbuffer (with-safe-file-operation self path "open" nil 
                             (editor:find-file-buffer path)))
                (newtext (editor::use-buffer newbuffer
                           (editor:points-to-string 
                            (editor:buffers-start newbuffer) 
                            (editor:buffers-end newbuffer)))))
      (editor::use-buffer (buffer (editor-buffer self))
        (editor::clear-buffer (buffer (editor-buffer self)))
        (editor::insert-string (editor::buffers-start (buffer (editor-buffer self))) newtext))
      (setf (editor:buffer-modified (buffer (editor-buffer self))) t)
      (editor::kill-buffer-no-confirm newbuffer))))


;;; OPEN file in the same window :
;;; changes the buffer accordingly
;;; and updates editor-file
;;; (supposed to be enabled when editor-file is non-NIL)
(defmethod open-new-text-file ((self om-text-editor) &optional path)
   (with-slots (ep) self
     (let ((current (capi::editor-pane-buffer ep)))
       ;;; save before open another one ?
       (check-save-buffer self)
       (open-new-file self path)
       (setf (buffer (editor-buffer self)) (capi::editor-pane-buffer ep))
       (setf (editor-file self) (editor:buffer-pathname (capi::editor-pane-buffer ep)))
       (editor::kill-buffer-no-confirm current))))
  
;;;=====================
;;; CREATE-WINDOW
;;;=====================

(defvar *editor-class* 'om-text-editor)

;;; NEW EDITOR
;;; (called from menu "New")
(defun open-new-text-editor (&optional path)
  (let ((win (when path (find-open-file path *editor-files-open*))))
    (if win (capi::find-interface (type-of win) :name (capi::capi-object-name win))
      (let* ((buffer (if path (editor:find-file-buffer path)
                       (editor::make-buffer (string (gensym))))))
        (setf win (make-instance *editor-class*
                                 :name (string (gensym))
                                 :x 200 :y 200 :width 800 :height 800
                                 :parent (capi:convert-to-screen)
                                 :internal-border 5 :external-border 0
                                 :display-state :normal
                                 :internal-min-height 200 :internal-min-width 300
                                  ;:font (om-make-font "courier" 20)
                                 :title (if path (namestring path) "New Buffer") 
                                 :editor-buffer (make-instance 'ombuffer :buffer buffer)
                                 :editor-file path
                                 :activate-callback #'(lambda (win activate-p) 
                                                        (when activate-p
                                                          (setf (capi::interface-menu-bar-items win) 
                                                                (append (internal-window-class-menubar win)
                                                                        (om-window-class-menubar win)))))
                                 :lisp-editor? t
                                 ))
        (setf (capi::layout-description (capi::pane-layout win)) 
              (list (setf (ep win) (make-instance 'capi::editor-pane :echo-area t 
                                                  :font *def-text-edit-font*
                                                  ))))
        (push win *editor-files-open*)
        (capi::display win)
        ;(capi::execute-with-interface win
        ;                              #'(lambda ()
        ;                                  (when (lisp-editor? win) 
        ;                                      (call-editor (ep win) 
        ;                                                   (list 'editor:lisp-mode-command (editor-pane-buffer (ep win))))
        ;                                      (echo-string win ""))))
        win
        ))))

(defmethod capi:interface-display :after ((win om-text-editor))
  (capi::execute-with-interface win
                                #'(lambda ()
                                    (when (lisp-editor? win) 
                                              (call-editor (ep win) 
                                                           (list 'editor:lisp-mode-command (editor-pane-buffer (ep win))))
                                              (echo-string win "")))))

;;; NEW editor window + open file
;;; called from menu "Open..."
(defun open-text-file ()
  (let ((path (prompt-for-file "Open File:" 
                               :pathname *last-open-directory*
                               :if-does-not-exist :error 
                               :filter "*.*" 
                               :filters '("Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*"))))
    (when path
      (setf *last-open-directory* (make-pathname :directory (pathname-directory path)))
      (open-new-text-editor path))))

;;;======================
;;; EXPORTED INTERFACE
;;;======================

;;; ouvre un fichier ou un new buffer
(defun om-open-new-text-editor (&optional path)
  (open-new-text-editor path))

; cherche def in path
(defun om-open-new-text-editor-at (path def)
  (let ((edwin (open-new-text-editor path))
        (string-to-search (concatenate 'string (string (car def))
                                        "[!\*]* "
                                        (string (cadr def)))
                          ))
    ;(print string-to-search)
    (when edwin
    (capi::execute-with-interface edwin
                                  #'(lambda () 
                                      (with-slots (ep) edwin
                                        (let ((buffer (editor-pane-buffer ep)))
                                          (editor::use-buffer buffer
                                            (call-editor ep (list 'EDITOR::REGEXP-FORWARD-SEARCH-COMMAND buffer string-to-search))
                                            ))))))
    edwin))

;;; ouvre un fichier only
(defmethod om-open-new-text-file (&optional path)
  (if (not path)
      (multiple-value-bind
          (a b)
          (capi:with-dialog-results (new-path ok)
              (capi:prompt-for-file "Choose a File" :if-does-not-exist :error
                                    :filter "*.*"
                                    :filters '("Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*")
                                    :pathname *last-open-directory*)
            (when ok
              (setf *last-open-directory* (make-pathname :directory (pathname-directory new-path)))
              (open-new-text-editor new-path))))
    (open-new-text-editor path)))

(defmethod om-set-window-title ((self om-text-editor) (title string))
  (setf (capi::interface-title self) title))


;;;====================
;;; SAVE
;;;====================

;;;
;;;  Utility functions
;;;

(defun set-hfs-codes (path type creator)
 #+cocoa
 (objc:with-autorelease-pool ()
   (labels ((transform-for-endianess (x)
            (if (find :little-endian *features*)
                (- 3 x)
              x))
          (code-for-string (str)
            (if (>= (length str) 4)
                (loop for i from 0 to 3
                        sum
                        (ash (char-code (aref str i)) (ash (transform-for-endianess i) 3)))
              0))
          (set-value-for-key (dict str key)
            (objc:invoke dict "setValue:forKey:"
                    (objc:invoke "NSNumber" "numberWithUnsignedInt:" (code-for-string str)) key)))
     (let ((dict (objc:invoke "NSMutableDictionary" "dictionaryWithCapacity:" 2)))
       (set-value-for-key dict type "NSFileHFSTypeCode")
       (set-value-for-key dict creator "NSFileHFSCreatorCode")
       (objc:invoke (objc:invoke "NSFileManager" "defaultManager") "changeFileAttributes:atPath:" dict (namestring path))
       (objc:invoke-into 'string dict "description"))))
)

(defun save-to-file (text path win)
    (with-open-file (ss path :direction :output :if-exists :supersede)
      (write-string text ss))
    (set-hfs-codes path *om-text-editor-type-code* *om-text-editor-creator-code*))

;;; SAVE the current buffer to the attached pathname
;;; or save as + change if no pathname
(defmethod save-text-file ((self om-text-editor))
  (with-slots (ep) self
    (let ((current (editor-pane-buffer ep)))
      (if (editor-file self)
          ;;; SAVE
          (let ((path (editor:buffer-pathname current)))
            (with-safe-file-operation self path "save" "Saved"
              (call-editor ep (list 'editor:save-file-command current))
              (set-hfs-codes path *om-text-editor-type-code* *om-text-editor-creator-code*)))
        ;;; SAVE AS
        (save-as-text-file self)
        ;(when-let (path (prompt-for-file "Save file as:"
        ;                                 :pathname (editor-file self)
        ;                                 :if-does-not-exist :ok
        ;                                 :filter "*.*"
        ;                                 :if-exists :prompt
        ;                                 :operation :save))
        ;  (with-safe-file-operation self path "save as" "Saved as"
        ;    (save-to-file (editor:points-to-string 
        ;                 (editor:buffers-start current) 
        ;                 (editor:buffers-end current))
        ;                  path self)
        ;    )
        ;  (open-new-file self path)
        ;  (setf (buffer (editor-buffer self)) (editor-pane-buffer ep)) 
        ;  (editor::kill-buffer-no-confirm current)
        ;  (setf (editor-file self) path)   
        ;  )
        ))))


;;; SAVE AS : the current buffer to a new file
;;; any buffer ok, always works
;;; do not change current buffer
(defmethod save-as-text-file ((self om-text-editor))
  (with-slots (ep) self
    (let ((current (editor-pane-buffer ep)))
      (when-let (path (prompt-for-file "Save file as:"
                                   :pathname (editor-file self)
                                   :if-does-not-exist :ok
                                   :filter "*.*"
                                   :if-exists :prompt
                                   :operation :save))
        (save-to-file (editor:points-to-string 
                         (editor:buffers-start current) 
                         (editor:buffers-end current))
                      path self)
        (editor::kill-buffer-no-confirm current)
        (open-new-file self path)
        (setf (buffer (editor-buffer self)) (capi::editor-pane-buffer ep))
        (setf (editor-file self) path)
        ))))

      ;  (if (string-equal (namestring path) (namestring (editor-file self)))
      ;      (with-safe-file-operation self path "save" "Saved"
      ;        (call-editor ep (list 'editor:save-file-command current))
      ;        (set-hfs-codes path *om-text-editor-type-code* *om-text-editor-creator-code*))
      ;    (with-safe-file-operation self path "save as" "Saved as"
      ;      (save-to-file (editor:points-to-string 
      ;                     (editor:buffers-start current) 
      ;                     (editor:buffers-end current))
      ;                    path self)))
      ;    t))))


       


;;; REVERT : put the file contents back in the editor
(defmethod revert-text-file ((self om-text-editor))
  (if (editor-file self)
    (with-slots (ep) self
      (let* ((current (editor-pane-buffer ep))
             (path (editor:buffer-pathname current)))
        (with-safe-file-operation self path "revert" "Reverted"
          (call-editor ep (list 'editor:revert-buffer-command nil current nil)))
        ))
    (error "No file is attached to this editor")))

;;;================================
;;; CLOSE / DESTROY
;;;================================

;;; CLOSE
(defmethod close-text-editor-window ((self om-text-editor))
  ;(check-save-buffer self)
  ;(destroy self)
  (capi::execute-with-interface self 'quit-interface self))

;;; DESTROY CALLBACK
(defmethod destroy-text-editor ((self om-text-editor)) nil
  (om-destroy-callback self))

;;; redefined in om-api windows
(defmethod om-destroy-callback  ((self om-text-editor)) 
  (editor::kill-buffer-no-confirm (buffer (editor-buffer self)))
  (setf *editor-files-open* (remove self *editor-files-open*))
  (setf (window (editor-buffer self)) nil))

;;; OLD STYLE : "EXIT ANYWAY"
;(defmethod check-close-buffer ((self om-text-editor))
;   (if (save-operation-enabled self)
;       (multiple-value-bind (answer successp)
;           (capi:prompt-for-confirmation
;            (format nil "Changes on ~A were not saved. Close anyway?"
;                    (if (editor-file self) (pathname-name (editor-file self)) "this text buffer"))
;            :cancel-button nil)
;         answer)
;    t))
  
;;; SAVE-operations-enabled
;;; NEW STYLE : "SAVE BEFORE CLOSE?"
;;; check if : Buffer modified A  
(defmethod check-close-buffer ((self om-text-editor))
  (if (and (om-lisp::buffer-modified-p self) 
           (om-lisp::save-operation-enabled self))
      (multiple-value-bind (answer successp)
          (capi:prompt-for-confirmation
           (format nil "Changes on ~A were not saved.~% Save before closing?"
                   (if (editor-file self) (pathname-name (editor-file self)) "this text buffer"))
           :cancel-button t :default-button :ok)
        (if answer
            (with-slots (ep) self
              (let ((current (capi::editor-pane-buffer ep))
                    (path (or (editor-file self)
                              (prompt-for-file "Save file as:"
                                               :pathname (editor-file self)
                                               :if-does-not-exist :ok
                                               :filter "*.*"
                                               :if-exists :prompt
                                               :operation :save))))
                (if path
                    ;;; (call-editor ep (list 'editor:save-file-command current))
                    (save-to-file 
                     (editor:points-to-string 
                      (editor:buffers-start current) 
                      (editor:buffers-end current))
                     path self)
                  (setf successp nil))))
          )
        successp)
    t))

;;; checks all not-saved text windows
;;; ask for save each time and retruns t or nil if cancel
(defun check-buffers-before-close ()
  (let ((editors (capi::collect-interfaces 'om-text-editor))
        (ok t))
    (loop for ed in editors 
          while ok do
          (when (save-operation-enabled ed)
            (capi::find-interface (type-of ed) :name (capi::capi-object-name ed))
            (setf ok (check-close-buffer ed)))
          ;(when ok (capi::destroy ed))
          )
    ok))


;;;=====================
;;; TEXT EDIT TOOLS 
;;;=====================

(defmethod text-edit-copy ((self om-text-editor))
  (with-slots (ep) self
    (let ((buffer (editor-pane-buffer ep)))
      (call-editor ep (list 'editor::copy-to-cut-buffer-command buffer)))))

(defmethod text-edit-cut ((self om-text-editor))
  (with-slots (ep) self
    (let ((buffer (editor-pane-buffer ep)))
      (call-editor ep (list 'editor::copy-to-cut-buffer-command buffer))
      (call-editor ep (list 'editor::kill-region-command buffer)))))

(defmethod text-edit-paste ((self om-text-editor))
  (with-slots (ep) self
    (let ((buffer (editor-pane-buffer ep)))
      (call-editor ep (list 'editor::insert-cut-buffer-command buffer)))))

(defmethod text-edit-undo ((self om-text-editor))
  (with-slots (ep) self
    (let ((buffer (editor-pane-buffer ep)))
      (call-editor ep (list 'editor::undo-command buffer)))))


(defvar *def-text-edit-font* nil)

(defmethod change-text-edit-font ((self om-text-editor))
  (with-slots (ep) self
    (setf (capi::simple-pane-font ep) 
          (setf *def-text-edit-font*
                (capi::prompt-for-font "" :font (capi::simple-pane-font ep))))))


;;; SELECT ALL BUFFER
;;; to do ge back to initial position...
(defmethod text-select-all ((self om-text-editor))
    (with-slots (ep) self
      (let ((buffer (editor-pane-buffer ep)))
        (editor::use-buffer buffer
          (editor::with-point ((p (editor::buffer-point buffer)))
            (call-editor ep (list 'editor::beginning-of-buffer-cancelling-selection-command buffer))
            #+cocoa(call-editor ep (list 'editor::end-of-buffer-extending-selection-command buffer))
            #-cocoa(call-editor ep (list 'editor::end-of-buffer-modifying-selection-command buffer))
            ;(editor::goto-line buffer 4)
            ;(call-editor ep (list 'editor::goto-point-command buffer p))
            ;(editor::move-point (editor::current-point) p)
            ;(editor::set-current-mark (editor::current-point))
            ;(call-editor ep (list 'editor::goto-point-command p 3))
            )))))


;;; Utile pour la suite...
;;;
;;; EDITOR::APROPOS-COMMAND --> symbol browser
;;; EDITOR::BACKUP-FILE-COMMAND --> cree un backup sans changer le buffer
;;; EDITOR::COMMENT-REGION-COMMAND
;;; EDITOR::EVALUATE-REGION-COMMAND

;;;=====================
;;; LISP TOOLS
;;;=====================

(setf editor::*SHOW-EDITOR-EVAL-WARNING* nil)

;; :buffer or :process
(defparameter *lisp-eval-mode* :buffer)
(defvar *lisp-eval-current-editor-file*)

;;; EVAL the buffer...
(defmethod eval-lisp-buffer ((self om-text-editor))
  (with-slots (ep) self
    (setf *lisp-eval-current-editor-file* (editor-file self))
    (if (equal *lisp-eval-mode* :buffer)
        (let ((buffer (editor-pane-buffer ep)))
          (call-editor ep (list 'editor::evaluate-buffer-command buffer)))
      (eval-string-on-process (capi:editor-pane-text ep)))
    ))

(defun eval-string-on-process (forms-as-string)
 (let ((commands (concatenate 'string forms-as-string (format nil "~Ct" #\Newline))))
   (loop while (> (length commands) 0)
         do
         (handler-case
             (multiple-value-bind (form new-pos) (read-from-string commands)
               (om-lisp::om-eval-on-process form)
               (loop while (and (< new-pos (length commands)) (lw:whitespace-char-p (aref commands new-pos)))
                     do
                     (incf new-pos))
               (setf commands (subseq commands new-pos)))
           (t (condition) (progn (capi::display-message "error ~A" condition) (abort)))))))


(defmethod eval-lisp-region ((self om-text-editor))
 (with-slots (ep) self
   (let* ((buffer (editor-pane-buffer ep))
          (command
           (if (editor:variable-value "Highlight Active Region" :buffer buffer)
               'editor::evaluate-region-command
             'editor::evaluate-defun-command)))
     (capi:call-editor ep (list command buffer)))))

(defmethod text-edit-abort ((self om-text-editor))
  (capi::execute-with-interface self 'abort))

(defmethod find-definition ((self om-text-editor))
 (with-slots (ep) self
    (let ((buffer (editor-pane-buffer ep))
          (symbol nil))
      (editor::use-buffer buffer
        (setf symbol (editor::intern-symbol-from-string (editor::read-symbol-from-point :previous t :read-package-name t)))
        ;(print (list symbol (type-of symbol)))
        (when symbol (om-lisp::om-edit-definition symbol))
      ))))

;;; LOAD THE LISP FILE ATTACHED...
(defmethod load-lisp-file ((self om-text-editor))
  (with-slots (ep) self
    (let ((path (editor:buffer-pathname (editor-pane-buffer ep))))
      (if path
          (if (probe-file path)
              (with-safe-file-operation self path "load" "Loaded"
                (load path :print nil :verbose nil))
            (error "File ~A not found." path))
        (echo-string self (concatenate 'string "This buffer is not attached to a file."))))))


(defvar *fasl-extension* (pathname-type (cl-user::compile-file-pathname "")))

;;; LOAD ANOTHER FILE
(defmethod load-a-lisp-file ((self om-text-editor))
  (let ((filename (capi::prompt-for-file "Choose a File to Load..." 
                                         :filters (list "All files" "*.*" "Lisp File" "*.lisp" "Compiled Lisp File" 
                                                        (concatenate 'string "*." *fasl-extension*))
                                         :pathname *last-open-directory*)))
    (when filename
      (if (probe-file filename)
          (progn 
            (load filename)
            (setf *last-open-directory* (make-pathname :directory (pathname-directory filename)))
            (echo-string self (concatenate 'string "File " (namestring filename) " loaded."))
            )
        (progn 
          (beep-pane nil)
          (echo-string self (concatenate 'string "File " (namestring filename) " not found."))
          ))
      )))

;;;=============================================
;;; ADDITIONAL EDITOR FEATURES
;;;=============================================

;;;=============================================
;;; ARGLIST (from LW examples)
;;;=============================================
(in-package "CL-USER")

(defvar *arglist-delay* 1)
(defvar *arglist-timer* nil)

(defvar *setf-names-p* nil "when true, show the argument list for setf names too.")

(editor:defcommand "Insert Space and Show Arglist" (p)
     "Display the argument list in the echo area a while after inserting Space to the right of the function."
     "Display the argument list."
  (editor:self-insert-command p #\Space)
  (let* ((x (if *setf-names-p*
                (editor:with-point ((temp1 (editor:current-point))
                                    (temp2 (editor:current-point)))
                  (when (editor:form-offset temp1 -1)
                    (ignore-errors
                      (let ((*package* (editor::buffer-package-to-use temp1)))
                        (read-from-string 
                         (editor:points-to-string temp1 temp2))))))
              (editor:buffer-symbol-at-point (editor:current-buffer))))
         (window (editor:current-window))
         (function (find-function-for-arglist x)))
    (when (fboundp function)
      (show-arglist function 
                    (capi:top-level-interface 
                     (editor:window-text-pane window)) 
                    window))))

(defun show-arglist (function interface editor-window)
  (let ((lambdalist (function-lambda-list  function)))
    (setq *arglist-timer* 
          (mp:make-timer 'capi:execute-with-interface interface 
                         'editor:process-character 
                         (list 'editor:message (format nil "ARGS: ~A" lambdalist))
                         editor-window))
    (mp:schedule-timer-relative *arglist-timer* *arglist-delay*)
    ))

(defun find-function-for-arglist (x)
  (typecase x
    (symbol x)
    (list (unless (dotted-list-p x)
            (if (eq (length x) 1)
                (find-function-for-arglist (car x))
              (case (car x)
                ((quote function) (find-function-for-arglist (cdr x)))
                (setf (and (= (length x) 2)
                           (symbolp (second x))
                           x)))))))) 

(editor:bind-key "Insert Space and Show Arglist" #\Space :mode "Lisp") 
(editor:bind-key "Insert Space and Show Arglist" #\Space :mode "Execute") 
