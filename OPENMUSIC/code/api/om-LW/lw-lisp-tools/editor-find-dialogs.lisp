;;===========================================================================
;LW Lisp Tools 
;Lisp programming tools for LispWorks delivered applications
;;===========================================================================

;===========================================================================
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; See file LICENSE for further informations on licensing terms.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; Author: Jean Bresson
; Contributions and code by: Sheldon Ball, Nicholas Ellis
;;===========================================================================

;;;;;====================
;; Find and Replace dialogs from S. Ball's ANVITA EDITOR
;;;;;

(in-package :om-lisp)

(defun print-capitalized (keyword)
  (substitute #\space #\- (string-capitalize keyword)))

(define-interface anvita-dialog ()
  ((ae-interface :initform nil :initarg :ae-interface :reader anvita-dialog-interface)))

(define-interface find-dialog (anvita-dialog)
  ((replace-pane :initform nil))
  (:panes
   (find-pane text-input-choice
              :visible-min-width '(:character 30)
              :items (:initarg :find-items))
   (direction-pane radio-button-panel
                   :title "Direction"
                   :title-position :frame
                   :items '(:up :down)
                   :print-function 'print-capitalized
                   :selected-item :down)
   (action-buttons push-button-panel
                   :items (:initarg :find-replace-actions)
                   :print-function 'print-capitalized
                   :default-button :find-next
                   :cancel-button :cancel
                   :selection-callback 'find-replace-callback
                   :callback-type :interface-data
                   :layout-class 'column-layout
                   :layout-args '(:uniform-size-p t)))
  (:layouts
   (find-replace-layout grid-layout
                        '("Find what:" find-pane)
                        :columns 2
                        :y-adjust :center)
   (option-layout row-layout
                  '(nil direction-pane)
                  :y-adjust :center)
   (find-replace-option-layout column-layout
                               '(find-replace-layout option-layout))
   (main-layout row-layout
                '(find-replace-option-layout action-buttons)
                :gap 10
                :x-adjust (:initarg ((:find-replace-actions-adjust
                                      find-replace-actions-adjust)
                                     :center))))
  (:default-initargs
   :layout 'main-layout
   :auto-menus nil
   :internal-border 10
   :title "Find"
   :visible-max-width t
   :visible-max-height t))

(define-interface find-replace-dialog (find-dialog)
  ()
  (:panes
   (replace-pane text-input-choice
                 :visible-min-width '(:character 30)
                 :items (:initarg :replace-items)))
  (:layouts
   (find-replace-layout grid-layout
                        '("Find what:" find-pane "Replace with:" replace-pane)
                        :columns 2
                        :y-adjust :center))
  (:default-initargs
   :title "Replace"))

(defvar *find-strings* nil)
(defvar *replace-strings* nil)

(defun find-replace-callback (self data)
  (with-slots (find-pane replace-pane direction-pane) self
    (if (eq data :cancel)
        (destroy self)
      (let* ((find-text (text-input-pane-text find-pane))
             (direction (if (eq (choice-selected-item direction-pane) :up)
                            :backward
                          :forward))
             (replace-text (and replace-pane
                                (text-input-pane-text replace-pane))))
        (pushnew find-text *find-strings* :test 'string-equal)
        (when replace-text
          (pushnew replace-text *replace-strings* :test 'string-equal))
        (let ((interface (anvita-dialog-interface self)))
          (with-slots (ep) interface
            (call-editor ep
                         (list 'do-editor-find-replace-callback
                               find-text
                               replace-text
                               direction
                               data
                               #'(lambda ()
                                   (execute-with-interface 
                                    self
                                    #'(lambda ()
                                        (display-message-for-pane self "Cannot find ~S" find-text))))))))))))

(defun move-point-to-found (found buffer point direction)
  (let* ((mark (or (editor:buffer-mark buffer t)
                   (progn
                     (editor:set-current-mark point)
                     (editor:buffer-mark buffer)))))
    (editor:move-point mark point)
    (editor:character-offset (if (eq direction :backward)
                                 mark
                               point)
                             found)
    (editor::set-highlight-buffer-region t buffer)))

(defun do-editor-find-replace-callback (find-text replace-text direction
                                                  data limit-callback)
  (block done
    (let* ((buffer (editor:current-buffer))
           (point (editor:buffer-point buffer)))
      (when (eq data :replace-all)
        (when-let (start (looking-at-editor-string-p find-text point direction))
          (editor:move-point point start))
        (editor::query-replace-string :point point
                                      :direction direction
                                      :target find-text
                                      :replacement replace-text
                                      :do-all t)
        (return-from done))
      (when (eq data :replace)
        (when-let (start (looking-at-editor-string-p find-text point direction))
          (editor::query-replace-string :point start
                                        :direction direction
                                        :target find-text
                                        :replacement replace-text
                                        :count 1
                                        :do-all t)))
      (loop (when-let (found (find-editor-string find-text point direction))
              (move-point-to-found found buffer point direction)
              (return))
            (editor:redisplay)
            (funcall limit-callback)
            (return)))))

(defun find-editor-string (string point direction &optional limit)
  (let ((pattern (editor::get-search-pattern string direction nil)))
    (editor::find-pattern point pattern limit)))

(defun looking-at-editor-string-p (string point direction)
  (let* ((buffer (editor:point-buffer point))
         (start (editor:buffer-mark buffer t))
         (end point))
    (and start
         (editor:with-point ((seek start :temporary))
           (let ((found (find-editor-string string seek direction end)))
             (and found
                  (if (eq direction :forward)
                      (and (editor:point= seek start)
                           (progn
                             (editor:character-offset seek found)
                             (editor:point= seek end)))
                    (and (editor:point= seek end)
                         (progn
                           (editor:character-offset seek found)
                           (editor:point= seek start)))))))
         start)))

(defun find-dialog-find-next (interface)
  (find-replace-callback interface :find-next))

;;;==================
;;; ENTRY POINTS
;;;==================
(defun find-in-file (interface)
  (display-anvita-dialog interface 'find-dialog
                          :find-replace-actions '(:find-next :cancel)
                          :find-replace-actions-adjust :center
                          :find-items *find-strings*))

(defun replace-in-file (interface)
  (display-anvita-dialog interface 'find-replace-dialog
                          :find-replace-actions '(:find-next :replace :replace-all :cancel)
                          :find-replace-actions-adjust :center
                          :find-items *find-strings*
                          :replace-items *replace-strings*))

;;;==================



;; Search

(defvar *search-targets* nil)
(defvar *search-directories* nil)

(define-interface search-directory-and-target ()
  ((dir :initarg :dir))
  (:panes 
   (target text-input-pane
           :text (:initarg :search-target)
           :visible-min-width 200
           :change-callback :redisplay-interface)
   (directory text-input-pane :enabled (and dir (file-directory-p dir))
              :text (if dir (namestring dir) "")
              :visible-min-width 80)
   (browse-button push-button
                  :text "Browse..."
                  :callback 'browse-for-folder
                  :callback-type :interface))
  (:layouts 
   (main-layout
    grid-layout
    '("Search for:" target nil 
                    "Folder:" directory browse-button)
    :title "Specify text and a folder to search in"
    :title-position :frame
    :rows 2))
  (:default-initargs
   :layout 'main-layout
   :auto-menus nil
   ))

(defun get-search-directory-and-string (interface)
  (declare (ignore interface))
  (let ((result
         (popup-confirmer (make-instance 'search-directory-and-target 
                                  :search-target (if *search-targets* (first *search-targets*) "")
                                  :dir (if *search-directories* (first *search-directories*) *proprietary-dir*))
                   nil
                   :ok-button "Search"
                   :ok-check 
                   (lambda (self) 
                     (with-slots (target dir) self
                       (let ((target-string (text-input-pane-text target)))
                         (and (string/= target-string "")
                              dir
                              (file-directory-p dir)))))
                   :title "Search target and folder")))
    (when result
      (with-slots (target dir) result
        (let ((target-string (text-input-pane-text target)))
          (pushnew target-string *search-targets* :test 'string-equal)
          (pushnew dir *search-directories* :test 'pathname-match-p)
          (values target-string dir))))))

(defun browse-for-folder (self)
  (when-let (folder (prompt-for-directory "Folder to search in"))
    (with-slots (dir directory) self
      (setf dir folder
            (text-input-pane-text directory) (namestring dir)))
    (redisplay-interface self)))

(defun open-anvita-file-and-show-result (self item)
  (let ((interface (anvita-dialog-interface self)))
    (with-slots (target) self
      (execute-with-interface interface
                              #'(lambda ()
                                  (open-anvita-file interface item)
                                  (with-slots (ep) interface
                                    (call-editor ep (list 'display-target-in-editor ep target))))))))

(defun display-target-in-editor (ep target)
  (let* ((current (editor-pane-buffer ep))
         (point (editor:buffer-point current)))
    (editor:buffer-start point)
    (when-let (found (find-editor-string target point :forward))
      (move-point-to-found found current point :forward))))

(define-interface search-dialog (anvita-dialog)
  ((target :initarg :target :initform nil))
  (:panes
   (hits list-panel
              :visible-min-width '(:character 40)
              :visible-min-height 100
              :items (:initarg :hits)
              :selection nil
              :selection-callback 'open-anvita-file-and-show-result
              :action-callback 'open-anvita-file-and-show-result
              :callback-type :interface-data
              :help-key :search-file-result))
  (:layouts
   (main-layout simple-layout
                '(hits)))
  (:default-initargs
   :layout 'main-layout
   :auto-menus nil
   :internal-border 10
   :title "Search Results"))

(defun display-anvita-dialog (interface class &rest args)
  (display (apply 'make-instance class :ae-interface interface args)
           :owner interface))

(defun search-anvita-files (interface)
  (multiple-value-bind (string directory)
      (get-search-directory-and-string interface)
    (when string
      (let* ((files (directory (make-anvita-path :wild directory)))
             (hits (loop with result = nil
                         for file in files
                         when (search string (file-string file) :test #'char-equal)
                         do (push file result)
                         finally (return result))))
        (if hits
            (display-anvita-dialog interface 'search-dialog 
                                   :target string
                                   :hits hits 
                                   :title (format nil "Files containing ~A" string))
          (display-message-for-pane interface "No Anvita files containing ~A." string))))))
      
    