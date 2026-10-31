(in-package :oa)

(export '(om-inspect) :om-api)

(defclass inspect-dialog (om-window)
  ((list1    :initarg :list1    :initform nil :accessor list1)
   (list2    :initarg :list2    :initform nil :accessor list2)
   (selected :initarg :selected :initform nil :accessor selected)))

(defun %unsigned-byte-type-p (obj)
  (let ((type (type-of obj)))
    (and (consp type)
         (eq (first type) 'unsigned-byte)
         (member (second type) '(2 4 8 16 32)))))

(defun decompose-object (obj)
  (cond
    ((consp obj)
     (if (listp (cdr obj))
         (loop for item in obj
               for i from 0
               collect (list i item))
         (list (list "car" (car obj)) (list "cdr" (cdr obj)))))
    ((or (arrayp obj) (%unsigned-byte-type-p obj))
     (list obj))
    ((sb-mop:class-slots (class-of obj))
     (loop with class = (class-of obj)
           for slot in (sb-mop:class-slots class)
           for name = (sb-mop:slot-definition-name slot)
           collect (list name (if (slot-boundp obj name)
                                  (slot-value obj name)
                                  :unbound))))
    ((atom obj) (list obj))
    (t nil)))

(defun %inspector-item-string (elt)
  (if (consp elt)
      (concatenate 'string
                   (string-upcase (format nil "~A" (car elt)))
                   (format nil ":   ~A" (cadr elt)))
      (format nil "~A" elt)))

(defun set-inspector-panel (item-list object)
  (om-set-item-list item-list
                    (mapcar #'%inspector-item-string (decompose-object object))))

(defmethod om-resize-callback ((self inspect-dialog) x y w h)
  (declare (ignore x y))
  (call-next-method)
  (let* ((menu (alld:window-menubar self))
         (mbh  (if menu (alloy:pxh (alloy:bounds (alloy:layout-element menu))) 0))
         (lh   (max 0 (round (- h mbh 20))))
         (diff (abs (- w 500)))
         (lw   (+ (round (/ diff 2)) 230)))
    (when (list1 self)
      (om-set-view-size (list1 self) (om-make-point lw lh))
      (om-set-view-position (list1 self) (om-make-point 10 10)))
    (when (list2 self)
      (om-set-view-size (list2 self) (om-make-point lw lh))
      (om-set-view-position (list2 self)
                            (om-make-point (abs (- (- w 250) (round (/ diff 2)))) 10)))))

(defmethod close-all-inspectors ((self inspect-dialog))
  (mapc #'om-close-window (om-get-all-windows 'inspect-dialog)))

(defmethod om-window-class-menubar ((self inspect-dialog))
  (list (om-make-menu
         "Windows"
         (list (om-new-leafmenu "Close all inspector windows"
                                (lambda () (close-all-inspectors self))
                                #\w)))))

(defun om-inspect (object &optional position)
  (let* ((pos (or position (om-make-point 200 200)))
         (win (om-make-window 'inspect-dialog
                              :size (om-make-point 500 300)
                              :min-size (om-make-point 500 300)
                              :position pos
                              :window-title (format nil "Inspecting ~A" object)
                              :resizable t)))
    (setf (list1 win)
          (om-make-dialog-item 'om-single-item-list
                               (om-make-point 10 10)
                               (om-make-point 230 260)
                               ""
                               :focus nil
                               :scrollbars :v
                               :range nil
                               :di-action
                               (lambda (item)
                                 (let ((idx (om-get-selected-item-index item)))
                                   (when idx
                                     (setf (selected win)
                                           (cadr (nth idx (decompose-object object))))
                                     (set-inspector-panel (list2 win) (selected win)))))
                               :after-action
                               (lambda (item)
                                 (let* ((idx (om-get-selected-item-index item))
                                        (sel (and idx (cadr (nth idx (decompose-object object))))))
                                   (when (and sel (consp (car (decompose-object sel))))
                                     (om-inspect sel
                                                 (om-add-points (om-view-position win)
                                                                (om-make-point 20 20))))))))
    (setf (list2 win)
          (om-make-dialog-item 'om-single-item-list
                               (om-make-point 250 10)
                               (om-make-point 230 260)
                               ""
                               :focus nil
                               :scrollbars :v
                               :range nil
                               :after-action
                               (lambda (item)
                                 (let* ((idx (om-get-selected-item-index item))
                                        (sub (and idx (decompose-object (selected win))))
                                        (entry (and idx sub (nth idx sub))))
                                   (when (consp entry)
                                     (let ((new (cadr entry)))
                                       (when (consp (car (decompose-object new)))
                                         (om-inspect new
                                                     (om-add-points (om-view-position win)
                                                                    (om-make-point 20 20))))))))))
    (set-inspector-panel (list1 win) object)
    (om-set-selected-item-index (list1 win) 0)
    (setf (selected win) (cadr (car (decompose-object object))))
    (set-inspector-panel (list2 win) (selected win))
    #+darwin
    (setf (capi::interface-menu-bar-items win)
          (internal-window-class-menubar win))
    #+win32
    (setf (capi::interface-menu-bar-items win) nil)
    #+linux (om-add-menu-to-win win)
    (om-add-subviews win (list1 win) (list2 win))
    (om-resize-callback win 0 0 500 300)
    (om-select-window win)
    win))
