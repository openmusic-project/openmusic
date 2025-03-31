(in-package :om)

;*saved-pref*
;*current-pref*

;*global-midi-approx*

(defun omicron-pref (object button)
  (let* ((pos (om-make-point 200 200))
         (approx *global-midi-approx*);(staff-tone (panel (om-view-container controls))))
         (index (find-indx approx))
         (indx1 (car index))
         (indx2 (second index))
         (win (om-make-window 'omicron-dialog 
                              :size (om-make-point 740 400)
                              :position pos
                              :window-title "Tuning and notation"
                              :resizable t
                              :external-min-width 740
                              :external-min-height 400 
                              :bg-color *controls-color*
                              :menu-items (list (om-make-menu
                                                 "Window"
                                                 (list (om-new-leafmenu
                                                        "Close all inspector windows"
                                                        #'(lambda () (mapc 'om-close-window (om-get-all-windows 'omicron-dialog)))
                                                        "w"
                                                        ))))
                              )))
    (setf (title1 win) (om-make-dialog-item 'om-static-text (om-make-point 80 8) (om-make-point 150 50) "Tuning system"))
    (setf (list1 win) (om-make-dialog-item 'om-single-item-list
                                           (om-make-point 10 30)
                                           (om-make-point 230 280)
                                           ""
                                           :focus nil
                                           :resizable t
                                           :scrollbars :v
                                           :callback-type '(:collection)
                                           :test-function 'string-equal
                                           :range nil
                                           :selection-callback #'(lambda (list) 
                                                                   (progn 
                                                                     (setf (selected win) (mapcar 'car *omicron-data*))
                                                                     (setf indx1 (capi::choice-selection list))
                                                                     (setf (index1 win) (capi::choice-selection list))
                                                                     ;(print (list "selection" (capi::choice-selection list) "indx:" indx1))
                                                                     (set-omicron-panel (list2 win) 
                                                                                        (second (nth (capi::choice-selection list) *omicron-data*)))
                                                                     (setf indx2 0);init choix 2
                                                                     (om-set-dialog-item-text (list3 win) 
                                                                                              (car (third (nth (capi::choice-selection list)  *omicron-data*))))
                                                                     )
                                                                   )))
    


    (setf (title2 win) (om-make-dialog-item 'om-static-text (om-make-point 300 8) (om-make-point 150 50) "Notation system"))
    (setf (list2 win) (om-make-dialog-item 'om-single-item-list
                                           (om-make-point 250 30)
                                           (om-make-point 230 280)
                                           ""
                                           :focus nil
                                           :resizable t
                                           :scrollbars :v
                                           :callback-type '(:collection)
                                           :test-function 'string-equal
                                           :range nil
                                           :selection-callback #'(lambda (list) 
                                                                  ; (print (list "second"  (capi::choice-selection list)))
                                                                   (setf indx2 (capi::choice-selection list))
                                                                   (setf (index2 win) (capi::choice-selection list))
                                                                   (om-set-dialog-item-text 
                                                                    (list3 win) 
                                                                    (nth 
                                                                     (capi::choice-selection list)
                                                                     (third (nth indx1  *omicron-data*)))))))
    
    (setf (title3 win) (om-make-dialog-item 'om-static-text (om-make-point 580 8) (om-make-point 80 50) "Description")) 
    (setf (list3 win) (om-make-dialog-item 'om-static-text
                                           (om-make-point 490 30)
                                           (om-make-point 230 280)
                                           ""
                                           :bg-color *om-white-color*
                                           ))
    (setf (but1 win) (om-make-dialog-item 'om-button (om-make-point 20 330) (om-make-point 160 20) 
                                          "Restore default"
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item)) 
                                                       (om-set-selected-item-index (list1 win) 1)
                                                       (om-set-selected-item-index (list2 win) 0)
                                                       (setf *global-midi-approx* 2)
                                                       (om-close-window win)
                                                       )
                                          ))
    (setf (but2 win) (om-make-dialog-item 'om-button (om-make-point 560 330) (om-make-point 80 20) "Cancel"
                                           :di-action (om-dialog-item-act item
                                                       (declare (ignore item)) 
                                                        (om-close-window win))
                                                       ))
    (setf (but3 win) (om-make-dialog-item 'om-button (om-make-point 650 330) (om-make-point 80 20) "Apply"
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item)) 
                                                       (progn
                                                       (om-set-dialog-item-text button 
                                                                                (give-symbol-of-approx (car (nth indx2 (nth indx1 *omicron-scales-list*)))))
                                                       (setf *global-midi-approx* (car (nth indx2 (nth indx1 *omicron-scales-list*))))
                                                       (om-close-window win)
                                                       ))
                                          ))
   
    (set-omicron-panel (list1 win) (mapcar 'car object))
    (om-set-selected-item-index (list1 win) indx1)
    (set-omicron-panel (list2 win) (nth indx1 (mapcar 'second object)))
    (om-set-selected-item-index (list2 win) indx2)
    (om-set-dialog-item-text (list3 win) (nth indx2 (nth indx1 (mapcar 'third object))))
    (om-add-subviews win (title1 win) (list1 win) (list2 win) (list3 win) (title1 win) (title2 win) (title3 win) (but1 win) (but2 win) (but3 win))
    #+cocoa(setf (capi::interface-menu-bar-items win)
                 (internal-window-class-menubar win))
    (om-select-window win)
    ))

