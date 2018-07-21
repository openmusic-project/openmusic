
;rewrite of functions normal-form and prime-form
;and update pcs-prime-form-check
;because of troubles with (0 5 10) and (0 5 7)

;source:
;christoph wuensch "satztechniken des 20. jahrhunderts"
;http://www.mta.ca/pc-set/pc-set_new/

;; Wolfgang Suppan Feb.2017

(in-package :om)


;;=========================================================================
;;     1. normal order
;;=========================================================================

(defun ws-pcs-normal-order-h0 (set)
"Arrange the pc integers in ascending (clockwise) orders"
(permute-circular (sort-ascending set)))
 
;(ws-pcs-normal-order-h0 '(8 3 7 9 0 4))
;(ws-pcs-normal-order-h0 '(2 4 8 10))

(defun ws-pcs-normal-order-h1 (liste)
"auto span test and reduce the list"
(do ((todo liste)(posx 1)(len (length (first liste))))
    ((or (= (length todo) 1)(> (+ posx 1) len)) (first todo))
  (setf todo (ws-pcs-normal-order-h2 todo posx))
  (incf posx 1)))

;(ws-pcs-normal-order-h1 '((0 3 4 7 8 9) (3 4 7 8 9 0) (7 8 9 0 3 4)))

(defun ws-pcs-normal-order-h2 (liste pos)
"filter candidates by span diff"
(let (diffxsetlist)
  (loop for item in liste 
        for resx = (mod (- (nth pos item)(nth 0 item)) 12)
        minimize resx into min-diff
        do (push (list resx item) diffxsetlist)
        finally (return (loop for item in diffxsetlist when (= (first item) min-diff) collect (second item))))))

;(ws-pcs-normal-order-h2 '((0 3 4 7 8 9) (3 4 7 8 9 0) (4 7 8 9 0 3) (7 8 9 0 3 4) (8 9 0 3 4 7) (9 0 3 4 7 8)) 5)

(defun ws-pcs-normal-order (set)
"span test..."
(let ((res (ws-pcs-normal-order-h2 (ws-pcs-normal-order-h0 set) (- (length set) 1))))
  (if (= (length res) 1)(first res)
    (ws-pcs-normal-order-h1 res))))

;(ws-pcs-normal-order '(8 3 7 9 0 4))
;(ws-pcs-normal-order '(1 4 7 8 10))
;(ws-pcs-normal-order '(2 4 8 10))
;(ws-pcs-normal-order '(4 7 8 11))

;;=========================================================================
;;     2. prime form
;;=========================================================================
(defun ws-pcs-prime-form1 (set)
  (let ((res (ws-pcs-normal-order set)))
    (mapcar #'(lambda (x) (mod (- x (first res)) 12)) res)))

;(ws-pcs-prime-form1 '(4 7 8 11))

(defun ws-pcs-prime-form2 (set)
  (ws-pcs-prime-form1 (pcs-invert-f1 set)))

;(ws-pcs-prime-form2 '(4 7 8 11))


(defun ws-pcs-prime-form-check (set)
  (let ((res (ws-pcs-prime-form1 set)))
    (if (pcs-prime-form-to-fn res) res (ws-pcs-prime-form2 set))))


;(ws-pcs-prime-form-check '(0 5 10))
;(pcs-prime-form-check '(0 5 10))

;(ws-pcs-prime-form-check '(10 0 5))
;(pcs-prime-form-check '(10 0 5))

;(ws-pcs-prime-form-check '(0 5 7))
;(pcs-prime-form-check '(0 5 7))

;(ws-pcs-prime-form-check '(2 3 4 7 8))
;(pcs-prime-form-check '(2 3 4 7 8))

;(ws-pcs-prime-form-check '(4 7 8 11))
;(pcs-prime-form-check '(4 7 8 11))

;;=========================================================================
;;     3. update of the OM function 
;;=========================================================================

(defun PCS-PRIME-FORM3 (type set)
  (let ((pform (ws-pcs-prime-form-check set)))
    (cond
     ((equal type :integer) pform)
     ((equal type :pitch) (integer-to-pitch pform))
     ((equal type :fn) (pcs-prime-form-to-fn pform))
     ((equal type :vector) (pcs :vector (pcs-prime-form-to-fn pform))))
    ))

;(p-form :integer '(8 10 11 1 2 5))
;(p-form :fn '(8 10 11 1 2 5))
;(p-form :vector '(8 10 11 1 2 5))


;(p-form :integer '(0 5 10))
;(p-form :integer '(0 5 7))
;(p-form :fn '(0 5 10)) ;->nil
;(p-form :fn '(0 5 7)) ;->nil


#|
;for testing:
(let ((liste (all-combinations '(0 1 2 3 4 5 6 7 8 9 10 11) 4)))
(with-output-to-string (*trace-output*)
  (time 
  (loop for item in liste for resx = (remove-duplicates (mk-mod item 12))   
              when (> (length resx) 1) do (ws-pcs-prime-form-check resx)))))

(let ((liste (all-combinations '(0 1 2 3 4 5 6 7 8 9 10 11) 4)))
(with-output-to-string (*trace-output*)
  (time 
  (loop for item in liste for resx = (remove-duplicates (mk-mod item 12))   
              when (> (length resx) 1) do (pcs-prime-form-check resx)))))

|#