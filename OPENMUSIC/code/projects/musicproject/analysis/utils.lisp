;; various general methods applicable to all or most analysis classes:

(defun format-alternative-list (prep alternatives)
  (let ((format-string (concatenate 'string "~#[ nil~; ~S~; ~S ~A"
				    (format nil "~A" prep)
				    "~:;~@{~#[~; "
				    (format nil "~A" prep)
				    "~] ~S~^,~}~]")))
    (apply #'format nil format-string alternatives)))

;; (format-alternative-list "and" (loop for i from 0 below 4 collect i))
;; (format-alternative-list "or" (loop for i from 0 below 4 collect i))

(defmethod! get-segment-begins ((self chord-seq) &optional (n 0) (add-initial-zero? t))
  :indoc '("chord-seq" "nth analysis")
  :doc "returns a list of offsets (ms.) for segments in analysis 'n'"
  (let* ((all-analyses (analysis self))
	 (n-analyses (length all-analyses)))
    (if (>= n n-analyses)
	(om-beep-msg (concatenate 'string (format nil "Error: ~A analyses found in ~A, n must be" n-analyses self)
				  (format-alternative-list "or" (loop for i from 0 below n-analyses collect i))))
	(let ((offsets (mapcar #'tb (analysis-segments (nth n all-analyses)))))
	  (if (and (> (car offsets) 0) add-initial-zero?)
	      (cons 0 offsets)
	      offsets)))))
