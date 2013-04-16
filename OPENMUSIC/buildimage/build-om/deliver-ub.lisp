

;(cd "/Users/bresson/OPENMUSIC/image/macos-i")
;(push :deliver-ppc *features*)

(defvar *om-root-dir* (make-pathname :directory (butlast (pathname-directory (current-pathname)) 2)))

(defvar *image-pathname* (find "OM" ;(directory (make-pathname :directory (append (pathname-directory *om-root-dir*) (list "image" "macos-i"))))
                               (directory *om-root-dir*)
                               :test 'string-equal 
                               :key #'(lambda (file) 
                                        (let ((name (car (last (pathname-directory file)))))
                                          (if (and 
                                               (system::directory-pathname-p file)
                                               (stringp name) (> (length name) 2))
                                              (subseq name 0 2)
                                            "")))))

(defvar *om-vers* (subseq (car (last (pathname-directory *image-pathname*))) 
                          3 (- (length (car (last (pathname-directory *image-pathname*)))) 4)))


(defvar *image-name* (concatenate 'string "OM " *om-vers*))

(loop for item in (directory (make-pathname :directory (pathname-directory (current-pathname))))
      when (and (stringp (pathname-name item))
                (> (length (pathname-name item)) 3)
                (string-equal (subseq (pathname-name item) 0 3) "OM "))
      do (delete-file item))

(save-universal-from-script *image-name* "deliver.lisp")

(quit)