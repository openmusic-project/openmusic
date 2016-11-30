(in-package :om)

;;;=========================
;;; dummy class for SDIF lib initialization
(defclass sdif-object () ())

(defmethod initialize-instance :after ((self sdif-object) &rest initargs)
  (sdif::sdif-init-cond))



;;;=========================
;;; SDIF MATRIX
;;;=========================

(defclass! SDIFMatrix (class-array) 
   ((signature :initform nil :accessor signature :documentation "SDIF type signature"))
   (:icon 644)
   (:documentation "SDIF data stored as a 2D array.

SDIF Matrix define multidimensional sound description data at a given moment (no temporal dimension).
The lines of the matrix define the different fields of the description data (e.g. frequencies, amplitudes, etc.)
The number of fields usually depend on the SDIF type specification corresponding to <signature>.
The number of elements is variable.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF

SDIFMatrix is a subclass of CLASS-ARRAY.

The fields (lines) of the matric are added using the \"add keyword inputs\" command ('k').
Each line appears as a standard \"keyword\" input. It should be given a name (click on the input, always begin with ':', e.g. :param1) and a value (connect something).
Lines can be removed with \"remove keyword input\" command ('K').

Instances are created by specifying a number of columns or components (input 1 = <numcols>) and specifying values for the different fields.

 "))


(defmethod get-slot-in-out-names ((self SDIFMatrix))
   (values '("self" "numcols" "signature") '(nil 1 "XXXX")
           '("object" "number of components" "SDIF type signature")
           '(nil nil nil)))

(defmethod fixed-slots-list ((self SDIFMatrix)) '(numcols signature))


(defmethod omNG-copy ((self SDIFMatrix)) 
  `(let ((copy ,(call-next-method)))
     (setf (signature copy) ',(signature self))
     copy))


;;; SDIFMatrix = comme une class array
;;; les fields = les controls du array

(defclass SDIFMatrixBox (ArrayBox) ())
(defmethod get-type-of-ed-box ((self SDIFMatrix)) 'SDIFMatrixBox)



;;; OPTIMIZED FILE WRITING

(defclass! raw-sdifmatrix ()
  ((signature :initform "" :initarg :signature  :accessor signature :documentation "SDIF type signature")
   (num-elts :initform 1 :initarg :num-elts  :accessor num-elts :documentation "number of elements")
   (num-fields :initform 1 :initarg :num-fields  :accessor num-fields :documentation "number of fields")
   (data :initform nil :initarg :data :accessor data :documentation "data list"))
  (:icon 647)
  (:documentation "2D SDIF data optimized for the SDIF library read/write operations.

SDIF Matrix define multidimensional sound description data at a given moment (no temporal dimension).
The lines of the matrix define the different fields of the description data (e.g. frequencies, amplitudes, etc.)
The number of fields usually depend on the SDIF type specification corresponding to <signature>.
The number of elements is variable.

RAW-SDIFMatrix is a virtual 2D array of <num-fields> x <num-elts>.
All data is contained in <data> as a flat list, containing the successive field values for eacch element (e.g. '(freq1 amp1 phase1 frezq2 amp2 phase2 ...)).
This data can be written into an SDIF file in a single operation, which may consequently speed the management and writing of large data sets.


See http://sdif.sourceforge.net/ for more inforamtion about SDIF.
"
))

;;; compat
(defclass! soft-sdifmatrix (raw-sdifmatrix) ())


;;;=========================
;;; SDIF FRAME
;;;=========================
;;; Ensemble de matrices correspondant a un instant d'echantillonnage 
;;; Unite minimum pour ecrire dans un fichier SDIF
(defclass! sdifframe (sdif-object)
   ((signature :initform nil :initarg :signature :accessor signature :documentation "SDIF type signature")
    (FTime :initform 0.0 :initarg :FTime :accessor FTime :documentation "time (s)")
    (StreamId :initform 0 :initarg :StreamId :accessor StreamId :documentation "integer")
    (LMatrix :initform nil :initarg :LMatrix :accessor LMatrix :documentation "list of SDIFMAtrix or raw-SDIFMatrix instances"))
   (:icon 643)
   (:documentation "An SDIF data chunk.

SDIF frames are data chunk containing one or more SDIF matrices (<lMatrices>) and precisely localized in time (<fTime>).
Frames can be grouped in streams identified by an ID (<streamID>) in order to describe parallele data.  
The number and type of matrices allowed in a given frame depends on the SDIF frame type specification corresponding to <signature>.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.
"
))

(defmethod sdifframep ((self t)) nil)
(defmethod sdifframep ((self sdifframe)) t)

(defmethod initialize-instance :after ((self sdifframe) &rest initargs)
   (setf (LMatrix self) (list! (LMatrix self))))






;;;=========================
;;; SDIF STREAM
;;;=========================
;;; Ensemble de Frames constituant un flux de données SDIF 
(defclass! sdifstream ()
   ((Id :initform 0 :initarg :Id :accessor Id :documentation "integer")
    (LFrames :initform nil :initarg :LFrames :accessor LFrames :documentation "List of SDIFFrames"))
   (:icon 641)
   (:documentation "A set or temporally ordered SDIF frames pertaining to a same description.


SDIF are grouped in streams identified by an ID (<streamID>) in order to describe parallele data.  
Streams are implicitely defined by the frame IDs, so the SDIFStream class is more designed for convenience purposes as it allows to gather all frames in same object.
It can actually be bypassed when building SDIF structures (SDIFFrames can be written or stored directly in SDIF buffers or files).

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.
"
    ))

(defmethod sdifstreamp ((self t)) nil)
(defmethod sdifstreamp ((self sdifstream)) t)

(defmethod initialize-instance :after ((self sdifstream) &rest initargs)   
   (setf (LFrames self) (list! (LFrames self)))
   (when (LFrames self)
       (setf (LFrames self) (sort (remove nil (LFrames self)) '< :key 'FTime))
       (let ((err nil))
         (loop for fr in (LFrames self) do
                 (if (sdifframep fr)
                     (when (not (equal (StreamId fr) (ID self)))
                         (setf (StreamId fr) (ID self))
                         (setf err t))
                   ))
         (when err (om-message-dialog (format nil "Warning :~%Some Frames IDs have been modified to fit the Stream ID ~D" (ID self)))))
       t))

(defmethod objfromobjs ((self list) (type sdifstream))
   (let ((id nil) (flist nil))
     (loop for item in self do 
             (when (sdifframep item)
                 (unless id 
                    (setf id (StreamID item)))
                 (push item flist)))
     (make-instance 'sdifstream :ID id :LFrames flist)))

(defmethod nb-frames ((self sdifstream))
   (length (LFrames self)))



;;;=========================
;;; SDIF TYPE
;;;=========================
(defclass! SDIFType ()
   ((Struct :initform 'F :initarg :Struct :accessor struct :documentation "frame (f) or matrix (m)")
    (Signature :initform "" :initarg :Signature :accessor signature :documentation "SDIF type signature")
    (Description :initform nil :initarg :description :accessor description :documentation "type description"))
   (:icon 646)
   (:documentation "An SDIF type declaration to be written in an SDIF file or buffer.

SDIF types define data structures in the SDIF frameworks.
They are identified by a 4 ASCII characters (e.g. 1TRC, 1FQ0, ETC..)

See http://sdif.sourceforge.net/ for more inforamtion about SDIF TYPES.

Matrix type description must be a list with the names of the different description fields in this matrix (e.g. '(\"freq\" \"amp\" \"phase\"))
Frame type description muste be a list with the types and names of the different matrices allowed in the frame (e.g. '((\"TYP1\" \"matrix1\") (\"TYP2\" \"matrix2\"))

"
    ))

(defmethod sdiftypep ((self t)) nil)
(defmethod sdiftypep ((self sdiftype)) t)


(defmethod get-slot-in-out-names ((self SDIFType))
   (values '("self" "struct" "signature" "description")
           '(nil 'F "" "")
           '("object" "Frame/Matrix" "signature (4 chars)" "description du type")
           '(nil ((1 (("Frame" 'F)  ("Matrix" 'M)))) nil nil)))


;;;=========================
;;; SDIF NVT
;;;=========================
(defclass! SDIFNVT ()
   ((NV-pairs :initform nil :initarg :NV-pairs :accessor NV-pairs :documentation "list of (name value) pairs")
    (TableName :initform nil :initarg :TableName :accessor TableName :documentation "name (string)")
    (ID :initform 0 :initarg :ID :accessor ID :documentation "table ID (integer)")
    (tnum :initform 0 :accessor tnum))
   (:icon 648)
   (:documentation "An SDIF Name/Value table to be written in an SDIF file or buffer.

SDIF NVTs gibe additional info on the data in an SDIF file in the form of name/value pairs
They have an ID and ususally a TableName name/value.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF TYPES.

"
    ))

(defun default-om-NVT ()
  (make-instance 'SDIFNVT 
                 :tablename "FileInfo"
                 :ID 0
                 :NV-pairs (list 
                            (list "Author" (string+ "OM " *version-string*))
                            (list "Date" (om-get-date)))))


;;;=========================
;;; SDIF SID
;;;=========================
(defclass! SDIFSID ()
   ((ID :initform 0 :initarg :ID :accessor ID :documentation "stream ID")
    (Source :initform "" :initarg :Source :accessor Source :documentation "stream source (or destination)")
    (TreeWay :initform nil :initarg :TreeWay :accessor TreeWay :documentation "stream objects and info"))
   (:icon 637)
   (:documentation "SDIF SID (or stream ID) chuncks are usually written in the file header to inform about the streams contents and relationships.

Well-formed SDIF files should have unique IDs for the different streams.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF SID and format.

"))



;;;=========================
;;; SDIF Buffer
;;;=========================
;;; Ensemble de frames et/ou streams
;;; + infos permettant de reconstruire un fichier SDIF complet

(defclass! SDIF-buffer ()
   ((Types :initform nil :initarg :Types :accessor Types :documentation "list of SDIFType")
    (NVTs :initform nil :initarg :NVTs :accessor NVTs  :documentation "list of SDIFNVT or SDIFIDS")
    (SIDs :initform nil :accessor SIDs  :documentation "list of SDIFSID")  ;; 'hidden' slot => set it using NVTs
    (LFrames :initform nil :initarg :LFrames :accessor LFrames  :documentation "list of SDIFStream or SDIFFrame"))
   (:icon 645)
   (:documentation "Representation of a complete SDIF file description, including Frames, type declarations and NVTs.

Use SAVE-SDIF-FILE to store the contents of the SDIF-Buffer in an SDIF file.

See http://sdif.sourceforge.net/ for more inforamtion about SDIF.

"
))


(defmethod sort-nvts ((self sdif-buffer))
  (let ((sorted-tables
         (loop for item in (list! (nvts self)) do
               when (subtypep (type-of item) 'sdifnvt)
               collect item into nvts
               when (subtypep (type-of item) 'sdifsid)
               collect item into sids
               finally return (list nvts sids))))
    (setf (nvts self) (car sorted-tables)
          (sids self) (cadr sorted-tables))
    ))

(defmethod initialize-instance :after ((self SDIF-buffer) &rest initargs)
   (setf (LFrames self) (list! (LFrames self)))
   (setf (LFrames self) (sort 
                         (remove nil (flat (loop for item in (flat (LFrames self)) collect (cond ((sdifframep item) item)
                                                                                                 ((sdifstreamp item) (LFrames item))
                                                                                                 (t nil)))))
                         '< :key 'FTime))
   (setf (Types self) (remove nil (list! (Types self))))
   
   (sort-nvts self)
   
   )

(defmethod omNG-save ((self SDIF-buffer) &optional (values? nil))
  `(make-instance 'SDIF-buffer))
  


