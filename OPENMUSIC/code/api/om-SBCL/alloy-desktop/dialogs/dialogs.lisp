(in-package :alloy-desktop)

(defclass dialog-service (desktop-service) ())
(defclass file-dialog-service (dialog-service) ())
(defclass color-dialog-service (dialog-service) ())
(defclass font-dialog-service (dialog-service) ())

(defgeneric choose-existing-file (service &key title default filter))
(defgeneric choose-new-file (service &key title default filter))
(defgeneric choose-directory (service &key title default))
(defgeneric choose-new-directory (service &key title default))
(defgeneric choose-color (service &key title default full-open prevent-full-open))
(defgeneric choose-font (service &key default variant))

(defmethod choose-existing-file ((service file-dialog-service) &key title default filter)
  (declare (ignore service))
  #+darwin
  (call-on-ui-thread
   (lambda ()
     (org.shirakumo.file-select:existing
      :title title :default default :filter filter)))
  #-darwin
  (org.shirakumo.file-select:existing
   :title title :default default :filter filter))

(defmethod choose-new-file ((service file-dialog-service) &key title default filter)
  (declare (ignore service))
  #+darwin
  (call-on-ui-thread
   (lambda ()
     (org.shirakumo.file-select:new
      :title title :default default :filter filter)))
  #-darwin
  (org.shirakumo.file-select:new
   :title title :default default :filter filter))

(defmethod choose-directory ((service file-dialog-service) &key title default)
  (declare (ignore service))
  #+darwin
  (call-on-ui-thread
   (lambda ()
     (org.shirakumo.file-select:existing
      :title title :default default :filter :directory)))
  #-darwin
  (org.shirakumo.file-select:existing
   :title title :default default :filter :directory))

(defmethod choose-new-directory ((service file-dialog-service) &key title default)
  (declare (ignore service))
  #+darwin
  (call-on-ui-thread
   (lambda ()
     (org.shirakumo.file-select:new
      :title title :default default)))
  #-darwin
  (org.shirakumo.file-select:new
   :title title :default default))

(defmethod choose-color ((service color-dialog-service)
                         &key title default full-open prevent-full-open)
  (declare (ignore service))
  (color-select:choose-color :title title
                             :default default
                             :full-open full-open
                             :prevent-full-open prevent-full-open))

(defmethod choose-font ((service font-dialog-service) &key default variant)
  (declare (ignore service))
  (font-select:select :default default :variant variant))
