(in-package :alloy-desktop)

(defvar *last-directory* nil
  "Directory remembered by the file choosers between calls.")

(defun directory-to-remember (result kind)
  "The directory to store in *LAST-DIRECTORY* after a chooser returned RESULT.
KIND :FILE keeps only the directory components of RESULT (host and device are
dropped); :DIRECTORY returns RESULT itself; :NEW-DIRECTORY takes the
directory-shaped RESULT, keeps its host and device and removes its last
directory component, which is the directory just named."
  (ecase kind
    (:file (make-pathname :directory (pathname-directory result)))
    (:directory result)
    (:new-directory
     (make-pathname :host (pathname-host result) :device (pathname-device result)
                    :directory (butlast (pathname-directory result))
                    :name nil :type nil))))

(defun run-file-dialog (operation options
                        &key backend (service (make-instance 'file-dialog-service)))
  "Run one file dialog and return what it answers, NIL when it is cancelled.
OPERATION is :OPEN or :SAVE. OPTIONS is the normalized property list (TITLE
DEFAULT FILTER OWNER SELECTED-FILTER IF-EXISTS IF-DOES-NOT-EXIST
FILE-PACKAGE-IS-DIRECTORY PANE-ARGS POPUP-ARGS). When BACKEND is non-NIL it is
called as (apply BACKEND OPERATION OPTIONS) and its value is returned as it
comes. Otherwise TITLE, DEFAULT and FILTER go to CHOOSE-EXISTING-FILE (:OPEN) or
CHOOSE-NEW-FILE (:SAVE) on SERVICE; on macOS those methods enter the panel on the
UI thread, because a panel opened from another thread takes over the event loop
and never gives it back."
  (if backend
      (apply backend operation options)
      (ecase operation
        (:open (choose-existing-file service
                                     :title (getf options :title)
                                     :default (getf options :default)
                                     :filter (getf options :filter)))
        (:save (choose-new-file service
                                :title (getf options :title)
                                :default (getf options :default)
                                :filter (getf options :filter))))))
