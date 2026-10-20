(in-package :alloy-desktop)

(defun get-user-name ()
  "Return the current desktop user's login name."
  (or (uiop:getenv "USER")
      (uiop:getenv "USERNAME")
      "user"))

(defun user-home ()
  "Return the current user's home directory pathname."
  (user-homedir-pathname))

(defparameter *month-abbreviations*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "English month abbreviations used by DATE-STRING when EXPAND-MONTH is true.")

(defun date-string (&optional universal-time expand-month)
  "Return a LispWorks HCL:DATE-STRING-compatible date/time string.

UNIVERSAL-TIME is NIL (the default) or a universal-time integer.  NIL uses
GET-UNIVERSAL-TIME.  When EXPAND-MONTH is true the date is written as
DD MMM YYYY; otherwise it is written as YYYY/MM/DD.  The time is always
HH:MM:SS, in the current time zone."
  (multiple-value-bind (sec min hour day month year day-of-week dst timezone)
      (decode-universal-time (or universal-time (get-universal-time)))
    (declare (ignore day-of-week dst timezone))
    (if expand-month
        (format nil "~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D"
                day (svref *month-abbreviations* (1- month)) year hour min sec)
        (format nil "~4,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                year month day hour min sec))))
