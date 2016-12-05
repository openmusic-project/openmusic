;;===========================================================================
;;Load in recent CFFI 
;;
;;Time-stamp: <2013-06-04 14:11:06 andersvi>
;;
;;This program is free software; you can redistribute it and/or modify
;;it under the terms of the GNU Lesser General Public License as published by
;;the Free Software Foundation; either version 2.1 of the License, or
;;(at your option) any later version.
;;  
;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU Lesser General Public License for more details.
;;  
;;You should have received a copy of the GNU Lesser General Public License
;;along with this program; if not, write to the Free Software 
;;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;Author: Anders Vinjar
;;===========================================================================

(require 'asdf)

(mapc #'(lambda (system)
	  (let ((dir (if (consp system) (car system) system))
		(asd-file (if (consp system) (cdr system) system)))
	    (load (make-pathname :directory (append (pathname-directory *load-pathname*) (list dir))
				 :name asd-file
				 :type "asd")
		  :package :asdf)))
      
      '("alexandria"			;dependencies for newer cffi
	"babel"
	"trivial-features"
	("CFFI" . "cffi")
	("CFFI" . "cffi-grovel")
	("CFFI" . "cffi-libffi")
	))

(pushnew :cffi *features*)
(provide :cffi)
(asdf:load-system :cffi)





