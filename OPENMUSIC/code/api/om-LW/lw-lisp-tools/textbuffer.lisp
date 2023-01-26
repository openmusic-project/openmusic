;;===========================================================================
;LW Lisp Tools 
;Lisp programming tools for LispWorks delivered applications
;;===========================================================================

;===========================================================================
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; See file LICENSE for further informations on licensing terms.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; Author: Jean Bresson
;;===========================================================================


;;; TEXT BUFFER UTILS

(in-package :om-lisp)

;;;================= 
;;; TEXT BUFFER ====
;;;=================

(export '(
          om-make-buffer
          om-copy-buffer
          om-buffer-delete
          om-kill-buffer
          om-buffer-insert
          om-buffer-insert-newline
          om-buffer-text
          om-buffer-char
          om-buffer-size
          om-buffer-substring
          om-lines-in-buffer
          om-buffer-lines
          om-buffer-line-start   
          om-buffer-line-end
          om-buffer-skip-forward
          om-buffer-insert-file
          om-buffer-write-file
          om-buffer-eval
          om-read-string-from-file
          ) :om-lisp)


;;; buffer : un editor::buffer
;;; window : un editeur sur laquelle le buffer est utilisé (ne pas effacer)
;;; killed? (plus utilise) un flag pour voir si le buffer est deja detruit (t)
;;;         ou en attente si il est encore sur un editeur (:flagged)
(defclass ombuffer () 
  ((buffer :accessor buffer :initarg :buffer :initform nil)
   (window :accessor window :initform nil)
   (killed? :accessor killed? :initform nil)))

(defun om-make-buffer ()
   ; (print (list "create buffer" (buffer b)))
  (make-instance 'ombuffer :buffer (editor::make-buffer (string (gensym)) :flag :om)))
   
; PLUS UTILISE...
; En declarant les objets :
;    (hcl::flag-special-free-action b)
; ...
; on peut leur appliquer une action au moment du GC
;
;(defun kill-buffers (object)
;  (when (equal (type-of object) 'ombuffer)
;    (print (list "buffer garbage" (buffer object)))
;    (editor::delete-buffer (buffer object))
;    ))
;
; (hcl::add-special-free-action 'kill-buffers)

(defun om-kill-buffer (ombuffer)
  ; (print (list "kill buffer explicit" (buffer ombuffer)))
  ;(editor::delete-buffer (buffer ombuffer))
  (editor::kill-buffer-no-confirm (buffer ombuffer)))

  ;(if (window ombuffer) (setf (killed? ombuffer) :flagged)
  ;  (progn (editor::delete-buffer (buffer ombuffer))
  ;    (setf (killed? ombuffer) t)))

(defun om-copy-buffer (ombuffer)
  (let ((newbuffer (om-make-buffer)))
    (when (and ombuffer (buffer ombuffer))
      (ignore-errors 
        (editor::set-buffer-contents (buffer newbuffer)
                                      (editor::points-to-string (editor::buffers-start (buffer ombuffer))
                                                             (editor::buffers-end (buffer ombuffer))))
        )
      )
    newbuffer))

(defun om-buffer-insert (ombuffer string &optional (position nil))
  (let ((textbuffer (buffer ombuffer)))
    (editor::use-buffer textbuffer
      ;(setf (editor::face-italic-p editor::*current-font*) t)
      (if position
          (editor::with-point ((p (editor:buffers-start textbuffer)))
            (editor::character-offset p position)
            (editor::insert-string p string))
        (editor::insert-string (editor:buffer-point textbuffer) string)))))

(defun om-buffer-insert-newline (ombuffer &optional (position nil))
    (let ((textbuffer (buffer ombuffer)))
      (editor::use-buffer textbuffer
        (if position
            (editor::with-point ((p (editor:buffers-start textbuffer)))
              (editor::character-offset p position)
              (editor::newline p))
          (editor::newline (editor:buffer-point textbuffer))))))

(defun om-buffer-text (ombuffer)
      (let ((textbuffer (buffer ombuffer)))
        (editor::use-buffer textbuffer
          (editor:points-to-string (editor:buffers-start textbuffer) 
                                   (editor:buffers-end textbuffer)))))
      
(defun om-buffer-substring (ombuffer from &optional to)  
      (let ((textbuffer (buffer ombuffer)))
        (editor::use-buffer textbuffer
          (editor::with-point ((p1 (editor:buffers-start textbuffer))
                               (p2 (if to (editor:buffers-start textbuffer)
                                       (editor:buffers-end textbuffer))))
            (editor::character-offset p1 from)
            (when to (editor::character-offset p2 to))
            (editor:points-to-string p1 p2)))))

(defun om-buffer-char (ombuffer &optional pos)
  (let ((textbuffer (buffer ombuffer)))
    (editor::use-buffer textbuffer
      (editor::with-point ((p (if pos (editor:buffers-start textbuffer)
                                (editor:buffer-point textbuffer))))
        (editor::character-at p pos)))))

(defun om-buffer-size (ombuffer)
  (let ((textbuffer (buffer ombuffer)))
    (editor::use-buffer textbuffer
      (editor::count-characters (editor::buffers-start textbuffer)
                                (editor::buffers-end textbuffer)))))
  
(defun om-lines-in-buffer (ombuffer)
  (let ((textbuffer (buffer ombuffer)))
    (editor::use-buffer textbuffer
      (+ 1 (editor::count-lines (editor::buffers-start textbuffer)
                                (editor::buffers-end textbuffer))))))
  

(defun om-buffer-lines (ombuffer)
  (let ((textbuffer (buffer ombuffer)) numlines listline)
    (editor::use-buffer textbuffer
      (setq numlines (editor::count-lines (editor::buffers-start textbuffer)
                                          (editor::buffers-end textbuffer)))
      (setq listlines (editor::list-lines (editor::buffers-start textbuffer) numlines))
      
      (butlast listlines (- (length listlines) (1+ numlines))) 
      )))


(defun om-buffer-delete (ombuffer &optional start end)
  (handler-bind ((error #'(lambda (c) 
                               (print (format nil "Error while cleaning text buffer : ~%~A" c))
                               (om-kill-buffer ombuffer)
                               (setf (buffer ombuffer) (editor::make-buffer (string (gensym)) :flag :om))
                               )))
  (let ((textbuffer (buffer ombuffer)))
    (if start
        (editor::use-buffer textbuffer
          (editor::with-point ((p1 (editor::buffers-start textbuffer)) 
                               (p2 (if end (editor::buffers-start textbuffer)
                                     (editor::buffers-end textbuffer))))
            (editor::character-offset p1 start)
            (when end (editor::character-offset p2 end))
            (editor::delete-between-points p1 p2)           
            ))
      (editor::clear-buffer textbuffer)))))
    

(defun test-buffer ()
  (let ((mybuffer (editor::make-buffer "test"))
        numlines (pos 0))
    ;; fill buffer
    (loop for i from 0 to 1000 do 
          (editor::insert-string (editor:buffer-point mybuffer) "0123456789") ; (format nil "~D" i))
          (editor::newline (editor:buffer-point mybuffer)))
    ;; test
    ;; (print (editor:points-to-string (editor:buffers-start mybuffer) (editor:buffers-end mybuffer)))))
    (setf numlines (+ 1 (editor::count-lines (editor::buffers-start mybuffer)
                                             (editor::buffers-end mybuffer))))

    ;(print numlines)
    ;; find positions of lines beginning
    (editor::with-point ((p (editor::buffers-start mybuffer)))

      ;(loop for i from 0 to numlines do
      ;      (editor::move-point-to-offset p pos)
      ;      (let ((next (editor::next-newline p)))
      ;        (print (list "NEXT NEWLINE AT" (editor::find-point-offset mybuffer p) "-->" next))
      ;        (setf pos (+ next 1))))
      
      (loop for l in (editor::list-lines p nil) do (print l))
      
      )
    
    (editor::kill-buffer-no-confirm mybuffer)
    ))

; (test-buffer)

;;; position de la fin de la ligne courante (ou au point start)
(defun om-buffer-line-end (ombuffer &optional pos)
  
  (let ((textbuffer (buffer ombuffer)))
    (editor::use-buffer textbuffer
      (editor::with-point ((p (if pos (editor::buffers-start textbuffer)
                                (editor:buffer-point textbuffer))))
        ;(when pos (editor::character-offset p pos))
        (when pos (editor::move-point-to-offset p pos))
        (let ((rep (editor::next-newline p)))
          (print (list "NEXT NEWLINE AT" (editor::find-point-offset textbuffer p) "-->" rep))
          rep
        )))))

;;; position du debut de la ligne courante (ou au point start)
(defun om-buffer-line-start (ombuffer &optional pos)
  (let ((textbuffer (buffer ombuffer)))
    (editor::use-buffer textbuffer
      (editor::with-point ((p (if pos (editor::buffers-start textbuffer)
                                (editor:buffer-point textbuffer))))
        (when pos (editor::character-offset p pos))
        (if (editor::same-line-p p (editor::buffers-start textbuffer))
            0
          (if (equal (editor::character-at p 0) #\Newline) pos
            (+ 1 (editor::find-point-offset textbuffer (editor::find-previous-character p #\Newline))))
          )))))

(defun om-buffer-skip-forward (ombuffer &optional start end)
  (let ((textbuffer (buffer ombuffer)))
    (editor::use-buffer textbuffer
      (editor::with-point ((p (if start (editor::buffers-start textbuffer)
                                (editor:buffer-point textbuffer)))
                           (limit (if end (editor::buffers-start textbuffer)
                                    (editor::buffers-end textbuffer))))
        (when start (editor::character-offset p start))
        (when end (editor::character-offset limit end))
        (loop while (and (editor::blank-line-p p) (editor::point< p limit)) do
                 (editor::move-point-to-offset p (editor::next-newline p)))
        (editor::find-point-offset textbuffer p)))
    ))
;; pour sauter aussi les comments : SKIP-LISP-READER-WHITESPACE

;; tests
;(setf buf (om-make-buffer))
;(om-buffer-insert buf "123456789")
;(om-buffer-insert buf "  ")
;(om-buffer-insert-newline buf)
;(om-buffer-text buf)
;(om-buffer-char buf 4)
;(om-buffer-delete buf)
;(om-buffer-size buf)
;(om-lines-in-buffer buf)
;(setf b2 (om-copy-buffer buf))
;(om-buffer-substring buf 6 6)
;(editor::kill-buffer-no-confirm (buffer buf))
;;(editor::delete-buffer (buffer buf))

(defun test-list-of-lines (buffer)
  (when buffer
    (let ((numlines (om-lines-in-buffer buffer))
          (pos 0) rep)
      (loop for i from 1 to numlines do
            (let ((start (om-buffer-line-start buffer pos))
                  (end (om-buffer-line-end buffer pos)))
              (push (om-buffer-substring buffer start end) rep)
              (print (list pos start end))
              (setf pos (+ 1 end))))
      (reverse rep))))

; (test-list-of-lines buf)

;;; DOC fonctions du package EDITOR
; (editor::buffers-start buf)  --> renvoie un point correspondant au debut du buffer
; (editor::current-point buf) --> (verif) renvoir lepoint courant (tous buffers confondus)
; (editor::buffer-point buf) --> renvoie le point courant sur un buffer
; (editor::buffer-end point) -> deplace point a la fin du buffer
; (editor::buffer-start point) -> deplace point au debut du debut... buffer
; (editor::delete-buffer buf &optional force-reset) --> efface un buffer
; (editor::clear-buffer) --> ?
; (editor::delete-between-points start end) --> efface entre les deux points
; (editor::delete-characters point &optional (n 1) --> efface n caracteres apres point
; (editor::count-lines beg end) --> compte le nombre de lignes dans le region
; (editor::line-end point) (editor::line-start point) --> ramene point au debut ou a la fin de la ligne
; (editor::list-lines point num) --> ?
; (editor::newline point &optional cound) --> ?
; (editor::start-line-p point) (editor::end-line-p point) --> T si debut ou fin de ligne
; (editor::move-point-to-offset point offset) --> voir a la place de faire characte-offset, start-point, etc. 
 
;;; ecrit le contenu d'un fichier dans le buffer
(defun om-buffer-insert-file (ombuffer path &optional position)
  (let ((buffer (buffer ombuffer)))
    (let ((filebuf (editor::find-file-buffer path)))
      (when filebuf 
        (if position
          (editor::use-buffer buffer 
            (editor::with-point ((p (editor:buffers-start buffer)))
              (editor::character-offset p position)
              (editor::insert-string p (editor::points-to-string (editor::buffers-start filebuf)
                                                                 (editor::buffers-end filebuf)))))
        (editor::set-buffer-contents buffer 
                                     (editor::points-to-string (editor::buffers-start filebuf)
                                                               (editor::buffers-end filebuf)))
        )
      (editor::kill-buffer-no-confirm filebuf)))))

;;; ecrit le contenu du buffer dans un fichier 
(defun om-buffer-write-file (ombuffer path &key (if-exists :supersede))
  (let ((buffer (buffer ombuffer)))
    (with-open-file (s path :direction :output :external-format :utf-8 :if-exists if-exists)
      (write-string (editor::points-to-string 
                     (editor::buffers-start buffer) 
                     (editor::buffers-end buffer)) 
                    s))))


;(defun om-buffer-eval (ombuffer)
;  (let ((textbuffer (buffer ombuffer)))
;    (editor::use-buffer textbuffer
;      (editor::with-point ((p1 (editor::buffers-start textbuffer)) 
;                           (p2 (editor::buffers-end textbuffer)))
;        (editor::region-eval textbuffer p1 p2 :print t)
        ;(editor::editor-eval-string textbuffer (editor:points-to-string p1 p2))
        ;(print (list "X" editor::*last-evaluate-result*))
;        ))))

(defun om-buffer-eval (ombuffer)
  (let ((textbuffer (buffer ombuffer)))
    (editor::use-buffer textbuffer
      (eval (read-from-string (concatenate 'string "(progn " 
                                           (editor:points-to-string (editor:buffers-start textbuffer) 
                                                                    (editor:buffers-end textbuffer))
                                           ")")))
      )))



(defun om-read-string-from-file (pathname)
  (let ((tmpbuffer (om-make-buffer))
        (str nil))
    (om-buffer-insert-file tmpbuffer pathname)
    (setf str (om-buffer-text tmpbuffer))
    (om-kill-buffer tmpbuffer)
    str))