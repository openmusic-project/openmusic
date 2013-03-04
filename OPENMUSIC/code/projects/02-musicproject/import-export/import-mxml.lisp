 

(in-package :om)

(defmethod! mxml-import (&optional path)
  :icon 352
  :doc "Constructs a poly object from the data exported by Finale. "
  (let* ((thelist nil)
         (file (or (and path (probe-file path))
                   (om-choose-file-dialog :prompt "Choose Music XML File" :button-string "Import" 
                                       :directory *last-imported* 
                                       :types '("MusicXML Documents" "*.xml" "All Documents" "*.*")))))
    (when file
      (setf *last-imported* (make-pathname :directory (pathname-directory file)))
      (with-open-file (f file :direction :input)
        (setf thelist (om-list-from-xml f)))
      (when thelist (xml-list-to-Poly thelist)))))

(defmethod! import-musicxml (&optional path)
  :icon 352
  :doc "
Constructs a poly object from a MusicXML file.

- <path> allows to specify the MusicXML file to import. If not specified, a file chooser dialog will be dispaled at evaluating the box.
"
  (mxml-import path))

(defun xml-equal (a b)
  (equal (interne a) (interne b)))

(defun tag-equal (a b)
  (if (listp b)
    (equal (interne a) (interne (car b)))
    (equal (interne a) (interne b))))

(defun first! (elem)
  (if (listp elem) (car elem) elem))

(defun find-tag-xml (list tag)
  (let ((pos (position tag list :key 'first! :test 'tag-equal)))
    (when pos
      (nth pos list))))

;;ADDED

(defun count-tag-xml (list tag)
  (let ((c (count tag list :key 'first! :test 'tag-equal)))
    c))

(defun is-first-tag-xml? (list tag)
  (cond
   ((listp (car list)) (is-first-tag-xml? (car list) tag))
    ((symbolp (car list)) (xml-equal (car list) tag))
    (t nil)))


(defun mult-durs (list val)
  (loop for item in list collect
        (if (atom item) (* val item)
            (cons (* val (car item)) (cdr item)))
        ))

(defun integer-ratios (ryt)
  (let ((ratio (find-if 'ratiop ryt :key 'first!)))
    (when (listp ratio) (setf ratio (car ratio)))
    (if ratio
      (integer-ratios (mult-durs ryt (denominator ratio)))
      ryt)))



(defun zero-float-p (x)
(and (floatp x) (> x 0) (< x 1))) 

;;EFFECTUER MEME DEMARCHE POUR TIEFLOAT
(defun integer-tiefloats (ryt)
  (let ((tiefloat (find-if 'zero-float-p ryt :key 'first!)))    
    (when (listp tiefloat) (setf tiefloat (car tiefloat)))    
    (if tiefloat
      (integer-tiefloats (mult-durs ryt (denominator (rationalize tiefloat))))
      ryt)))

(defun xml-to-tree-struct (ryt)
  (loop for item in ryt collect
        (if (atom  item) item
            (let ((list (xml-to-tree-struct item)) vals)
                  (setf vals (loop for f in list collect (first! f)))
                  (list (apply '+ vals) list)))))


(defun xml-to-tree1 (ryt)
  (let (tree)
    (setf tree (integer-ratios ryt))

 ;   (setf tree (integer-tiefloats ryt))

    (loop for item in tree collect
          (if (atom  item) item
              (let ((list (xml-to-tree1 (second item))))
                (list (car item) list))))))

(defun xml-to-tree2 (ryt)
  (let (tree)
    ;(setf tree (integer-ratios ryt))

    (setf tree (integer-tiefloats ryt))

    (loop for item in tree collect
          (if (atom  item) item
              (let ((list (xml-to-tree2 (second item))))
                (list (car item) list))))))

(defvar compteur 0)
 
;;;================MY-GET-REAL-CHORDS=======
;;;
;;; COMME GET-REAL-CHORDS  mais avec les continuation-chords

(defun my-get-real-chords (self)
  (loop for item in (collect-chords self)
        when (or (chord-p item) (cont-chord-p item)) collect item))


;;=================GET BACK A MUSICAL OBJECT (CHORD FOR NOW) FROM A PATH (list)


(defun get-musobject-from-path (musobj path)
(if (cdr path)
  (get-musobject-from-path (nth (car path) (inside musobj) ) (cdr path))     
              
  (nth (car path) (my-get-real-chords musobj))
))



;;=================GET BACK A MUSICAL OBJECT (WHATEVER !!) FROM A PATH (list)

(defun get-musobject-from-path2 (musobj path)
(if (cdr path)

  (get-musobject-from-path2 (nth (car path) (inside musobj) ) (cdr path))     
   
      


  (nth (car path) (inside musobj))
  
))




;;;;;;DEFINE MY ADD EXTRA TO REALIZE THEM (SKIP GROUP)
(defmethod! my-add-extra-xml ((self simple-container) extras path)
   :icon 162
   (let ((extra1 (loop for item in (list! extras) collect (convert-extra item)))
         (obj   ( get-musobject-from-path self path)))
     (when obj
       (loop for item in extra1 do
             (set-extra-in-list (clone item) obj)))))



(defun xml-to-tree (ryt)
  (let* ((tree (xml-to-tree-struct  ryt)) 
          ties)
    (xml-to-tree2 (xml-to-tree1 tree))
    ))

;;;OK AVNT XML-TO-TREE2

(defun xml-list-to-Poly (list)
  (let (voices tem listtemp tempvoice tempi premvoice graces texts)
    (when (is-first-tag-xml? list 'score-partwise)
      (pop list)
      (loop while (not (is-first-tag-xml?  list  'part-list)) do
            (pop list))
      (if (is-first-tag-xml?  list  'part-list)
        (pop list))
      ;(setf list (list (car list)))
      (setf prevoices
            (loop for item in list 
                  collect (xml-list-to-staves item)))
      (setf prevoices (flat-once prevoices))
      (setf voices
            (loop for item in prevoices   ;;avant list
                  collect (let* ( (rep (mat-trans  (xml-list-to-voice item)))  
                                tree  chords ) 
                            

                            (setf chords (flat (first rep) 1))
                         
                            
                            (setf graces (flat (fifth rep) 1))

                            (setf texts (flat (eighth rep) 1))

                            (setf tree (loop for sig in (second rep)
                                             for ryt in (third rep)                                    
                                             collect (list sig (xml-to-tree ryt))))
                           ; (print tree)

                            (setf *transpose* nil)
                            (setf *numdia* nil)
                            (setf *numchrom* nil)
                            (setf tempi (cons (fourth rep) tempi))

                          ;  (print chords)

                            

                            (setf premvoice (make-instance 'voice
                                                           :tree tree
                                                           :chords chords                                          
                                                           ))
                            


                            (loop for item2 in graces do
       
                                 (set-grace-notes (nth (third item2) (cons-chord&rest-list (nth (second item2) (inside premvoice))))  (first item2) t)
                                  )

                            (loop for item3 in texts do
       
                                (loop   for item4 in (first item3) 
                                        for i = 2 then (+ i 2) do
                                        (my-add-extra-xml 
                                  
                                  ;(nth (third item3) (cons-chord&rest-list (nth (second item3) (inside premvoice)))) 
                                  premvoice

 (make-instance 'text-extra :deltay i :thetext item4 ) (list (second item3) (third item3) ) )
                                        )

                                )
                            premvoice

                                  ))
            )

  
      ;;;;AFTER EACH CREATED VOICE, CHANGE THE TEMPO IF NECESSARY


(setf tempi (flat-once (delete nil tempi)))


( if tempi
          (progn 
            
            (setf listtemp (loop for temp in tempi
                                 collect (second temp)))
            
            (if (every #'(lambda(x) (= x 0)) listtemp)  
                (loop for vo in voices do (setf (tempo vo) (first (first tempi)) ))
              
              ;;IF TEMPO IS MODIFIED DURING THE SCORE
              (progn
                

                (loop for vo in voices do (setf (tempo vo)  (list (list 1/4 (first (first tempi))) 
                                                                  (loop for temp in tempi  
                                                                   ;     do (print temp)
                                                                        if (not (= (second temp) 0)) 
                                                                        collect (list (list (second temp) 0) (list 1/4 (first temp) nil))
                                                                        )
                                                                  )
                                                )
                      )
                )
              )
            )
  )
          
     ; (om-inspect voices)
      (setf poly (make-instance 'poly :voices voices))
      
)
)
      
)


(defvar *last-sign* nil)
(defvar *last-div* nil)
(defvar *referencedurtotal* nil)


(defvar *tempo* nil)

(defvar *transpose* nil)
(defvar *numdia* nil)
(defvar *numchrom* nil)

(defvar *num-measure* 0)

(defvar *previous-tie* nil)

;;;;;;;RENVOIE UNE LISTES DES VOIX CONTENUES DANS LES VOICES  DU XML (1 voix piano -> 2 voix)

(defun xml-list-to-staves (list1)     ;;;; car list TYPE : (:|part| :|id| "P2")



;;DANS PREMIERE MESURE -> (second list1)

(let* (  (staves-number 0)  (newvoice nil)   attr  staff?)
(setf attr (find-tag-xml (second list1)  'attributes))
(when attr
(setf staves (find-tag-xml (cdr attr) 'staves))
(if staves  
    (progn
      (setf staves-number (read-from-string (second staves))  )
      (loop for i from 1 to staves-number do
            (setf newvoice (cons list1 newvoice)))

    ;;;;;FIND-TAG-XML : RENVOIE LE CAR DU BON TAG (donc chercher dans le cdr)

    (loop for i from 1 to staves-number 
          do (progn
               (let* ((listtemp  (copy-list (nth (- i 1) newvoice))))
                 (loop for item in listtemp 
                       when (progn (setf measure?  (find-tag-xml item 'measure) )
                              measure?)
                       do 
                       (let* ((listprov  item))
                         (loop for item2 in (cdr item) 
                               do  (progn                                     
                                     (setf note?  (find-tag-xml item2 'note) )
                                     (when note? 
                                       (setf staff? (find-tag-xml (cdr item2) 'staff))
                                       
                                       ;;SI PAS DE STAFF (ON ENLEVE LA NOTE) ON REGARDE LE VOICE TAG
                                       
                                       (if staff?                                           
                                           (when (not (= (read-from-string (second staff?)) i ))                                          
                                             (setf item (remove item2 item)))                                                             
                                             (progn (setf voice?  (find-tag-xml item2 'voice) ) 
                                               (when (not (= (read-from-string (second voice?)) i ))                   
                                                 (setf item (remove item2 item)))))))
                               )
                         (setf (nth (position listprov listtemp) listtemp ) item)                               
                         ))
                 (setf (nth (- i 1) newvoice) listtemp)
                 )))
    newvoice
    )  
 (list list1)))))







(defun xml-list-to-voice (list)
(setf *num-measure* 0) 
  (loop for item in (cdr list)            ;;;; car list TYPE : (:|part| :|id| "P2")
        collect (xml-list-to-measure item)
        do (setf *num-measure* (+ *num-measure* 1) )))


(defun xml-list-to-measure (list)

;;;num-element-sound : without rests
  (let* ( (num-element -1) (num-element-sound -1) chords  attr attrtemp listattr key keynumalt keymode beampile chordpile (prec-chord-tie nil)  durchordpile  durchordtielist tiepile tietemp durpile cur-dur-list tim numbeats beattype  durtotal tie  gracepile gracetemppile graces (count-tag 0) (tempo nil) copytempo (beamnumber 0) motherdur listgroupdur lyrictemp listlyric lyric text myextratext textpile)    ;;;tie like durations
        (setf tempo (find-tag-xml list 'direction))
    (when tempo (setf tempo (find-tag-xml tempo 'sound)))
    (when tempo 
      (if (tag-equal    (second (flat tempo)) "tempo" )
          (setf tempo (read-from-string (third (flat  tempo))))
          (setf tempo nil)
          ))
    (when tempo (setf tempo (list tempo *num-measure*)))
    (setf attr (find-tag-xml list 'attributes))
    (setf attrtemp attr)

    ;;;LIST OF ATTRIBUTES (THERE CAN BE SEVERAL ATTRIBUTES CONTAINING THE RELATED TAGS
    (setf listattr (loop while attrtemp 
                         collect attrtemp
                         do (setf attrtemp (find-tag-xml (cdr ( member attrtemp list)) 'attributes))))
    (loop for item in listattr do
          (progn 
            (when  (find-tag-xml (cdr item) 'divisions) 
              (progn (setf div (find-tag-xml (cdr item) 'divisions))  
                (when div (setf *last-div* (read-from-string (second div))))))

            ;;TONALITY
            (when (find-tag-xml (cdr item) 'key)
              (setf key (find-tag-xml (cdr item) 'key))
              (setf keynumberalt (find-tag-xml (cdr key) 'fifths))
              (setf keymode (find-tag-xml (cdr key) 'mode))
              (setf keymode (second keymode))
              )

            (when  (find-tag-xml (cdr item) 'transpose) 
              (progn (setf trans (find-tag-xml (cdr item) 'transpose))  
                (when trans 
                  (setf *transpose* t)
                  (setf *numdia* (read-from-string (second (find-tag-xml (cdr trans) 'diatonic))))
                  (setf *numchrom* (read-from-string (second (find-tag-xml (cdr trans) 'chromatic))))
                  )))
            (when  (find-tag-xml (cdr item) 'time) 
              (progn                
                (setf tim (find-tag-xml (cdr item) 'time))                                                                                                  
                (when tim 
                  (setf numbeats (find-tag-xml (cdr tim) 'beats))
                  (setf beattype (find-tag-xml (cdr tim) 'beat-type)))                
                (setf *referencedurtotal* (* 4 (/ (read-from-string (second numbeats)) (read-from-string (second beattype)))))                
                (setf sign (find-tag-xml (cdr item) 'time))
                (when sign (setf *last-sign* (list (read-from-string (second (second sign))) (read-from-string (second (third sign)))))))))    
      )
    (loop while (not (is-first-tag-xml?  list  'note)) do
          (pop list))
    (loop for note in list do
          (when (is-first-tag-xml? note 'note)
            (let* (pitch (alter 0) chord? rest? grace? dur beam irratio)
              (setf grace? (find-tag-xml (cdr note) 'grace))
              (setf rest? (find-tag-xml (cdr note) 'rest))
              (if (not rest?)
                (progn          
     
                  ;;"REAL" NOTE
                  (when (find-tag-xml (cdr note) 'pitch) 
                    (setf pitch (xml2midi (find-tag-xml (cdr note) 'pitch)))
                    (setf alter (find-tag-xml (cdr (find-tag-xml (cdr note) 'pitch)) 'alter))
                    (if alter (setf alter (read-from-string (second alter)))
                              (setf alter 0)
                      )
                    )
                    
                  ;;"PERCUSSIVE" NOTE
                  (when (find-tag-xml (cdr note) 'unpitched) (setf pitch (xml2midi-unpitch (find-tag-xml (cdr note) 'unpitched))))
                  (setf beam (find-tag-xml (cdr note) 'beam))
                  (when beam (setf beam (second beam)))

                  ;;LYRICS

                  (setf lyrictemp (find-tag-xml (cdr note) 'lyric))
                  
                  (setf listlyric (loop while lyrictemp 
                                        collect lyrictemp
                                        do (setf lyrictemp (find-tag-xml (cdr ( member lyrictemp (cdr note))) 'lyric))))
                  

                
                  (when listlyric 
                    (setf myextratext  
                          (loop for item in listlyric  
                                if (find-tag-xml (cdr item) 'text   )                                                                                                                             
                                collect   (second (find-tag-xml (cdr item) 'text) )
                               
                                ))
                    (push (list myextratext  *num-measure* (+ num-element-sound 1) ) textpile)
                    )
                  
                  
                  
                
                   
                  
                  
             

                  (setf tie (find-tag-xml (cdr note) 'tie))
                  (when tie (setf tie (third  (flat tie)))) 
                  (setf chord? (find-tag-xml (cdr note) 'chord))                 
                  (if grace?
                      ;;GRACE NOTES
                      
                      ;;IF GRACE NOTE : TWO CASES : GRACE CHORD OR GRACE NOTE
                      (if chord?                          
                          (push pitch  gracetemppile)
                        (progn 
                          (when gracetemppile
                            (push (reverse gracetemppile) gracepile)
                            (setf gracetemppile nil)
                            )
                          (push pitch  gracepile)
                          )
                        )                    
                    ;;; NO GRACE NOTE
                    (if chord?                       
                        ;;IF CHORD  
                        
                        ;;PROBLEM OF DIFFERENT DURATIONS OF THE NOTES OF THE SAME CHORD
                        ;;WE MUST STOCK THE DURATIONS AND  CHECK AT THE END OF THE CHORD
                        
                        (if *previous-tie*
                            ;;IF THERE IS STILL THE TIE OF PRECEDENT MEASURE
                            (progn
                              (setf count-tag (count-tag-xml (cdr note) 'tie))
                              (when  (= count-tag 1)
                                (when (equal tie "stop")
                                  (setf *previous-tie* nil)                                  
                                  (setf prec-chord-tie t)
                                  ))
                              (setf chordpile nil))
                          ;;NO PRECEDENT MEASURE TIE BUT CHECK IF WE HAVE A FINISHING TIE CHORD AND STOCK THE DURATION OF THE NOTE INSIDE THE CHORD
                          (when ( not prec-chord-tie)  
                            (push (list pitch alter) chordpile) 
                            (push tietemp tiepile)
                            (setf dur (find-tag-xml (cdr note) 'duration)) 
                            (setf dur (read-from-string (second dur)) ) 
                            (setf dur   (/ dur *last-div*))
                            (push dur durchordpile))
                          )                         
                      
                      ;;IF NOTE
                      (if *previous-tie*
                          
                          ;CHECK IF WE STILL HAVE PRECEDENT TIE
                          (progn                
                            (setf count-tag (count-tag-xml (cdr note) 'tie))
                            (when  (= count-tag 1)
                              (when (equal tie "stop")
                                (setf *previous-tie* nil)
                                ;;IF THERE IS A CHORD AFTER, KEEP THE TIE INFORMATION
                                (setf prec-chord-tie t)))              
                            ;;WE EMPTY THE CHORDPILE
                            (setf chordpile nil)
                            (setf num-element (+ 1 num-element))                           
                            (setf dur (find-tag-xml (cdr note) 'duration))
                            (setf tie (find-tag-xml (cdr note) 'tie))
                            (when tie (setf tie (third  (flat tie)))) 
                            (when dur
                              (setf dur (read-from-string (second dur)))                              
                              (if (equal tie "stop")
                                  (setf dur (float (/ dur *last-div*)))
                                (setf dur   (/ dur *last-div*))
                                )          
                              ;;BEAM PROBLEM
                              (cond
                               ((null beam)  (push dur durpile) )
                               ((or (string-equal beam "begin") (string-equal beam "start"))                                 
                                (progn (setf beamnumber (+ 1 beamnumber))                                  
                                  (setf motherdur cur-dur-list)
                                  (push motherdur listgroupdur )                                                                    
                                  (setf cur-dur-list nil)
                                  (push dur cur-dur-list)        
                                  ))
                               ((string-equal beam "continue") (push dur cur-dur-list) )
                               ((string-equal beam "end")                                 
                                (if (= 1 beamnumber)                                    
                                    (progn
                                      (push dur cur-dur-list)
                                      (push (reverse cur-dur-list) durpile)
                                      (setf cur-dur-list nil)                                      
                                      (setf beamnumber (-  beamnumber 1)))                                
                                  (progn
                                    (push dur cur-dur-list)
                                    (setf motherdur (cons (reverse cur-dur-list) motherdur))                                    
                                    (setf cur-dur-list motherdur)
                                    (pop listgroupdur)                                    
                                    (when (first listgroupdur)  (setf motherdur (first listgroupdur)))                           
                                    (setf beamnumber (-  beamnumber 1)))                             
                                  )
                                ))))                        
                        ;;NO PRECEDENT TIE
                        (progn                          
                          (setf prec-chord-tie nil)
                          ;;WHEN WE HAVE COMPLETED A CHORD

                          ;;WE HAVE TO CHECK IF ALL THE NOTE DURATIONS OF THE SAME CHORD ARE EQUAL 
                          ;;IF NOT, WE ADD EXTRA NOTES TO SEPARATE THE CHORD AND WE ADD TIES 
                       ;   (print "durchordpile")
                       ;   (print durchordpile)
                          (when chordpile                              
                            (setf count-tag (count-tag-xml (cdr note) 'tie))                       
                            (when (< count-tag 2)
                              (when (not (equal tie "stop")) (push (reverse chordpile) chords) (push tietemp tiepile) ))
                            ;;WE HAVE A CHORD (MORE THAN ONE NOTE)
                            (when (> (list-length durchordpile) 1)
                              ;;IF ALL THE NOTES OF THE CHORD DON'T HAVE THE SAME DURATION WE MUST CUT THE CHORD
                              (when (not (every #'(lambda(x) (equal (first durchordpile) x)) durchordpile)) 
                               
                                (pop durpile)
                                
                                (setf durchordpile (mapcar #'(lambda(x) (/ x *last-div* )) durchordpile))
                               ; (print "yeah")
                                (setf durchordtielist (make-unbalanced-chord (reverse durchordpile) (reverse chordpile)))  
                               ; (print durchordtielist)
                                (setf chordpile (first durchordtielist))
                                (push (second durchordtielist) durpile)
                                (setf tiepile (third durchordtielist) )
                                (push (reverse chordpile) chords)
                                ))
                            )
                          (setf chordpile nil)
                          (setf durchordpile nil)
                          (push  (list pitch alter) chordpile)
                          (push tietemp tiepile)
                          (setf num-element (+ 1 num-element))
                          (setf dur (find-tag-xml (cdr note) 'duration))
                          (push (/ (read-from-string (second dur)) *last-div*) durchordpile)
                          (setf tie (find-tag-xml (cdr note) 'tie))
                          (when tie (setf tie (third  (flat tie)))) 
                          ;;IF GRACES STOCKED
                          (when gracepile 
                            (push (list (reverse gracepile) *num-measure* num-element)  graces)
                            (setf gracepile nil))                          
                          (when dur
                            (setf dur (read-from-string (second dur)))                        
                            (if (equal tie "stop")
                              (setf dur (float (/ dur *last-div*)))
                              (setf dur   (/ dur *last-div*))
                              )
                            ;;BEAM PROBLEM
                            (cond
                             ((null beam)  (push dur durpile)  )
                             ((or (string-equal beam "begin") (string-equal beam "start")) 
                              (progn (setf beamnumber (+ 1 beamnumber))                                  
                                (setf motherdur cur-dur-list)
                                (push motherdur listgroupdur )
                                (setf cur-dur-list nil)
                                (push dur cur-dur-list)))
                             ((string-equal beam "continue")  (push dur cur-dur-list) )
                             ((string-equal beam "end")                              
                              (if (= 1 beamnumber)
                                  (progn
                                    (push dur cur-dur-list)
                                    (push (reverse cur-dur-list) durpile)
                                    (setf cur-dur-list nil)
                                    (setf beamnumber (-  beamnumber 1))
                                    )                                
                                (progn
                                  (push dur cur-dur-list)
                                  (setf motherdur (cons (reverse cur-dur-list) motherdur))                                  
                                  (setf cur-dur-list motherdur)
                                  (pop listgroupdur)                                  
                                  (when (first listgroupdur)  (setf motherdur (first listgroupdur)))                               
                                  (setf beamnumber (-  beamnumber 1))                                  
                                  ))
                                )))))))
                  (setf num-element-sound (+ num-element-sound 1)))
                  
                ;;IF REST
                (progn
                  (setf beam (find-tag-xml (cdr note) 'beam))
                  (when beam (setf beam (second beam)))
                  (setf tie nil)
                  (setf  count-tag 0)
                  (setf dur (find-tag-xml (cdr note) 'duration))
                  (when dur
                    (setf dur (read-from-string (second dur)))
                    (setf dur (* -1 (/ dur *last-div*)))
                    (setf num-element (+ 1 num-element))
                    ;;BEAM PROBLEM
                    (cond
                     ((null beam)  (push dur durpile))
                     ((or (string-equal beam "begin") (string-equal beam "start"))    
                      (progn (setf beamnumber (+ 1 beamnumber))                        
                        (setf motherdur cur-dur-list)
                        (push motherdur listgroupdur )
                        (setf cur-dur-list nil)
                        (push dur cur-dur-list)))
                     ((string-equal beam "continue")  (push dur cur-dur-list))
                     ((string-equal beam "end")                       
                      (if (= 1 beamnumber)                          
                          (progn
                            (push dur cur-dur-list)
                            (push (reverse cur-dur-list) durpile)
                            (setf cur-dur-list nil)                            
                            (setf beamnumber (-  beamnumber 1))                            
                            )                        
                        (progn
                          (push dur cur-dur-list)
                          (setf motherdur (cons (reverse cur-dur-list) motherdur))                          
                          (setf cur-dur-list motherdur)
                          (pop listgroupdur)                          
                          (when (first listgroupdur)  (setf motherdur (first listgroupdur)))
                          (setf beamnumber (-  beamnumber 1)))))
                      )))))))
  (if (or (= count-tag 2) (equal tie "start"))
      (setf *previous-tie* t)
    (setf *previous-tie* nil))  
   

  

 ; (print "durchordpile2")
 ; (print durchordpile)
  (setf durtotal (reduce #'+ (mapcar #'abs (flat durpile))))
  (when chordpile  
    (if (> (list-length durchordpile) 1)
      ;;IF ALL THE NOTES OF THE CHORD DON'T HAVE THE SAME DURATION WE MUST CUT THE CHORD
      (if (not (every #'(lambda(x) (equal (first durchordpile) x)) durchordpile)) 
        (progn 
          (pop durpile)

          (setf durchordpile (mapcar #'(lambda(x) (/ x *last-div* )) durchordpile))
          ;(print "yeah")
          (setf durchordtielist (make-unbalanced-chord (reverse durchordpile) (reverse chordpile)))  
         ; (print durchordtielist)
          ;(setf chordpile  (first durchordtielist))
          (push (second durchordtielist) durpile)
          (setf tiepile (third durchordtielist) )
          
          ;(print "chord")
          ;(print chordpile)

    
 
          (push (reverse chordpile) chords)  
           ;;BUG ? ONE COUPLE OF PARENTHESIS TOO MUCH
          (when (< (list-length chords) 2) (setf chords (flat chords 1)))
        )  
        (push (reverse chordpile) chords)
       ) 
        
        
      (push (reverse chordpile) chords)
      )
    )
  (print (list "chords---------" (reverse chords) ) )  
  (print (list "durs---------" (reverse durpile) ) )


  
    (setf chords (loop for item in chords 
                       collect 
                       (if ( <= (list-length (flat item)) 2)
                           ;;IF ONE NOTE
                           (let* ( (mynote  (make-instance 'note :midic (first (flat item 1)) ))
                                   (rightnote (find-if #'(lambda(x) (equal (second x)  (second (flat item 1)))) (find-alt-list (first (flat item 1)))))
                                  
                                   )  
                             
                             ;;RIGHTNOTE IS NIL IF THERE IS NO 0 IN THE ALT LIST
                              (when (not rightnote) (setf rightnote (find-if #'(lambda(x) (equal (second x) 1)) (find-alt-list (first (flat item 1))))))
                           (when (not (= (second (flat item 1)) 0 ))  (set-note-tonalite mynote (first rightnote) (second rightnote)))
                           (objfromobjs (list mynote) (make-instance 'chord))
                           )
                         
                         (objfromobjs (loop for item2 in  item
                                            collect 
                                            (let* ( (mynote  (make-instance 'note :midic (first item2) ))
                                                    (rightnote (find-if #'(lambda(x) (equal (second x) (second item2))) (find-alt-list (first item2))))
                                                    )
                                              
                                               ;;RIGHTNOTE IS NIL IF THERE IS NO 0 IN THE ALT LIST
                                                (when (not rightnote) (setf rightnote (find-if #'(lambda(x) (equal (second x) 1)) (find-alt-list (first (flat item 1))))))
                                                (when (not (= (second item2) 0 ))  (set-note-tonalite mynote (first rightnote) (second rightnote)))
                                                mynote
                                                )
                                            ) (make-instance 'chord) )
                         )
                       )
          )
    
    

  (if (< durtotal *referencedurtotal*)
      (list (reverse chords) *last-sign*  (cons  (-  durtotal *referencedurtotal*) (reverse durpile))  tempo  (reverse graces) tiepile (list keynumberalt keymode) textpile)
    (list (reverse chords) *last-sign*  (reverse durpile)  tempo  (reverse graces) tiepile (list keynumberalt keymode) textpile)
    )  
  ))



(defvar *notas-name* '("c" nil "d" nil "e" "f" nil "g" nil "a" nil "b"))

(defun xml2midi (list)
  (let ((step (find-tag-xml (cdr list) 'step))
        (octv (find-tag-xml (cdr list) 'octave))
        (alter (find-tag-xml (cdr list) 'alter))
        )

    (if *numdia*
      (progn     
        (setf step (position (second step) *notas-name* :test 'string-equal))
        (if alter
          (setf alter (read-from-string (second alter)))
          (setf alter 0))
        (cond  
         ( (= *numdia* -1) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 100))
         ( (= *numdia* -2) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 300))
         ( (= *numdia* -3) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 500))
         ( (= *numdia* -4) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 700))
         ( (= *numdia* -5) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 800))
         ( (= *numdia* -6) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 1000))
         ( t (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))))
         )
      )
      (progn  
      (setf step (position (second step) *notas-name* :test 'string-equal))          
      (if alter
        (setf alter (read-from-string (second alter)))
        (setf alter 0))
      (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter)))
    )
  )
)
)

;;;XML2MIDI POUR DES HAUTEURS "DISPLAY" (INDICATRICES : Ex PERCUSSIONS ETC)
(defun xml2midi-unpitch (list)
  (let ((step (find-tag-xml (cdr list) 'display-step))
        (octv (find-tag-xml (cdr list) 'display-octave))
        (alter (find-tag-xml (cdr list) 'display-alter))
        )

    (if *numdia*
      (progn     
        (setf step (position (second step) *notas-name* :test 'string-equal))
        (if alter
          (setf alter (read-from-string (second alter)))
          (setf alter 0))
        (cond  
         ( (= *numdia* -1) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 100))
         ( (= *numdia* -2) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 300))
         ( (= *numdia* -3) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 500))
         ( (= *numdia* -4) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 700))
         ( (= *numdia* -5) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 800))
         ( (= *numdia* -6) (- (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))) 1000))
         ( t (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter))))
         )
      )
      (progn  
      (setf step (position (second step) *notas-name* :test 'string-equal))          
      (if alter
        (setf alter (read-from-string (second alter)))
        (setf alter 0))
      (* 100 (+ (* (+ 1 (read-from-string (second octv))) 12) (+ step alter)))
    )
  )
)
)

;;PREND UN ACCORD NON EQUILIBRE DANS LE TEMPS ET UNE LISTE DE HAUTEURS 
;;RENVOIE UNE LISTE AVEC
      ;LA LISTE DE CHORD (CHORDPILE)
      ;LA LISTE DE DURATION (DURPILE)
      ;LA LISTE DE TIE (TIEPILE)

;;tiepile : nil au départ
(defun make-unbalanced-chord (durchordpile chordpile)
  (let* ( (assoclist  (pairlis durchordpile chordpile))
          (pivot 0) 
          chordresult  tieresult durresult )
 

    ;;ASSOCIER LES DURCHORDPILE AVEC LES CHORDPILE (assoclist : pairlis durchordpile chordpile) 
    ;;CLASSER LE DURCHORDPILE PAR ORDRE CROISSANT
    (sort assoclist '< :key 'car)
    ;;RETIRER LE DURCHORDPILE MINIMUM A TOUS LES DURCHORDPILE (ET DONC A CAR ASSOCLIST)
;(setf pivot (car (car assoclist)))
;(setf assoclist (mapcar #'(lambda(x) (setf (car x) (- (car x) pivot)) x) assoclist)) 

    ;;PUSH (CONS PLUTOT) LES CHORDPILE (second assoclist) DANS CHORDPILE


    (loop while assoclist
          do (setf pivot (car (car assoclist)))
          (setf assoclist (mapcar #'(lambda(x) (setf (car x) (- (car x) pivot)) x) assoclist))
                  
          (setf durresult (cons pivot durresult  ))
         
          (setf working (loop for item in assoclist
                              collect (cdr item) into mylist
                              if (> (car item) 0)
                              collect (cdr item) into mylist2
                              finally (return (list mylist mylist2))))
          (setf chordresult (cons (first working) chordresult ))
          (setf tieresult (cons (second working)  tieresult ))
          (setf assoclist (remove-if #'(lambda(x) (zerop (car x))) assoclist))
         
                      
                       
          )
      

    (list (reverse chordresult)  (reverse durresult) (reverse tieresult) )

    ;;PUSH (CONS PLUTOT) LES CHORDPILE (second assoclist) DONT LES DURCHORDPILE SONT NON NULS DANS TIEPILE
    ;;ENLEVER LES DURCHORDPILE (car assoclist) NULS (DANS ASSOCLIST)
    ;;if assoclist nil (list durchordpile chordpile tiepile) else make-unbalanced-chord (durchordpile chordpile tiepile)

    )
  )