(in-package :alloy-desktop)

;;; Light desktop look: GDI-style black input text on light grey faces.

(simple:define-palette desktop-light
  (:window                 #xF0F0F0)
  (:border                 #xD0D0D0)
  (:background             #xE0E0E0)
  (:foreground             #x151515)
  (:background-alt         #xE8E8E8)
  (:tooltip-background     #xD8D8D8)
  (:tooltip-foreground     #x202020)
  (:component-background   #x0088EE)
  (:component-foreground   #xF0F0F0)
  (:input-background       #xD0D0D0)
  (:input-foreground       #x000000)
  (:placeholder-foreground #x808080)
  (:alert-background       #xFF0000)
  (:alert-foreground       #xFFFFFF)
  (:notice-background      #x0088EE)
  (:notice-foreground      #xFFFFFF)
  :weak-focus
  (:border                 #xA0A0A0)
  (:component-background   #x1199FF)
  :strong-focus
  (:border                 #x33AAFF)
  (:component-background   #x33AAFF)
  (:input-background       #xE0E0E0)
  (:placeholder-foreground #xA0A0A0)
  :disabled
  (:component-background   #xB0B8BF)
  (:component-foreground   #x808080)
  (:input-background       #xA0A0A0)
  (:input-foreground       #x606060)
  (:placeholder-foreground #x808080)
  :activated
  (:component-background   #x80D0FF)
  (:component-foreground   #xFFFFFF))

(defparameter *default-window-background*
  #+win32  (chroma:rgb 0.96 0.96 0.96 1.0)
  #-win32  (chroma:rgb 0.9  0.9  0.9  1.0)
  "Background of a window made without an explicit colour: the light grey of the
platform's dialog face.")
