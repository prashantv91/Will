; Classes and functions for maps and worlds.
; Note: We always use the curses coordinate system.
;
;   (0,0) --> x
;     |
;     v    
;     y     (y,x)
;

(in-package :will.map)

;(defclass-2 terrain ()
  ; Base class for terrains. Doesn't make sense.
  ;)

(defclass-2 tile ()
  ; Each tile on the map.            
  ((terrain 'grass)
   (occupant nil)
   (items nil)
   (viewers nil)))

(defclass-2 area ()
  ; Named in lieu of 'map'.
  ((map-name "")
   (map-array nil)
   (height 0)
   (width 0)))

(defclass-2 world ()
  ; Collection of areas.
  ((world-name "")
   (areas nil)))


(defun make-tile (terrain)
  ; Returns a tile with terrain as @terrain, rest nil.
  (make-instance 'tile :terrain terrain))



