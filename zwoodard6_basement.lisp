(defparameter location-x 50)
(defparameter location-y 50)

(defparameter room-items '())
(defparameter room-monsters '())
(defparameter room-decorations '())
(defparameter room-traps '())

;;;Function rand
;;;Deterministically calculates a value based on seed-x and seed-y
;;;Meant to appear random(ish)
;;;Constants chosen randomly from a list of primes
(defun rand (seed-x seed-y)
  (mod (* 275604541 (+ seed-x 8831) (+ seed-y 10627)) 15485863))

(defun dungeon-walk (direction)
  (cond
   ((eq direction 'north) (setq location-y (1+ location-y)))
   ((eq direction 'south) (setq location-y (1- location-y)))
   ((eq direction 'east) (setq location-x (1+ location-x)))
   ((eq direction 'west) (setq location-x (1- location-x)))
   ((and (eq direction 'upstairs) (= location-x 50) (= location-y 50))
    (setq *in-dungeon* nil)
    (return-from dungeon-walk (walk direction)))
   (T (return-from dungeon-walk '(you cannot go that way.)))
  )
  (build-room location-x location-y)
  (dungeon-look)
)

(defun dungeon-look ()
  (labels ((describe-decoration (x)
				(if (null x) ()
				  (append '(you see a)
					  (cadr (assoc x *dungeon-decorations*))
					  '(on the floor.))))
	   (describe-monster (x)
			     (if (null x) ()
			       (append '(there is a)
				       (list (nth (random (length *monster-descriptors*)) *monster-descriptors*))
				       (cadr (assoc x *dungeon-monsters*))
				       '(error in the middle of the room.))))
	   (describe-item (x)
			  (if (null x) ()
			    (append '(there is a)
				    (cadr (assoc x *dungeon-items*))
				    '(on the floor.))))
	   (describe-trap (x)
			  (if (null x) ()
			    (append '(WATCH OUT! There is a)
				    (cadr (assoc x *dungeon-traps*))
				    '(preventing you from leaving the room.))))
	   )
	  (append '(You are in a small dark room... There are doors to the north south east and west.)
		  (if (and (= location-x 50) (= location-y 50))
		      (describe-paths *location* *edges*)) ;Basement exit
		  (describe-decoration (car room-decorations))
                  (describe-monster (car room-monsters))
		  (describe-trap (car room-traps))
		  (describe-item (car room-items))
	  )))

;;;Macro
;;;DO NOT CALL OUTSIDE OF CONTEXT OF build-room
;;;Uses seed and x to determine whether to execute
;;;Not random, behavior defined by seed and the x chance argument
(defmacro chance (x &body body)
  `(if (= (mod seed ,x) 0) ,@body))


;;;Macro room-list-add
;;;DESTRUCTIVE
;;;Adds the the monster/item/trap/decoration indicated by seed&allowed-list to room-list
;;;EX: (room-list-add monsters 1 *dungeon-monsters*) adds the first monster in *dungeon_monsters* to list monsters
(defmacro room-list-add (room-list seed allowed-list)
  `(setq ,room-list (cons (car (nth (mod ,seed (length ,allowed-list)) ,allowed-list)) ,room-list))
)
(defun build-room (x y)
  (let ((seed (rand x y))
	(monsters ())
	(items ())
	(traps ())
	(decorations ()))
    (chance 2 (room-list-add decorations seed *dungeon-decorations*))
    (chance 2 (room-list-add items seed *dungeon-items*))
    (chance 3 (room-list-add monsters seed *dungeon-monsters*))
    (chance 4 (room-list-add traps seed *dungeon-traps*))
    ;;Update global room variables
    (setq room-monsters monsters)
    (setq room-traps traps)
    (setq room-items items)
    (setq room-decorations decorations)
    (list 'monsters monsters 'items items 'traps traps 'decorations decorations) 
))