;;;Function pickup
;;;Takes in an object as a parameter
;;;Adds the object to the player inventory, if the object is accessible in the current location
(defun pickup (object)
  (cond
   (*in-dungeon* (dungeon-pickup object))
   ((member object (objects-at *location* *objects* *object-locations*))
    (push (list object 'body) *object-locations*)
    `(you are now carrying the ,object))
   (t '(you cannot get that.))))

;;;Function dungeon-pickup
;;;Picks up an item from the current dungeon room
;;;Takes in an object as a parameter
;;;Adds the object to the player invetory if it is in the current dungeon room
(defun dungeon-pickup (object)
  (cond
   ((member object room-items)
    (setq room-items (remove object room-items))
    (push (list object 'body) *object-locations*)
    `(you are now carrying the ,object))
   (t '(you cannot get that.))))
;;;Function force-pickup
;;;Like pickup, but adds the object regardless of location, good for testing
(defun force-pickup (object)
  (cond ((push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
	  (t '(you cannot get that.))))

;;;Function inventory
;;;Returns the contents of the player's inventory
(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;;Function have
;;;Takes an object as a parameter
;;;Returns true if the player has the object in their inventory, else false/nil
(defun have (object) 
    (member object (cdr (inventory))))


;;;Function remove-all-from-inven
;;;Takes in a list of objects
;;;Removes all of the objects from *inventory*
(defun remove-all-from-inven (objects) 
  (cond
   ((listp objects)
    (loop for obj on objects
	    do (push (list (car obj) 'used) *object-locations*)
    )
   )
  )
)

;;;Function have-all-parts
;;;Returns true when the inventory contains at least 1 item of each type:
;;;RAM, CPU, GPU, and HDD
(defun have-all-parts ()
  (loop for type in '(ram cpu gpu hdd)
	always (loop for item in (cadr (assoc type *valid-parts*))
		     thereis (have item))))