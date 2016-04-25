;;Game state variables
;Difficulty from 0 to 100, where 100 is the hardest
(defparameter *difficulty* 40)
;Used for basement combat/traps, when it hits 0 you die
(defparameter *health* 10)
;Set to true after talking to wizard for first time
(defparameter *started-quest* nil)
;Set to true after completeing quest, exit when true
(defparameter *victory* nil)
;Set to true when in basement, changes execution path for many functions
(defparameter *in-dungeon* nil)

(defparameter *allowed-commands* '(look walk pickup inventory))
(defparameter *objects* '(whiskey))
(defparameter *object-locations* '((whiskey living-room)))
(defparameter *location* 'living-room)
;;Defines a list of "nodes" where each node is a location and its description                                  
(defparameter *nodes* '((living-room (you are in the living-room.
                            a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                            there is a well in front of you.))
                        (attic (you are in the attic.
                            there is a giant welding torch in the corner.))))
(defparameter *edges* '((living-room (garden west door) (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))
(defparameter *valid-ram* '(2gb-ram
			    4gb-ram
			    8gb-ram))
(defparameter *valid-gpu* '(gtx-660
			    gtx-750
			    r7-370
			    gtx-950
			    r9-390
			    gtx-980
			    ))
(defparameter *valid-cpu* '(i3-6100
			    i5-6400
			    i5-6600k
			    i7-6700k
			    i7-5820k
			    i7-5960x
			    ))
(defparameter *valid-hdd* '(128gb-hdd
			    256gb-hdd
			    512gb-hdd
			    1tb-hdd
			    2tb-hdd
			    4tb-hdd
			    128gb-ssd
			    256gb-ssd
			    512gb-ssd
			    1tb-ssd
			    2tb-ssd
			    4tb-ssd			   
			    ))
(defparameter *valid-parts* (list
			     (list 'ram *valid-ram*)
			     (list 'gpu *valid-gpu*)
			     (list 'cpu *valid-cpu*)
                             (list 'hdd *valid-hdd*)
			    ))

;;DUNGEON VARIABLES
(defparameter *monster-descriptors* '(nasty unsafe unchecked vile complicated horrible foul terrible dangerous bad))
(defparameter *dungeon-monsters* '((stack-overflow (stack overflow))
				   (integer-overflow (integer overflow))
				   (integer-underflow (integer underflow))
				   (race-condition (race condition))
				   (missing-semicolon (missing semicolon))
				   (sql-injection (sql injection))
                                   (casting-error (casting))
                                   (divison-by-zero (divison by zero))
                                   (depreciated (depreciated))
                                   (undefined-variable (undefined variable))
                                   (undefined-function (undefined function))
                                   (out-of-memory (out of memory))
                                   (memory-leak (memory leak))
                                   (invalid-type (invalid type))
                                   (out-of-bounds (array out of bounds))
				 ))
(defparameter *dungeon-decorations* '((skull (bleached white skull))
				      (painting (blood soaked painting))
				      (armor (suit of shiny armor))
				      (coffin (rusty metal coffin with the lid missing))
				     ))
(defparameter *dungeon-traps* '((spike-pit (spike pit filled with bones))
				(tripwire (hidden trip wire))
				(swinging-blades (razor sharp swinging blade))
				(dart-trap (hidden poison dart shooter))
			       ))
(defparameter *dungeon-items* '((mountain-dew (warm mountain dew))
				(doritos (fresh bag of doritos))
				(cheetos (unopened bag of cheetos))
			       ))
;;Add the dungeon items to the object list
(loop for item in *dungeon-items*
      do (push (car item) *objects*))
;;Add the computer parts to the object list
(loop for sublist in *valid-parts*
      do (loop for item in (cadr sublist)
	       do (push item *objects*)))


;;;Function simple-game-action
;;;For game actions with no objects or location requirements
(defmacro simple-game-action (command &body body)
  `(progn (defun ,command ()
	        ,@body
		  )
	    (pushnew ',command *allowed-commands*)))

;;Define the main help function, prints contents of *help*
(simple-game-action help
		    (let ((help-text (append '((The goal of this game is to retrieve computer parts for the wizard))
					     '((Explore rooms\, pickup objects\, and use them to progress.))
					     '((Here is a list of commands to help you do this\:))
					     (mapcar #'(lambda (x) (list '--> x)) *allowed-commands*)
					     '(Consult the user manual for details on each command))))
		      (loop for line on help-text do (game-print (car line)))))
;;Define the aliases
(simple-game-action h (help))
(simple-game-action ? (help))


;;;Function game-action
;;;Allows easy definition of a game actions between two objects
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
            (if (and (eq *location* ',place)
                     (eq subject ',subj)
                     (eq object ',obj)
                     (have ',subj))
                ,@body
            '(i cant ,command like that.)))
          (pushnew ',command *allowed-commands*)))

;;;Function new-object
;;;Will add a new object to the game
;;;Error checks to make sure the location exists and the item does not already exist
(defmacro new-object (object location)
  `(cond
    ;;Check to make sure location exists
    ((not (assoc ,location *nodes*))
     (format T "Error in new-object: location does not exist~%"))
    ;;Check to make sure the object doesn't already exist
    ((member ,object *objects*)
     (format T "Error in new-object: object already exists~%"))
    (T
     (pushnew ,object *objects*) ;Add object to the object list
     (push (list ,object ,location) *object-locations*) ;Add empty alist to object locations list
    )
   )
)
;;;Function new-location
;;;Will add a new location to the game
;;;Error checks to make sure the location does not exist
(defmacro new-location (location &optional description)
  `(cond
    ;;Error check, make sure location exists
    ((assoc ,location *nodes*)
     (format T "Error in new-location: location already exists~%"))
    (T
     (push (list ,location ,description) *nodes*) ;Add to list of locations
     (push (list ,location) *edges*) ;Add an empty list of paths to the location's paths
    )
   )
)

;;;Function new-path
;;;Will add a new path to the game. Can be uni or bidirectional
;;;Error checks to make sure the locations exist and the path does not
;;;If the path has only dir1 specified, it is unidirectional
(defmacro new-path (loc1 loc2 type dir1 &optional dir2)
  `(cond
    ;;Error check, make sure locs exist
    ((not (and (assoc ,loc1 *nodes*) (assoc ,loc2 *nodes*)))
     (format T "Error in new-path: one or more of the locations do not exist~%"))
    ((and ,dir1 ,dir2) ;Check to see if path is bidirectional
     ;;Create first path if not exists
     (unless (path-exists ,loc1 ,loc2)
       (nconc (assoc ,loc1 *edges*)
              (list (list ,loc2 ,dir1 ,type))))
     ;;Create second path if not exists
     (unless (path-exists ,loc2 ,loc1)
       (nconc (assoc ,loc2 *edges*)
	            (list (list ,loc1 ,dir2 ,type)))))
    (,dir1 ;Check to see if path in unidirectional
     ;;Create first path if not exists
     (unless (path-exists ,loc1 ,loc2)
       (nconc (assoc ,loc1 *edges*)
              (list (list ,loc2 ,dir1 ,type)))))
    (T (format T "Error in new-path: no direction specified"))))

;;Now that we have new macros, add my custom game elements
;;;Function load-custom-game
;;;Loads all custom items, locations, and paths
(defmacro load-custom-game ()
  `(progn
     (new-location 'basement '(you are in a dark basement.))
     (new-path 'basement 'living-room 'staircase 'upstairs 'downstairs)
))
(load-custom-game)

;;;Macro dungeon-game-action
;;;Creates a command <command>
;;;Checks to see if the room-list is empty
;;;If non-empty, executes body
(defmacro dungeon-game-action (command room-list &body body)
  `(progn (defun ,command ()
	    (cond
	     ((not *in-dungeon*) '(you cannot do this outside of the basement.))
	     ((endp ,room-list) '(this room is already cleared.))
	     (,room-list
	      ,@body)))
	  (pushnew ',command *allowed-commands*)))

;;;Creates a SPEL that allows fighing error monsters
(dungeon-game-action fight room-monsters
		     (cond
		      ((>= (random 100) *difficulty*) ;Random chance to win
		       (setq room-monsters ()) ;Clear room of monsters
		       (let ((drop (monster-drop-item))) ;Get dropped items
			 (append ;Build return message
			  '(you have debugged the error.)
			  '(you have received )
			  (if drop
			      (progn ;There is a drop
				(force-pickup drop) ;Add item to inventory
				(list drop))
			    '(nothing)) ;No drop
			  )))
		      (T
		       (decr-health 1)
		       '(you have been defeated by the error. you lose 1hp))))

;;;Creates a SPEL that allows disarming traps in the current room
(dungeon-game-action disarm room-traps
		     (cond
		      ((>= (random 100) *difficulty*)
                       (setq room-traps ())
		       '(you have disarmed the trap))
		      (T
		       (decr-health 1)
                       '(you make a mistake and are injured by the trap))))

;;;Creates a SPEL that allows the player to check their health
(simple-game-action health `(your health is currently ,*health*))

;;;Creates a SPEL that allows the player to cheat
;;;Gives all items to player, sets location to living-room
(simple-game-action cheat
		    (progn
		      (loop for object in *objects*
			    do (force-pickup object))
		      (setq *location* 'living-room)
		      (setq *in-dungeon* nil)
		      '(you have cheated. you have all items and have been returned to the living room)))
;;;Creates a SPEL that returns the player back to the first basement room
(simple-game-action backtrack
		    (progn
		      (setq location-x 50)
		      (setq location-y 50)
		      `(you have been returned to the first dungeon room.)))

;;;Creates a SPEL to interact with the wizard
(simple-game-action talk
		    (cond
		     ((not (eq *location* 'living-room)) ;Error check: wrong location
		      '(you must be in the living room to talk to the wizard))
		     (T ;Correct location
		      (append
		       '(you lean in close and the wizard whispers to you\:)
		       (cond
			((and *started-quest* (have-all-parts)) ;case: started-quest and completed it
			 (setq *victory* T)
			 '(you have everything i need! thank you. i will return you to where you came from now)
			 )
			(*started-quest* ;case: started quest, not complete
			 '(you do not have all the parts! Come back once you have gotten a gpu\, cpu\, hdd\, and ram.)
			 )
			(T ;case: quest not started, never talked to wizard before
			 (setq *started-quest* T)
			 (append
			  '(i have brought you here for one reason.)
			  '(i want a sweet new desktop but am far to weak to build one.)
			  '(you must venture into my basement and fight coding errors monsters.)
			  '(luckily i already have a power supply\, case\, and montherboard.)
			  '(Come back once you have gotten a gpu\, cpu\, hdd\, and ram.))))))))
;;Does not fit into existing macros
;;;Function consume
;;;Takes a consumable object as a parameter
;;;Remove the object from the inventory
;;;Increments *health* by a number between 0 and 5
(defun consume (object)
  (cond
   ((not (member object '(mountain-dew cheetos doritos))) ;Item cannot be consumed
    '(that item is not consumable.))
   ((have object) 
    ;Remove the object from the inventory
    (setq *object-locations*
	  (remove object *object-locations*
		  :test #'(lambda (x y) (eq (car y) x))))
    `(you have consumed the ,object .your health is now ,(incf *health* (random 5)) hp)
    )
   (T ;Item is not in inventory, but is consumable
    '(you do not have that item))))
(pushnew 'consume *allowed-commands*)