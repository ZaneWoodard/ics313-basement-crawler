;;Game state variables
(defparameter *computer-assembled* nil)
(defparameter *in-dungeon* nil)
(defparameter *allowed-commands* '(look walk pickup inventory))
(defparameter *objects* '(whiskey bucket frog chain))
(defparameter *object-locations* '((whiskey living-room)
                                   (bucket living-room)
                                   (chain garden)))
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
;;COMPUTER VARIABLES
(defparameter *assembled-parts* '())
(defparameter *valid-ram* ())
(defparameter *valid-gpu* ())
(defparameter *valid-cpu* ())
(defparameter *valid-mobo* ())
(defparameter *valid-psu* ())
(defparameter *valid-case* ())
(defparameter *valid-parts* (list
			     (list 'ram *valid-ram*)
			     (list 'gpu *valid-gpu*)
			     (list 'cpu *valid-cpu*)
			     (list 'mobo *valid-mobo*)
			     (list 'psu *valid-psu*)
			     (list 'case *valid-case*)
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

;;Define the help content
(defparameter *help* '((***WIZARDS WORLD HELP***)))
(progn
  (nconc *help* '((The goal of this game is to assemble a computer.)))
  (nconc *help* '((Explore rooms\, pickup objects\, and use them to progress)))
  (nconc *help* '((Here is a list of commands to help you do this\:)))
  (nconc *help*
	  (mapcar #'(lambda (x) (list '--> x)) *allowed-commands*))
)
  

;;;Function simple-game-action
;;;For game actions with no objects or location requirements
(defmacro simple-game-action (command &body body)
  `(progn (defun ,command ()
	        ,@body
		  )
	    (pushnew ',command *allowed-commands*)))

;;Define the main help function, prints contents of *help*
(simple-game-action help
		        (loop for line on *help* do (game-print (car line))))
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
     (new-object 'book 'living-room)
     (new-path 'basement 'living-room 'staircase 'upstairs 'downstairs)
;     (new-path 'basement 'living-room 'ladder 'upstairs 'downstairs)))
))
(load-custom-game)
(game-action assemble case motherboard living-room
	     (cond
	      ;;Cannot assemble it twice check
	      (*computer-assembled* '(you have already assembled the computer))
	      ;;Check for all the necessary components
	      ((not (have 'ram)) '(you do not have ram))
	      ((not (have 'cpu)) '(you do not have a cpu))
	      ((not (have 'gpu)) '(you do not have a gpu))
	      ((not (have 'psu)) '(you do not have a psu))
	      ;;Assemble the computer
	      (T
	       (new-object 'computer *location*)
	       (pickup 'computer)
	       (remove-all-from-inven '(ram cpu gpu psu case motherboard))
	       (setf *computer-assembled* 't)
	       '(you have assembled the computer))))