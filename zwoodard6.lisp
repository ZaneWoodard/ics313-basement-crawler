;;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Zane Woodard                                         Date: 4-14-2016
;;;; Course: ICS313        Assignment: Assignment 6
;;;; File: zwoodard6.lisp

(load "zwoodard6_util.lisp")
(load "zwoodard6_load.lisp")
(load "zwoodard6_inven.lisp")
(load "zwoodard6_basement.lisp")
;;;Function describe-location
;;;Takes a location and a list of nodes as input
;;;Returns the description of a location according to the list of nodes
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))
;;;Function describe-path
;;;Takes an argument containing an edge
;;;Returns a description of the path described by the edge
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;;;Function describe-paths
;;;Takes a location to describe the paths of
;;;Returns descriptions of all paths associated with the location
;;;See describe-path for further info
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;;Function objects-at
;;;Takes in a location, a list of objects, and an alist of object locations
;;;Returns a list of all objects in the list of objects at location according to the object locations alist
(defun objects-at (loc objs obj-loc)
   (labels ((is-at (obj)
              (eq (cadr (assoc obj obj-loc)) loc)))
       (remove-if-not #'is-at objs)))

;;;Function describe-objects
;;;Takes in a location loc, list of objects objs, and an alist of object locations obj-loc
;;;Returns a description of all objects in objs at loc according to obj-loc
(defun describe-objects (loc objs obj-loc)
   (labels ((describe-obj (obj)
                `(you see a ,obj on the floor.)))
      (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;;;Function look
;;;Takes no arguments
;;;Returns a description of the current 'view' of the player
;;;The 'view' consists of a description of all objects, locations, and paths accessible at the current location
(defun look ()
  (if *in-dungeon*
      (dungeon-look)
      (append (describe-location *location* *nodes*)
	      (describe-paths *location* *edges*)
	      (describe-objects *location* *objects* *object-locations*))))
;;;Function path-exists
;;;Checks to see if a path from loc1 to loc2 exists
(defun path-exists (loc1 loc2)
  (find-if #'(lambda (x) (eq (car x) loc2))
	   (cdr (assoc loc1 *edges*))))
;;;Function walk
;;;Takes a direction as an argument
;;;Moves the player in that direction by updating the current location *location* if a path exists
(defun walk (direction)
  (cond
   (*in-dungeon*
    ;Handle with custom dungeon walking handler
    (dungeon-walk direction))
   (T
    (labels ((correct-way (edge)
			  (eq (cadr edge) direction)))
	    (let ((next (find-if #'correct-way (cdr (assoc *location* *edges*)))))
	      (if (eq (car next) 'basement) ;moveing into basement
		  (setq *in-dungeon* T))
	      (if next 
		  (progn (setf *location* (car next)) 
			 (look))
		'(you cannot go that way.)))))))
;;;Function game-repl
;;;Enters the custom READ-EVAL-PRINT loop for the game
(defun game-repl ()
    (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
            (game-print (game-eval cmd))
            (game-repl))))