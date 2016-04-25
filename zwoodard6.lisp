;;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name: Zane Woodard                                         Date: 4-14-2016
;;;; Course: ICS313        Assignment: Assignment 6
;;;; File: zwoodard6.lisp

(load "zwoodard6_util.lisp")
(load "zwoodard6_wizardworld_core.lisp")
(load "zwoodard6_load.lisp")
(load "zwoodard6_inven.lisp")
(load "zwoodard6_basement.lisp")
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
	      (when (eq (car next) 'basement) ;Moving into basement
		(setq *in-dungeon* T)
		(build-room location-x location-y))
	      (if next 
		  (progn (setf *location* (car next)) 
			 (look))
		'(you cannot go that way.)))))))
;;;Function game-repl
;;;Enters the custom READ-EVAL-PRINT loop for the game
(defun game-repl ()
    (let ((cmd (game-read)))
      (unless (or *victory* (eq (car cmd) 'quit))
	(game-print (game-eval cmd))
	(game-repl))))

(progn
  (game-print (look))
  (game-repl)
)