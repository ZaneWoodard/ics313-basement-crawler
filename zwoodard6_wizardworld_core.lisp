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
;;;Function path-exists
;;;Checks to see if a path from loc1 to loc2 exists
(defun path-exists (loc1 loc2)
  (find-if #'(lambda (x) (eq (car x) loc2))
           (cdr (assoc loc1 *edges*))))


;;;Function inc-health
;;;Increases the player's health by x
;;;Returns the current health
(defun inc-health (x)
  (game-print `(your health is now ,(+ *health* x)))
  (setq *health* (+ *health* x)))

;;;Function decr-health
;;;Decreases the player's health by x
;;;Returns the current health
(defun decr-health (x)
  (game-print `(your health is now ,(- *health* x)))
  (setq *health* (- *health* x))
  (when (<= *health* 0)
    (game-print '(you have died before you could assemble the computer!))
    (quit))
  *health*)
