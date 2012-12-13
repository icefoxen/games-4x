;;;; main.lisp
;;;; main file for this test game-like thing.

;(in-package :4x)
;(use-package :l-math)
;(eval-when (:compile :load :execute)
;  (ql:quickload :l-math))

(defconstant +zero-vec+ (lm:vector 0 0))

(defstruct star
  ;(name "some star" :type string)
  (loc +zero-vec+ :type lm:vector)
  (owner -1 :type number)
  (pop 0.0 :type float)
  (maxpop 0.0 :type float)
  (resources 1.0 :type float)
  (ships-in-orbit () :type list))

;;; Star production tasks can be:
;;; :growth --grows the population faster
;;; :taxes --gives empire money
;;; :ship --builds a ship of a certain type

(defun star-set-owner (star empire)
  (let ((emp-id (empire-id empire)))
    (setf (star-owner star) emp-id)
    (push star (empire-owned-stars empire))))

(defun star-production (star)
  (* (star-resources star) (star-pop star)))

;(defun star-grow-pop (star growth-rate)
;  ())


(defconstant +max-pop+ 100.0)
(defconstant +min-pop+ 25.0)
(defconstant +pop-range+ (- +max-pop+ +min-pop+))
(defconstant +min-resources+ 0.5)
(defconstant +max-resources+ 1.5)
(defconstant +resource-range+ (- +max-resources+ +min-resources+))

(defun make-random-star (xlim ylim)
  (let ((x (random xlim))
	(y (random ylim))
	(resources (+ +min-resources+ (random +resource-range+)))
	(maxpop    (+ +min-pop+ (random +pop-range+))))
    (make-star :loc (lm:vector x y) :owner -1 :pop 0.0 :maxpop maxpop :resources resources)))

(defstruct empire
  (name "invalid name" :type string)
  (id 0 :type number)
  (treasury 100 :type number)
  (ships () :type list)
  (owned-stars () :type list)
  (pop-growth-rate (+ 0.02 (random 0.1)) :type float))

(defstruct ship-class
  (name "" :type string)
  (warp-speed 1.0 :type float))

(defconstant +colony-ship+ (make-ship-class :name "Colony" :warp-speed 2.0))
(defconstant +scout-ship+ (make-ship-class :name "Scout" :warp-speed 5.0))

;;; Right now, valid ship classes are :colony and :scout
(defstruct ship
  (loc +zero-vec+ :type lm:vector)
  (in-orbit-of nil :type (or star null))
  (destination nil :type (or star null))
  (owner 0 :type number)  ; Empire ID
  (class +scout-ship+ :type ship-class))

(defun ship-warp-speed (ship)
  (ship-class-warp-speed (ship-class ship)))

(defun make-colony-ship (orbiting owner)
  (let ((ship 
	 (make-ship 
	  :owner owner 
	  :class +colony-ship+)))
    (ship-enter-orbit ship orbiting)
    ship))

(defun make-scout-ship (orbiting owner)
  (let ((ship 
	 (make-ship 
	  :owner owner 
	  :class +scout-ship+)))
    (ship-enter-orbit ship orbiting)
    ship))
  
(defun ship-enter-orbit (ship star)
  (let ((loc (star-loc star)))
    (setf (ship-loc ship) loc)
    (setf (ship-in-orbit-of ship) star)
    (push ship (star-ships-in-orbit star))))

(defun ship-leave-orbit (ship)
  (let ((star (ship-in-orbit-of ship)))
    (setf (ship-in-orbit-of ship) nil)
    (setf (star-ships-in-orbit star) (remove ship (star-ships-in-orbit star)))))

(defun ship-move-towards (ship dest-vec)
  "This assumes that dest-vec is out of reach of the ship's current movemnt;
otherwise it will overshoot."
  (let* ((speed (ship-warp-speed ship))
	 (loc (ship-loc ship))
	 (offset (lm:- dest-vec loc))
	 (distance (lm:length offset))
	 (dir-unit-vec (lm:/ offset distance))
	 (movement-vec (lm:* dir-unit-vec speed))
	 (new-pos (lm:+ loc movement-vec)))
    (setf (ship-loc ship) new-pos)))
    
;;; XXX: Sigh, fix this.
(defun ship-move (ship)
  (let ((dest-star (ship-destination ship)) ; ship-destination is a star
	(loc (ship-loc ship)))
    (when (not (null dest-star))
      (if (ship-in-orbit-of ship)
	  (ship-leave-orbit ship))
      ;; Actually move ship
      ;; Enter orbit of destination if we are close enough.
      (let ((distance (lm:length (lm:- (star-loc dest-star) loc)))
	    (speed (ship-warp-speed ship)))
	(if (< distance speed)
	    (ship-enter-orbit ship dest-star)
	    (ship-move-towards ship (star-loc dest-star)))))))




(defstruct universe
  (max-x 100.0 :type float)
  (max-y 100.0 :type float)
  (empires ())
  (starmap ()))
  



;;; LOOP <:-D
(defun make-starmap (xlim ylim numstars)
  (loop for i from 1 to numstars
       collect (make-random-star xlim ylim)))

;(defun do-turn-star (empire star)
;  (let ((growth-rate (empire-pop-growth-rate empire)))
;    (star-grow-pop star growth-rate)))
    


(defun do-turn (universe)
  ;(do-production)
  ;(do-research)
  ;(do-economy)
  ;(do-movement)
  ;(do-combat)
  ;(do-colonization)
  universe)


(defvar universe (make-starmap 100 100 100))
(defvar home (nth 3 universe))
(defvar me (make-empire))
