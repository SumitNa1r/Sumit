;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require rackunit)
(require rackunit/text-ui)
(define l 200)
(define h 400)
(define radius 15)
(define no "north")
(define ea "east")
(define we "west")
(define so "south")

;;Data Definition
(define-struct robot (x y dir))
;;A robot is a (make-robot Number Number string))
;;x and y are the co-ordinates of the centre of the robot, in pixels
;;direction will tell the direction where the robot is facing

;;TEMPLATE:
;;robot-fn : robot -> ??
;(define robot-fn r
;  (...
;     (robot-x r)
;     (robot-y r)
;     (robot-dir r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;initial-robot : Number Number -> robot
;;GIVEN: Set of x and y coordinates
;;RETURNS: robot with its centre at the given location and facing north
;;(initial-robot 30 30) = robot with its centre at 30 30 facing north
;;(initial-robot 1000 1000)= robot with its centre at the centre 1000 1000
;;facing north

(define (initial-robot rx ry)
  (make-robot rx ry no)) 

;;Tests
;;test cases will include conditions where the robot is initialized inside the 
;;given area, outside the given area and on the border

(define-test-suite initial-robot-tests
  (check-equal? (initial-robot 50 50) (make-robot 50 50 no) )
  (check-equal? (initial-robot 0 0) (make-robot 0 0 no))
  (check-equal? (initial-robot 500 500) (make-robot 500 500 no)))
(run-tests initial-robot-tests)

;;robot-left: robot -> robot
;;GIVEN: robot
;;RETURNS: a robot like the original, but turned 90 degrees left
;;robot-left (make-robot(50 50 "north")) -> (make-robot(50 50 "west"))
;;robot-left (make-robot(50 50 "west")) -> (make-robot(50 50 "south"))
;;robot-left (make-robot(50 50 "east")) -> (make-robot(50 50 "north"))
;;robot-left (make-robot(50 50 "south")) -> (make-robot(50 50 "east"))
;;STRATEGY: Structural Decomposition on robot


(define (robot-left r)
  (cond    
    [(string=? (robot-dir r) no)
        (make-robot (robot-x r) (robot-y r) we)]
    [(string=? (robot-dir r) we)
        (make-robot (robot-x r) (robot-y r) so)]
    [(string=? (robot-dir r) so)
        (make-robot (robot-x r) (robot-y r) ea)]
    [(string=? (robot-dir r) ea)
        (make-robot (robot-x r) (robot-y r) no)]))

;;Tests
;;It will cover cases for all directions
(define-test-suite robot-left-tests
  (check-equal? (robot-left (make-robot 50 50 no)) (make-robot 50 50 we))
  (check-equal? (robot-left (make-robot 50 50 we)) (make-robot 50 50 so))
  (check-equal? (robot-left (make-robot 50 50 ea)) (make-robot 50 50 no))
  (check-equal? (robot-left (make-robot 50 50 so)) (make-robot 50 50 ea)))
(run-tests robot-left-tests)


;;robot-right: robot -> robot
;;GIVEN: robot
;;RETURNS: a robot like the original, but turned 90 degrees right
;;robot-right (make-robot(50 50 "north")) -> (make-robot(50 50 "east"))
;;robot-right (make-robot(50 50 "east")) -> (make-robot(50 50 "south"))
;;robot-right (make-robot(50 50 "south")) -> (make-robot(50 50 "west"))
;;robot-right (make-robot(50 50 "west")) -> (make-robot(50 50 "north"))
;;STRATEGY: Structural Decomposition on robot


(define (robot-right r)
  (cond    
    [(string=? (robot-dir r) no)
        (make-robot (robot-x r) (robot-y r) ea)]
    [(string=? (robot-dir r) we)
        (make-robot (robot-x r) (robot-y r) no)]
    [(string=? (robot-dir r) so)
        (make-robot (robot-x r) (robot-y r) we)]
    [(string=? (robot-dir r) ea)
        (make-robot (robot-x r) (robot-y r) so)]))

;;Tests
;;It will cover cases for all directions
(define-test-suite robot-right-tests
  (check-equal? (robot-right (make-robot 50 50 no)) (make-robot 50 50 ea))
  (check-equal? (robot-right (make-robot 50 50 we)) (make-robot 50 50 no))
  (check-equal? (robot-right (make-robot 50 50 ea)) (make-robot 50 50 so))
  (check-equal? (robot-right (make-robot 50 50 so)) (make-robot 50 50 we)))
(run-tests robot-right-tests)

;;robot-west? : Robot -> Boolean
;;GIVEN: a robot
;;ANSWERS: whether the robot is facing in the specified direction.
;;robot-west(make-robot(50 50 "north")) -> true
;;robot-west(make-robot(50 50 "east")) -> false
;;robot-west(make-robot(50 50 "west")) -> false
;;robot-west(make-robot(50 50 "south")) -> false
;;STRATEGY: Structural Decomposition on robot

(define (robot-west r)
( string=? (robot-dir r) we))

;;test
;;;;It will cover cases for all directions
(define-test-suite robot-west?-tests
  
 (check-equal? (robot-west(make-robot 50 50 so)) false)
 (check-equal? (robot-west(make-robot 50 50 we)) true)
 (check-equal? (robot-west(make-robot 50 50 ea)) false)
 (check-equal? (robot-west(make-robot 50 50 no)) false))
(run-tests robot-west?-tests)
 
;;robot-east? : Robot -> Boolean
;;GIVEN: a robot
;;ANSWERS: whether the robot is facing in the specified direction.
;;robot-east(make-robot(50 50 "north")) -> false
;;robot-east(make-robot(50 50 "east")) -> true
;;robot-east(make-robot(50 50 "west")) -> false
;;robot-east(make-robot(50 50 "south")) -> false
;;STRATEGY: Structural Decomposition on robot

(define (robot-east r)
  ( string=? (robot-dir r) ea))

;;test
;;It will cover cases for all directions
(define-test-suite robot-east?-tests
 (check-equal? (robot-east(make-robot 50 50 so)) false)
 (check-equal? (robot-east(make-robot 50 50 we)) false)
 (check-equal? (robot-east(make-robot 50 50 ea)) true)
 (check-equal? (robot-east(make-robot 50 50 no)) false))
(run-tests robot-east?-tests)

;;robot-north? : Robot -> Boolean
;;GIVEN: a robot
;;ANSWERS: whether the robot is facing in the specified direction.
;;robot-north(make-robot(50 50 "north")) -> true
;;robot-north(make-robot(50 50 "east")) -> false
;;robot-north(make-robot(50 50 "west")) -> false
;;robot-north(make-robot(50 50 "south")) -> false
;;STRATEGY: Structural Decomposition on robot

(define (robot-north r)
  ( string=? (robot-dir r) no))

;;test
;;It will cover cases for all directions
(define-test-suite robot-north?-tests
 (check-equal? (robot-north(make-robot 50 50 so)) false)
 (check-equal? (robot-north(make-robot 50 50 we)) false)
 (check-equal? (robot-north(make-robot 50 50 ea)) false)
 (check-equal? (robot-north(make-robot 50 50 no)) true))
(run-tests robot-north?-tests)

;;robot-south? : Robot -> Boolean
;;GIVEN: a robot
;;ANSWERS: whether the robot is facing in the specified direction.
;;robot-south(make-robot(50 50 "north")) -> false
;;robot-south(make-robot(50 50 "east")) -> false
;;robot-south(make-robot(50 50 "west")) -> false
;;robot-south(make-robot(50 50 "south")) -> true
;;STRATEGY: Structural Decomposition on robot

(define (robot-south r)
  ( string=? (robot-dir r) so))

;;test
;;It will cover cases for all directions
(define-test-suite robot-south?-tests
 (check-equal? (robot-south(make-robot 50 50 so)) true)
 (check-equal? (robot-south(make-robot 50 50 we)) false)
 (check-equal? (robot-south(make-robot 50 50 ea)) false)
 (check-equal? (robot-south(make-robot 50 50 no)) false))
(run-tests robot-south?-tests)



;robot-forward : Robot PosInt -> Robot
;GIVEN: a robot and a distance
;RETURNS: a robot like the given one, but moved forward by the
;specified number of pixels.  If moving forward the specified number of
;pixels would cause the robot to run into a wall, the robot should stop
;at the wall.
;Example;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (robot-forward r dist)
(cond
  [( string=? (robot-dir r) no ) 
   (cond
     [(and (and (<= (- (robot-y r) dist) radius) ( and( >= (robot-x r)  radius ) ( <= (robot-x r) (- l radius)))) (>= (robot-y r) radius))  (make-robot (robot-x r) radius no)]
        [else (make-robot (robot-x r) (- (robot-y r) dist) no)])]
  
  [( string=? (robot-dir r) so ) 
   (cond
     [(and (and (>= (+ (robot-y r) dist) (- h radius)) ( and( >= (robot-x r) radius) ( <= (robot-x r) (- l radius)))) (<= (robot-y r) (- h radius))) (make-robot (robot-x r) (- h radius) so)]
        [else (make-robot (robot-x r) (+ (robot-y r) dist) so)])]
  
  [( string=? (robot-dir r) ea ) 
   (cond
     [(and (and (>= (+ (robot-x r) dist) (- l radius)) ( and( >= (robot-y r) radius) ( <= (robot-y r) (- h radius)))) (<= (robot-x r) (- l radius))) (make-robot (- l radius) (robot-y r) ea)]
        [else (make-robot  (+(robot-x r) dist)(robot-y r) ea)])]
  
  [( string=? (robot-dir r) we ) 
   (cond
     [(and (and (<= (- (robot-x r) dist) radius) ( and( >= (robot-y r)  radius ) ( <= (robot-y r) (- h radius)))) (>= (robot-x r) radius))  (make-robot radius (robot-y r) we)]
        [else (make-robot (- (robot-x r) dist) (robot-y r)  we)])]
  
)
)
;;Tests
(define-test-suite robot-forward-tests
;;teste for moving forward while facing north
(check-equal? (robot-forward (make-robot 15 15 "north") 700) (make-robot 15 15 "north"))
(check-equal? (robot-forward (make-robot 50 15 "north") 70) (make-robot 50 15 "north"))
(check-equal? (robot-forward (make-robot 300 15 "north") 700) (make-robot 300 -685 "north"))
(check-equal? (robot-forward (make-robot -300 15 "north") 700) (make-robot -300 -685 "north"))
(check-equal? (robot-forward (make-robot 15 -15 "north") 700) (make-robot 15 -715 "north"))
(check-equal? (robot-forward (make-robot 15 500 "north") 700) (make-robot 15 15 "north"))
;;teste for moving forward while facing west 
(check-equal? (robot-forward (make-robot 15 15 "west") 700) (make-robot 15 15 "west"))
(check-equal? (robot-forward (make-robot 15 14 "west") 700) (make-robot -685 14 "west")) 
(check-equal? (robot-forward (make-robot 15 50 "west") 700) (make-robot 15 50 "west")) 
(check-equal? (robot-forward (make-robot -15 15 "west") 700) (make-robot -715 15 "west")) 
(check-equal? (robot-forward (make-robot 300 15 "west") 400) (make-robot 15 15 "west")) 
(check-equal? (robot-forward (make-robot 15 700 "west") 700) (make-robot -685 700 "west"))
;;teste for moving forward while facing south
(check-equal? (robot-forward (make-robot 15 15 "south") 700) (make-robot 15 385 "south"))
(check-equal? (robot-forward (make-robot 15 150 "south") 25) (make-robot 15 175 "south"))
(check-equal? (robot-forward (make-robot 15 150 "south") 700) (make-robot 15 385 "south"))
(check-equal? (robot-forward (make-robot 1500 15 "south") 700) (make-robot 1500 715 "south"))
(check-equal? (robot-forward (make-robot -900 50 "south") 700) (make-robot -900 750 "south"))
(check-equal? (robot-forward (make-robot 50 500 "south") 700) (make-robot 50 1200 "south"))
(check-equal? (robot-forward (make-robot 15 -1500 "south") 1500) (make-robot 15 0 "south"))
;;test for moving forward while facing east
(check-equal? (robot-forward (make-robot 15 15 "east") 700) (make-robot 185 15 "east"))
(check-equal? (robot-forward (make-robot 15 700 "east") 700) (make-robot 715 700 "east"))
(check-equal? (robot-forward (make-robot 15 -15 "east") 700) (make-robot 715 -15 "east"))
(check-equal? (robot-forward (make-robot 50 50 "east") 700) (make-robot 185 50 "east"))
(check-equal? (robot-forward (make-robot -15 55 "east") 700) (make-robot 185 55 "east"))
(check-equal? (robot-forward (make-robot 15 13 "east") 700) (make-robot 715 13 "east"))
(check-equal? (robot-forward (make-robot 10 0 "east") 700) (make-robot 710 0 "east")))
(run-tests robot-forward-tests)
