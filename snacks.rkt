;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require rackunit)
(require rackunit/text-ui)
(define co "chocolate")
(define ca "carrots")
(define re "release")
(define cocost 175)
(define cacost 70)


;;Data Definition:
(define-struct Machine (ChocoBars CarrotSticks Amount TempAmount Balance))
;;A Machine is a (make-Machine(Number Number Number))
;;Interp
;;ChocoBars is number of chocolate bars in the machine
;;CarrotBars is number of carrot bars in the machine
;;Amount is amount of money in the bank
;;TempAmount is amount inserted into the machine before slecting a product
;;Balance is amount that has to be returned to the customer after successful purchase 

;TEMPLATE:
;Machine-Fn : Machine -> ??
;(define (Machine-fn m)
;  (...
;    (Machine-ChocoBars m)
;    (Machine-CarrotSticks m)
;    (Machine-Amount m)
;    (Machine-TempAmount m)
;    (Machine-Balance m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;initial-machine : Number Number -> Machine
;GIVEN: the number of chocolate bars and the number of packages of
;carrot sticks
;RETURNS: a machine loaded with the given number of chocolate bars and
;carrot sticks, with an empty bank.
;EXAMPLES:
;(initial-machine 10 10) -> (make-Machine 10 10 0 0 0)
;(initial-machine 20 12) -> (make-Machine 20 20 0 0 0)
;;DESIGN STRATEGY: Domain Knowledge

(define (initial-Machine co ca)
 (make-Machine 10 10 0 0 0))

(initial-Machine 20 20)


;machine-chocolates : Machine -> Number
;GIVEN: a machine state
;RETURNS: the number of chocolate bars left in the machine
;EXAMPLES:
;(machine-chocolates (make-Machine 20 30 25 0 0) -> 20
;(machine-chocolates (make-Machine 50 30 25 0 0) -> 50
;DESIGN STRATEGY: Structural Decomposition of machine

(define (machine-chocolates m )
 (Machine-ChocoBars m))

(machine-chocolates (make-Machine 20 30 25 0 0))


;machine-carrots : Machine -> Number
;GIVEN: a machine state
;RETURNS: the number of carrots Sticks left in the machine
;EXAMPLES:
;(machine-carrots (make-Machine 20 30 25 0 0) -> 20
;(machine-carrots (make-Machine 50 30 25 0 0) -> 50
;DESIGN STRATEGY: Structual Decomposition of machine

(define (machine-carrots m )
 (Machine-CarrotSticks m))

(machine-carrots (make-Machine 20 30 25 0 0))


;machine-bank : Machine -> Number
;GIVEN: a machine state
;RETURNS: the amount of money in the machine's bank, in cents
;EXAMPLES:
;(machine-bank (make-Machine 20 30 25 0 0) -> 25
;(machine-bank (make-Machine 50 30 215 0 0) -> 215
;DESIGN STRATEGY: Structual Decomposition of machine

(define (machine-bank m )
 (Machine-Amount m))

(machine-bank (make-Machine 20 30 215 0 0))

;machine-next-state : Machine CustomerInput -> Machine
;GIVEN: a machine state and a customer input
;RETURNS: the state of the machine that should follow the customer's
;input
;EXAMPLES:
;(machine-next-state (make-Machine 1 1 50 10 0) re ) -> (make-Machine 1 1 50 0 10)
;(machine-next-state (make-Machine 1 1 50 10 0) 2 ) -> (make-Machine 1 1 52 10 0)
;(machine-next-state (make-Machine 1 1 0 80 0) ca ) -> (make-Machine 1 1 70 0 10)
;(machine-next-state (make-Machine 1 1 0 185 0) co ) -> (make-Machine 1 1 175 0 10)

;DESIGN STRATEGY: Structural Decomposition of machine

(define (machine-next-state m ci)
  (if (integer? ci) (make-Machine (Machine-ChocoBars m) (Machine-CarrotSticks m) (Machine-Amount m) (+ (Machine-TempAmount m) ci) (Machine-Balance m))
  
  (cond
    
    [(string=? ci co) (cond
                         [(and (> (Machine-ChocoBars m) 0) (> (Machine-TempAmount m) cocost)) (make-Machine (- (Machine-ChocoBars m) 1) (Machine-CarrotSticks m) (+ (Machine-Amount m) cocost)  0 (- (Machine-TempAmount m) cocost))]
                         [else  (make-Machine (Machine-ChocoBars m) (Machine-CarrotSticks m) (Machine-Amount m) (Machine-TempAmount m) (Machine-Balance m) )])]
    
    [(string=? ci ca) (cond
                         [(and (> (Machine-CarrotSticks m) 0) (> (Machine-TempAmount m) cacost)) (make-Machine (Machine-ChocoBars m) (- (Machine-CarrotSticks m) 1) (+ (Machine-Amount m) cacost)  0 (- (Machine-TempAmount m) cacost))]
                         [else  (make-Machine (Machine-ChocoBars m) (Machine-CarrotSticks m) (Machine-Amount m) (Machine-TempAmount m) (Machine-Balance m) )])]
    
    [(string=? ci re) (make-Machine (Machine-ChocoBars m) (Machine-CarrotSticks m) (Machine-Amount m) 0 (Machine-TempAmount m))]
    
    [else (make-Machine (Machine-ChocoBars m) (Machine-CarrotSticks m) (Machine-Amount m) (+ (Machine-TempAmount m) ci) (Machine-Balance m))]
    ))
)
(machine-next-state (make-Machine 1 1 50 10 0) re )



