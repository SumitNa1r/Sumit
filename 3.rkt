;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require rackunit)
(require rackunit/text-ui)

(define t "true")
(define f "false")

;;Data Definition
(define-struct diff-exp (rand1 rand2))
;;A diff-exp is a (make-diff-exp Number Number)
;;rand1 is the first operand
;;rand2 is the second operand

;TEMPLATE
;diff-exp-fn : diff-exp -> ??
;(define diff-exp-fn diff
;  (...
;    (diff-exp-rand1 diff)
;    (diff-exp-rand1 diff)))


(define-struct mult-exp (rand1 rand2))
;;A mult-exp is a (make-mult-exp Number Number)
;;rand1 is the first operand
;;rand2 is the second operand

;TEMPLATE
;mult-exp-fn : mult-exp -> ??
;(define mult-exp-fn mult
;  (...
;    (mult-exp-rand1 mult)
;    (mult-exp-rand1 mult)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expr-to-image expr bool)
  (cond
    [(diff-exp? expr) (
                       cond
                        [(boolean? bool) (beside 
                                            (text "(" 11 "black" )
                                            (text (number->string (diff-exp-rand1 expr)) 11 "black" )
                                            (text " " 11 "black" )
                                            (text " - " 11 "black" )
                                            (text (number->string (diff-exp-rand2 expr)) 11 "black" )
                                            (text ")" 11 "black" ))]
                        
                        [(boolean? bool) (beside 
                                            (text "( " 11 "black" )
                                            (text "-" 11 "black" )
                                            (text " " 11 "black" )
                                            (text (number->string (diff-exp-rand1 expr)) 11 "black" )
                                            (text " " 11 "black" )
                                            (text (number->string (diff-exp-rand2 expr)) 11 "black" )
                                            (text ")" 11 "black" ))])]
    [(mult-exp? expr) (
                       cond
                        [(boolean? bool ) (beside 
                                            (text "(" 11 "black" )
                                            (text (number->string (mult-exp-rand1 expr)) 11 "black" )
                                            (text " " 11 "black" )
                                            (text " * " 11 "black" )
                                            (text (number->string (mult-exp-rand2 expr)) 11 "black" )
                                            (text ")" 11 "black" ))]
                        
                        [(boolean? bool) (beside 
                                            (text "( " 11 "black" )
                                            (text "*" 11 "black" )
                                            (text " " 11 "black" )
                                            (text (number->string (mult-exp-rand1 expr)) 11 "black" )
                                            (text " " 11 "black" )
                                            (text (number->string (mult-exp-rand2 expr)) 11 "black" )
                                            (text ")" 11 "black" ))])]
                                            
))
  
  (expr-to-image (make-mult-exp 3 4) true)