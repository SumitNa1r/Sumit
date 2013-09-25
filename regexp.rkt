;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname regexp) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")))))
(require rackunit)
(require rackunit/text-ui)
(define start "start")
(define s0 "s0")
(define s1 "s1")
(define s2 "s2")
(define s3 "s3")
(define err "error")

;A state is one of the following
;-- start   interp: The initial state of finite state machine
;-- s0      interp: First state of finite state machine
;-- s1      interp: second state of finite state machine
;-- s2      interp: Third state of finite state machine
;-- s3      interp: Fourth state of finite state machine
;                   It also represents the acceptance state
;-- error      interp: Error state of finite state machine
;state-fn : s -> ??
;(define(state-fn s)
;  (cond
;    [(string=? s "start")
;     ...]
;    [(string=? s "s0")
;     ...]
;    [(string=? s "s1")
;     ...]
;    [(string=? s "s2")
;     ...]
;    [(string=? s "s3")
;     ...]
;    [(string=? s "error")
;     ...]))
;A mykeyevent input is one of the following
;a        intrep: Key event a
;b        intrep: Key event b
;c        intrep: Key event c
;d        intrep: Key event d
;e        intrep: Key event e
;others   intrep: Any other key
;mykeyevent-fn : mke -> ??
;(define (acceptable-fn af)
;  (cond
;    [(string=? mke "a")
;     ...]
;    [(string=? mke "b")
;     ...]
;    [(string=? mke "c")
;     ...]
;    [(string=? mke "d")
;     ...]
;    [(string=? mke "e")
;     ...]
;    [else 
;      ...]))
;initial-state : Number -> State
;GIVEN: a number
;RETURNS: a representation of the initial state
;of your machine.  The given number is ignored.
;EXAMPLES:
;(initial-state 3) -> "start"
;DESIGN STRATEGY: Domain Knowledge
(define (initial-state num)
  "start")
;TEST:
(define-test-suite initial-state-test
  (check-equal? (initial-state 3) "start")
  (check-equal? (initial-state -3) "start"))
(run-tests initial-state-test)
;next-state : State mykeyevent -> State
;GIVEN: a state of the machine and a key event
;RETURNS: the state that should follow the given key event.  A key
;event that is to be discarded should leave the state unchanged.
;EXAMPLE
;
;DESIGN STRATEGY
(define (next-state st key)
  (cond
    [(string=? st "start")
     (start-nextstate key)]
    [(string=? st "s0")
     (s0-nextstate key)]
    [(string=? st "s1")
     (s1-nextstate key)]
    [(string=? st "s2")
     (s2-nextstate key)]
    [(string=? st "s3")
     (s3-nextstate key)]
    [(string=? st "error")
      (error-nextstate key)]))
;TESTS: Written below
;checkkeylength : state mykeyevent -> state
;GIVEN: a key event and a state
;RESULT:the state that should follow the given key event.  A key
;event that is to be discarded should leave the state unchanged.
;EXAMPLE;
;DESIGN STRATEGY
(define (checkkeylength st key)
  (if (> (string-length key) 1) st err))
;TESTS
(define-test-suite checkkeylength-test
  (check-equal? (checkkeylength s1 "aadd") s1)
  (check-equal? (checkkeylength s0 "g") err))
(run-tests checkkeylength-test)

;start-nextstate : mykeyevent -> State
;GIVEN: a key event
;RETURNS: the state that should follow the given key event.  A key
;event that is to be discarded should leave the state unchanged.
;EXAMPLE
;
;DESIGN STRATEGY
(define (start-nextstate key)
  (cond
    [(string=? key "a")
     "s0"]
    [(string=? key "b")
     "s0"]
    [(string=? key "c")
     "s1"]
    [(string=? key "d")
     "error"]
    [(string=? key "e")
     "error"]
    [else 
      (checkkeylength start key)]))
;TESTS: Written below
;s0-nextstate : mykeyevent -> State
;GIVEN: a key event
;RETURNS: the state that should follow the given key event.  A key
;event that is to be discarded should leave the state unchanged.
;EXAMPLE
;
;DESIGN STRATEGY
(define (s0-nextstate key)
  (cond
    [(string=? key "a")
     "s0"]
    [(string=? key "b")
     "s0"]
    [(string=? key "c")
     "s1"]
    [(string=? key "d")
     "error"]
    [(string=? key "e")
     "error"]
    [else 
      (checkkeylength s0 key)]))
;TESTS: Written below
;s1-nextstate : mykeyevent -> State
;GIVEN: a key event
;RETURNS: the state that should follow the given key event.  A key
;event that is to be discarded should leave the state unchanged.
;EXAMPLE
;
;DESIGN STRATEGY
(define (s1-nextstate key)
  (cond
    [(string=? key "a")
     "error"]
    [(string=? key "b")
     "error"]
    [(string=? key "c")
     "error"]
    [(string=? key "d")
     "s2"]
    [(string=? key "e")
     "s3"]
    [else 
      (checkkeylength s1 key)]))
;TESTS: Written below
;s2-nextstate : mykeyevent -> State
;GIVEN: a key event
;RETURNS: the state that should follow the given key event.  A key
;event that is to be discarded should leave the state unchanged.
;EXAMPLE
;
;DESIGN STRATEGY
(define (s2-nextstate key)
  (cond
    [(string=? key "a")
     "error"]
    [(string=? key "b")
     "error"]
    [(string=? key "c")
     "error"]
    [(string=? key "d")
     "s2"]
    [(string=? key "e")
     "s3"]
    [else 
      (checkkeylength s2 key)]))
;TESTS: Written below
;s3-nextstate : mykeyevent -> State
;GIVEN: a key event
;RETURNS: the state that should follow the given key event.  A key
;event that is to be discarded should leave the state unchanged.
;EXAMPLE
;
;DESIGN STRATEGY
(define (s3-nextstate key)
  (cond
    [(string=? key "a")
     "error"]
    [(string=? key "b")
     "error"]
    [(string=? key "c")
     "error"]
    [(string=? key "d")
     "error"]
    [(string=? key "e")
     "error"]
    [else 
      (checkkeylength s3 key)]))
;TESTS: Written below
;error-nextstate : mykeyevent -> State
;GIVEN: a key event
;RETURNS: the state that should follow the given key event.  A key
;event that is to be discarded should leave the state unchanged.
;EXAMPLE
;
;DESIGN STRATEGY
(define (error-nextstate key)
  (cond
    [(string=? key "a")
     "error"]
    [(string=? key "b")
     "error"]
    [(string=? key "c")
     "error"]
    [(string=? key "d")
     "error"]
    [(string=? key "e")
     "error"]
    [else 
      (checkkeylength err key)]))
;TESTS: Written below

(define-test-suite next-state-test
  (check-equal? (next-state start "a") "s0")
  (check-equal? (next-state start "b") "s0")
  (check-equal? (next-state start "c") "s1")
  (check-equal? (next-state start "d") "error")
  (check-equal? (next-state start "e") "error")
  (check-equal? (next-state start "mmm") "start")
  (check-equal? (next-state start "ddddddd") "start")
  (check-equal? (next-state s0 "a") "s0")
  (check-equal? (next-state s0 "b") "s0")
  (check-equal? (next-state s0 "c") "s1")
  (check-equal? (next-state s0 "d") "error")
  (check-equal? (next-state s0 "e") "error")
  (check-equal? (next-state s0 "saas") "s0")
  (check-equal? (next-state s0 "sddddfdsfd") "s0")
  (check-equal? (next-state s0 "f") "error")
  (check-equal? (next-state s1 "d") "s2")
  (check-equal? (next-state s1 "a") "error")
  (check-equal? (next-state s1 "b") "error")
  (check-equal? (next-state s1 "c") "error")
  (check-equal? (next-state s1 "e") "s3")
  (check-equal? (next-state s1 "ddsddsds") "s1")
  (check-equal? (next-state s2 "ssssss") "s2")
  (check-equal? (next-state s2 "a") "error")
  (check-equal? (next-state s2 "b") "error")
  (check-equal? (next-state s2 "c") "error")
  (check-equal? (next-state s2 "e") "s3")
  (check-equal? (next-state s2 "d") "s2")
  (check-equal? (next-state s3 "a") "error")
  (check-equal? (next-state s3 "b") "error")
  (check-equal? (next-state s3 "c") "error")
  (check-equal? (next-state s3 "d") "error")
  (check-equal? (next-state s3 "e") "error")
  (check-equal? (next-state s3 "sddsds") "s3")
  (check-equal? (next-state err "a") "error")
  (check-equal? (next-state err "b") "error")
  (check-equal? (next-state err "c") "error")
  (check-equal? (next-state err "d") "error")
  (check-equal? (next-state err "e") "error")
  (check-equal? (next-state err "dddddd") "error"))
(run-tests next-state-test)

;accepting-state? : State -> Boolean
;GIVEN: a state of the machine
;RETURNS: true iff the given state is a final (accepting) state
;EXAMPLE:
;
;DESIGN STRATEGY :
(define (accepting-state s)
  (cond
    [(string=? s "s3") true]
    [else false]))
;TEST:
(define-test-suite accepting-state-test 
  (check-equal? (accepting-state "s3") true) 
  (check-equal? (accepting-state "s1") false))
(run-tests accepting-state-test)
;error-state? : State -> Boolean
;GIVEN: a state of the machine
;RETURNS: true iff the string seen so far does not match the specified
;regular expression and cannot possibly be extended to do so.
;EXAMPLE:
;DESIGN STRATEGY:
(define (error-state s)
  (cond
    [(string=? s "error") false]
    [else true]))
;TEST:
(define-test-suite error-state-test
  (check-equal? (error-state "s3") true)
  (check-equal? (error-state "error") false))
(run-tests error-state-test)



