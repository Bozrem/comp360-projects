#lang racket
;;; COMP 360 - Day 6 Practice Problems
;;; Local Bindings, Scope, and Avoiding Repeated Recursion

;;; ============================================================
;;; PROBLEM 1: Understanding let
;;; ============================================================

;;; Part A: Predict what each expression evaluates to, then test
;;; in DrRacket.

"lets test"
(let ((x 5) (y 3))
  (+ x y))
; Prediction:
; 8

(let ((x 10))
  (let ((x 20))
    x))
; Prediction:
; 20

(define z 100)
(let ((z 1))
  (+ z 5))
; Prediction:
; 6

(let ((a 4) (b 2) (c 3))
  (* a (+ b c)))
; Prediction:
; 24


;;; Part B: The following expression causes an error. Explain why,
;;; then fix it using let*.

; (let ((x 5) (y (+ x 1)))
;   (* x y))

; Why does this cause an error?
; Your explanation:
; let won't allow use of defined variables in subsequent definitions

; Fixed version using let*:
; Your code here:
(let* ((x 5) (y (+ x 1)))
  (* x y))


;;; ============================================================
;;; PROBLEM 2: Shadowing
;;; ============================================================

;;; Predict the value of each expression. Pay careful attention
;;; to which binding of each variable is in scope.
"shadow test"
(define x 10)
(define y 20)

(let ((x 1))
  (+ x y))
; Prediction:
; 21

(let ((x 1))
  (let ((y 2))
    (+ x y)))
; Prediction:
; 3

(let ((x 1) (y x))   ; Note: uses let, not let*
  (+ x y))
; Prediction (assuming x is defined as 10 above):
; 11

(let* ((x 1) (y x))  ; Note: uses let*
  (+ x y))
; Prediction:
; 2

;;; ============================================================
;;; PROBLEM 3: Using let Inside Functions
;;; ============================================================

;;; Write a function `cylinder-volume` that takes the radius and
;;; height of a cylinder and returns its volume.
;;; Volume = pi * r^2 * h
;;;
;;; Use a let to define a local variable `pi` with value 3.14159.
;;;
;;; Examples:
;;;   (cylinder-volume 1 1)  => approximately 3.14159
;;;   (cylinder-volume 2 3)  => approximately 37.699 (3.14159 * 4 * 3)
;;;   (cylinder-volume 0 5)  => 0

; Your code here:
"cylinder  test"
(define (cylinder-volume r h)
  (let ((pi 3.14159))
    (* pi r r h)))

(cylinder-volume 1 1)
(cylinder-volume 2 3)
(cylinder-volume 0 5)

;;; Write a function `quadratic-roots-exist?` that takes three
;;; numbers a, b, and c (coefficients of ax^2 + bx + c) and returns
;;; #t if real roots exist, #f otherwise.
;;;
;;; Real roots exist when the discriminant (b^2 - 4ac) >= 0.
;;;
;;; Use a let to compute the discriminant once, then check it.
;;;
;;; Examples:
;;;   (quadratic-roots-exist? 1 0 -1)  => #t  (x^2 - 1 = 0 has roots)
;;;   (quadratic-roots-exist? 1 0 1)   => #f  (x^2 + 1 = 0 has no real roots)
;;;   (quadratic-roots-exist? 1 -5 6)  => #t  (x^2 - 5x + 6 = 0 has roots)

; Your code here:
(define (quadratic-roots-exist? a b c)
  (let ((determinant (- (* b b) (* 4 a c))))
    (if (>= determinant 0)
        #t
        #f)))

(quadratic-roots-exist? 1 0 -1)
(quadratic-roots-exist? 1 0 1)
(quadratic-roots-exist? 1 -5 6)

;;; ============================================================
;;; PROBLEM 4: Internal Defines (Helper Functions)
;;; ============================================================

;;; Write a function `distance` that takes two points (each a pair
;;; of x,y coordinates) and returns the Euclidean distance between them.
;;; Distance = sqrt((x2-x1)^2 + (y2-y1)^2)
;;;
;;; Use internal defines to create helper functions:
;;;   - (get-x pt) returns the x coordinate of a point
;;;   - (get-y pt) returns the y coordinate of a point
;;;   - (square n) returns n squared
;;;
;;; Examples:
;;;   (distance (cons 0 0) (cons 3 4))  => 5
;;;   (distance (cons 1 1) (cons 1 1))  => 0
;;;   (distance (cons 0 0) (cons 1 1))  => approximately 1.414

; Your code here:
(define (distance p1 p2)
  (define (get-x pt) (car pt))
  (define (get-y pt) (cdr pt))
  (define (square n) (* n n))
  ; Now write the body using these helpers:
  (sqrt (+ (square (- (get-x p1) (get-x p2))) (square (- (get-y p1) (get-y p2))))))

"distance test"
(distance (cons 0 0) (cons 3 4))
(distance (cons 1 1) (cons 1 1))
(distance (cons 0 0) (cons 1 1))


;;; ============================================================
;;; PROBLEM 5: Avoiding Repeated Recursion
;;; ============================================================

;;; The following function finds the largest element in a
;;; non-empty list of numbers. However, it has a SERIOUS efficiency
;;; problem - it calls (find-max lst) multiple times!

(define (find-max lst)
  (cond
    ((null? (cdr lst)) (car lst))
    ((> (car lst) (find-max (cdr lst))) (car lst))
    (else (find-max (cdr lst)))))

;;; Part A: Explain why find-max is inefficient.
;;; How many times might find-max be called for a list of length n?
; Your explanation:


;;; Part B: Rewrite find-max to be efficient using let.
;;; (This is the good-max example from the slides. Try to write it without reference.)
(define (good-max lst)
  (cond ((null? (cdr lst)) (car lst))
        (else
         (let ((max-rest (good-max (cdr lst))))
           (cond ((> (car lst) max-rest) (car lst))
                 (else max-rest))))))
    

; Test cases:
"good-max test"
(good-max '(1 2 3 4 5))
(good-max '(5 4 3 2 1))
(good-max '(3 1 4 1 5 9)) 


;;; ============================================================
;;; PROBLEM 6: Combining let with Recursion
;;; ============================================================

;;; Write a function `running-sum` that takes a list of numbers and
;;; returns a list where each element is the sum of all elements
;;; up to and including that position.
;;;
;;; Examples:
;;;   (running-sum '(1 2 3 4))    => '(1 3 6 10)
;;;   (running-sum '(5 5 5))      => '(5 10 15)
;;;   (running-sum '(1))          => '(1)
;;;   (running-sum '())           => '()
;;;
;;; Hint: Write a helper function that takes an additional
;;; "accumulator" parameter tracking the sum so far.
;;; Use an internal define for the helper.

; Your code here:


;;; ============================================================
;;; PROBLEM 7: Scope Challenge
;;; ============================================================

;;; Without running this code, predict what (mystery 5) returns.
;;; Then verify your answer in DrRacket.

(define (mystery n)
  (define (helper x)
    (+ x n))  ; Can helper see n? Why or why not?
  (let ((n 100))
    (helper 1)))

; What does (mystery 5) return?
; Your prediction:

; Explanation (which binding of n does helper use?):


;;; ============================================================
;;; PROBLEM 8: Practical Application
;;; ============================================================

;;; A "transaction" is a pair: (cons type amount)
;;; where type is either 'deposit or 'withdrawal
;;;
;;; Write a function `process-transactions` that takes a starting
;;; balance and a list of transactions, and returns the final balance.
;;;
;;; Use let to extract the type and amount from each transaction
;;; to make your code more readable.
;;;
;;; Examples:
;;;   (process-transactions 100 '())  => 100
;;;   (process-transactions 100 (list (cons 'deposit 50)))  => 150
;;;   (process-transactions 100 (list (cons 'withdrawal 30)))  => 70
;;;   (process-transactions 100 (list (cons 'deposit 50)
;;;                                   (cons 'withdrawal 30)
;;;                                   (cons 'deposit 20)))  => 140

; Your code here:
(define (process-transactions balance transactions)
  (cond
    ((null? transactions) balance)
    (else
     (let ((type 'replace-me)      ; extract type from first transaction
           (amount 'replace-me))   ; extract amount from first transaction
       ; Your code: check type and recursively process remaining
       'replace-me))))
