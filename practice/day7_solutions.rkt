#lang racket
;;; COMP 360 - Day 7 Practice Problems
;;; Lambda Functions and Higher-Order Functions

;;; ============================================================
;;; PROBLEM 1: Lambda Basics
;;; ============================================================

;;; Part A: Rewrite each named function as an equivalent lambda expression.
;;; (Don't use define for the function itself, just write the lambda.)

; (define (add-one x) (+ x 1))
; Lambda version:
; (lambda (x) (+ x 1))

; (define (multiply a b) (* a b))
; Lambda version:
(lambda (x y) (* x y))


; (define (is-positive? n) (> n 0))
; Lambda version:
(lambda (n) (> n 0))

; (define (first-of-pair p) (car p))
; Lambda version:
(lambda (p) (car p))


;;; Part B: What does each of the following evaluate to?
"lambdas"
((lambda (x) (* x x)) 5)
; Prediction: 25

((lambda (x y) (+ x y)) 3 4)
; Prediction: 7

((lambda (f x) (f (f x))) add1 5) ; add1 is built-in, feel free to test it out!
; Prediction: 7

(let ((square (lambda (x) (* x x))))
  (square 6))
; Prediction: 36


;;; ============================================================
;;; PROBLEM 2: Higher-Order Functions - Passing Functions
;;; ============================================================

;;; Here is the sum-any function from class:
(define (sum-any func a b)
  (if (> a b)
      0
      (+ (func a)
         (sum-any func (+ a 1) b))))

;;; Use sum-any with lambda expressions to compute each of the following.
;;; Do NOT define named helper functions at the top level.
"sum-any"
;;; a. Sum of cubes from 1 to 5: 1^3 + 2^3 + 3^3 + 4^3 + 5^3 = 225
; Your code (should evaluate to 225):
(sum-any (lambda (x) (* x x x)) 1 5)

;;; b. Sum of (2n + 1) for n from 1 to 4: 3 + 5 + 7 + 9 = 24
; Your code (should evaluate to 24):
(sum-any (lambda (x) (+ (* 2 x) 1)) 1 4)

;;; c. Sum of 1/n for n from 1 to 5: 1 + 0.5 + 0.333... + 0.25 + 0.2
; Your code (should evaluate to approximately 2.28):
; multiply by 1.0 to decimalize the result
(* 1.0 (sum-any (lambda (x) (/ 1 x)) 1 5))

;;; ============================================================
;;; PROBLEM 3: Writing Higher-Order Functions
;;; ============================================================

;;; Write a function `apply-twice` that takes a function f and a value x,
;;; and returns f(f(x)).
;;;
;;; Examples:
;;;   (apply-twice add1 5)                    => 7
;;;   (apply-twice (lambda (x) (* x 2)) 3)    => 12  ; 3 -> 6 -> 12
;;;   (apply-twice (lambda (s) (string-append s "!")) "hi")  => "hi!!"

; Your code here:
(define (apply-twice f x) (f (f x)))

"apply-twice"
(apply-twice add1 5)
(apply-twice (lambda (x) (* x 2)) 3)
(apply-twice (lambda (s) (string-append s "!")) "hi")

;;; Write a function `apply-n-times` that takes a function f, a value x,
;;; and a number n, and applies f to x exactly n times.
;;;
;;; Use recursion. When n is 0, return x unchanged.
;;;
;;; Examples:
;;;   (apply-n-times add1 0 5)                => 5
;;;   (apply-n-times (lambda (x) (* x 2)) 1 4) => 16  ; 1 -> 2 -> 4 -> 8 -> 16
;;;   (apply-n-times add1 10 0)               => 10

; Your code here:
(define (apply-n-times f x n)
  (cond ((= n 0) x)
        (else (apply-n-times f (f x) (- n 1)))))

"apply-n-times"
(apply-n-times add1 0 5)
(apply-n-times (lambda (x) (* x 2)) 1 4)
(apply-n-times add1 10 0)


;;; ============================================================
;;; PROBLEM 4: transform-list (like map)
;;; ============================================================

;;; Write a function `transform-list` that takes a function and a list,
;;; and returns a new list with the function applied to each element.
;;;
;;; Examples:
;;;   (transform-list add1 '(1 2 3))           => '(2 3 4)
;;;   (transform-list (lambda (x) (* x x)) '(1 2 3 4))  => '(1 4 9 16)
;;;   (transform-list car '((1 2) (3 4) (5 6)))        => '(1 3 5)
;;;   (transform-list (lambda (x) x) '(a b c))         => '(a b c)
;;;   (transform-list add1 '())                => '()

; Your code here:
(define (transform-list f a)
  (cond ((null? a) '())
        (else (cons (f (car a)) (transform-list f (cdr a))))))

"transform-list"
(transform-list add1 '(1 2 3))
(transform-list (lambda (x) (* x x)) '(1 2 3 4))
(transform-list car '((1 2) (3 4) (5 6)))
(transform-list (lambda (x) x) '(a b c))
(transform-list add1 '())

;;; Now use transform-list to solve these problems.
;;; Use lambda expressions - do NOT use built-in or user-defined functions.

;;; a. Given a list of numbers, return a list of their absolute values.
;;;    Example: '(-1 2 -3 4) => '(1 2 3 4)
; (transform-list ??? '(-1 2 -3 4))
(transform-list (lambda (x) (if (< x 0) (- x) x)) '(-1 2 -3 4))

;;; b. Given a list of pairs, return a list of just the second elements.
;;;    Example: '((a . 1) (b . 2) (c . 3)) => '(1 2 3)
; (transform-list ??? '((a . 1) (b . 2) (c . 3)))
(transform-list (lambda (x) (cdr x)) '((a . 1) (b . 2) (c . 3))) 

;;; c. Given a list of numbers, return a list of booleans indicating
;;;    whether each number is even.
;;;    Example: '(1 2 3 4 5) => '(#f #t #f #t #f)
;;;    Hint: (even? n) returns #t if n is even
; (transform-list ??? '(1 2 3 4 5))
(transform-list (lambda (x) (if (= (remainder x 2) 0) #t #f)) '(1 2 3 4 5))

;;; ============================================================
;;; PROBLEM 5: keep-if (like filter)
;;; ============================================================

;;; Write a function `keep-if` that takes a predicate (a function
;;; returning #t or #f) and a list, and returns a new list containing
;;; only the elements for which the predicate returns #t.
;;;
;;; Examples:
;;;   (keep-if even? '(1 2 3 4 5 6))           => '(2 4 6)
;;;   (keep-if (lambda (x) (> x 3)) '(1 5 2 4 3))  => '(5 4)
;;;   (keep-if string? '(1 "hi" 2 "bye" 3))   => '("hi" "bye")
;;;   (keep-if even? '())                      => '()

; Your code here:
(define (keep-if f a)
  (cond ((null? a) '())
        ((f (car a)) (cons (car a) (keep-if f (cdr a))))
        (else (keep-if f (cdr a)))))

"keep-if"
(keep-if even? '(1 2 3 4 5 6))
(keep-if (lambda (x) (> x 3)) '(1 5 2 4 3))
(keep-if string? '(1 "hi" 2 "bye" 3))
(keep-if even? '())

;;; Use keep-if and lambda to solve these:

;;; a. Keep only positive numbers from a list.
;;;    Example: '(-1 2 -3 4 0 -5) => '(2 4)
; (keep-if ??? '(-1 2 -3 4 0 -5))
(keep-if (lambda (x) (> x 0)) '(-1 2 -3 4 0 -5))

;;; b. Keep only pairs where the cdr is greater than 10.
;;;    Example: '((a . 5) (b . 15) (c . 8) (d . 20)) => '((b . 15) (d . 20))
; (keep-if ??? '((a . 5) (b . 15) (c . 8) (d . 20)))
(keep-if (lambda (x) (> (cdr x) 10)) '((a . 5) (b . 15) (c . 8) (d . 20)))


;;; ============================================================
;;; PROBLEM 6: Internal Defines with Higher-Order Functions
;;; ============================================================

;;; Write a function `count-matching` that takes a predicate and a
;;; list, and returns the count of elements satisfying the predicate.
;;;
;;; Use an internal define to create a helper, NOT a top-level define.
;;;
;;; Examples:
;;;   (count-matching even? '(1 2 3 4 5 6))  => 3
;;;   (count-matching (lambda (x) (> x 0)) '(-1 2 -3 4))  => 2
;;;   (count-matching string? '(1 "a" 2 "b"))  => 2

; Your code here:
(define (count-matching pred lst)
  (define (helper remaining count)
    (cond ((null? remaining) count)
          ((pred (car remaining)) (helper (cdr remaining) (+ 1 count)))
          (else (helper (cdr remaining) count))))
  (helper lst 0))

(count-matching even? '(1 2 3 4 5 6))
(count-matching (lambda (x) (> x 0)) '(-1 2 -3 4))
(count-matching string? '(1 "a" 2 "b"))

;;; ============================================================
;;; PROBLEM 7: Putting It All Together
;;; ============================================================

;;; A "student record" is a list with three elements:
;;;   (list name major gpa)
;;; Example: (list 'alice 'cs 3.8)

(define students
  (list (list 'alice 'cs 3.8)
        (list 'bob 'math 2.9)
        (list 'carol 'cs 3.5)
        (list 'dan 'physics 3.2)
        (list 'eve 'cs 3.9)
        (list 'jace 'cs 2.2)))

;;; Part A: Using internal defines, write helper functions to extract
;;; each field from a student record, then use them in the main function.
;;;
;;; Write `cs-honors-students` that takes a list of student records
;;; and returns a list of NAMES of CS majors with GPA >= 3.5.
;;;
;;; Example:
;;;   (cs-honors-students students)  => '(alice carol eve)
;;;
;;; Structure your solution like this:

(define (cs-honors-students records)
  (define (get-name record) (car record))
  (define (get-major record) (cadr record))  ; Hint: use cadr
  (define (get-gpa record) (caddr record))    ; Hint: use caddr
  (define (cs-honors? record)
    ; returns #t if record is a CS major with gpa >= 3.5
    (and (eq? (get-major record) 'cs) (>= (get-gpa record) 3.5)))
  ; Now use your helpers to filter and extract names
  ; Hint: You might want to filter first, then transform
  (let ((cs-honors (keep-if cs-honors? records)))
    (transform-list get-name cs-honors)))
(cs-honors-students students)

;;; Part B: Write `average-gpa-by-major` that takes a major (symbol)
;;; and a list of student records, and returns the average GPA of
;;; students in that major.
;;;
;;; Use internal defines for helpers. Use let to store intermediate
;;; results (like the filtered list or the sum).
;;;
;;; Examples:
;;;   (average-gpa-by-major 'cs students)     => 3.733...
;;;   (average-gpa-by-major 'math students)   => 2.9
;;;   (average-gpa-by-major 'physics students) => 3.2
;;;
;;; Hint: Filter to get only students in that major, then compute
;;; the sum of GPAs and divide by the count.

; Your code here:
(define (average-gpa-by-major major records)
  (define (get-major record) (cadr record))
  (define (get-gpa record) (caddr record))
  (define (sum a)
    (cond ((null? a) 0)
          (else (+ (car a) (sum (cdr a))))))
  (let ((majors (keep-if (lambda (x) (eq? (get-major x) major)) records))
        ) ; majors now has only the majors we want
    (/ (sum (transform-list get-gpa majors)) (length majors))
    ))
(average-gpa-by-major 'cs students)
(average-gpa-by-major 'math students)
(average-gpa-by-major 'physics students)

;;; ============================================================
;;; PROBLEM 8: Challenge - Function Composition
;;; ============================================================

;;; Write a function `compose` that takes two single-argument functions
;;; f and g, and returns a new function that computes f(g(x)).
;;;
;;; Examples:
;;;   ((compose add1 add1) 5)        => 7      ; add1(add1(5))
;;;   ((compose abs sub1) 3)         => 2      ; abs(sub1(3)) = abs(2)
;;;   ((compose (lambda (x) (* x 2))
;;;             (lambda (x) (+ x 1))) 5)  => 12  ; (* 2 (+ 5 1))
;;;
;;; Hint: compose returns a lambda!

; Your code here:


;;; Use compose to create a function that:
;;; 1. Adds 1 to a number
;;; 2. Squares the result
;;; 3. Subtracts 10
;;;
;;; Test: applying this to 4 should give: ((4+1)^2) - 10 = 25 - 10 = 15

; (define add1-square-sub10 (compose ??? (compose ??? ???)))
; (add1-square-sub10 4)  ; should be 15
