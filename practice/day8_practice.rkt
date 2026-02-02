#lang racket
;;; COMP 360 - Day 8 Practice Problems
;;; Foldr

;;; Here is foldr for reference:
; (define (foldr func base lst)
;   (if (null? lst) base
;       (func (car lst)
;             (foldr func base (cdr lst)))))

;;; Remember: (foldr f base '(x1 x2 x3)) computes
;;;           (f x1 (f x2 (f x3 base)))


;;; ============================================================
;;; PROBLEM 1: Convert to Foldr
;;; ============================================================

;;; Part A: Convert this function to use foldr.
;;; The recursive structure matches foldr exactly!

;(define (product lst)
;  (if (null? lst) 1
;      (* (car lst) (product (cdr lst)))))

;;; Your foldr version:
(define (product lst)
  (foldr ??? ??? ???))

;;; Part B: Convert this function to use foldr.
;;; Hint: Try using a lambda which increments a counter!

;(define (count-evens lst)
;  (if (null? lst) 0
;      (+ (if (even? (car lst)) 1 0)
;         (count-evens (cdr lst)))))

;;; Your foldr version:
(define (count-evens lst)
  (foldr ??? ??? ???))


;;; ============================================================
;;; PROBLEM 2: Use Foldr to Solve
;;; ============================================================

;;; Part A: Write a function (all-positive? lst) using foldr that
;;; returns #t if every element in the list is positive, #f otherwise.
;;;
;;; Examples:
;;;   (all-positive? '(1 2 3 4))     => #t
;;;   (all-positive? '(1 -2 3 4))    => #f
;;;   (all-positive? '())            => #t  (vacuously true)
;;;
;;; Hint: What should base be? What does the lambda need to check?

; Your code here:



;;; Part B: Write a function (my-append lst1 lst2) using foldr that
;;; appends two lists together.
;;;
;;; Examples:
;;;   (my-append '(1 2) '(3 4))      => '(1 2 3 4)
;;;   (my-append '() '(a b c))       => '(a b c)
;;;   (my-append '(x y z) '())       => '(x y z)
;;;
;;; Hint: Which list should you fold over? What should base be?
;;;       Think about what cons does: compare (cons '(1 2 3) 4) and (cons 4 '(1 2 3))

; Your code here:



;;; ============================================================
;;; PROBLEM 3: Trace Through Foldr
;;; ============================================================

;;; What does this expression evaluate to?
;;; Trace through the evaluation step by step.



;;; Step-by-step:
;;; foldr will compute: (f 1 (f 2 (f 3 base)))
;;; where f = (lambda (x acc) (cons (* x 2) acc)) and base = '()
;;;
;;; Innermost first:
;;;   (f 3 '())         = ???
;;;   (f 2 ???)         = ???
;;;   (f 1 ???)         = ???
;;;
;;; Final answer:


;;; ============================================================
;;; PROBLEM 4: More Foldr Practice
;;; ============================================================

;;; Part A: Write (string-concat lst) that concatenates a list of
;;; strings into one string.
;;;
;;; Examples:
;;;   (string-concat '("hello" " " "world"))  => "hello world"
;;;   (string-concat '("a" "b" "c"))          => "abc"
;;;   (string-concat '())                     => ""
;;;
;;; Hint: Use string-append, which takes two strings.

; Your code here:


;;; Part B: Write (count-if pred lst) that counts how many elements
;;; in lst satisfy the predicate pred.
;;;
;;; Examples:
;;;   (count-if even? '(1 2 3 4 5 6))         => 3
;;;   (count-if (lambda (x) (> x 0)) '(-1 2 -3 4))  => 2
;;;   (count-if string? '(1 "a" 2 "b" 3))     => 2
;;;
;;; This is like count-evens but generalized to any predicate!

; Your code here:


;;; ============================================================
;;; PROBLEM 5: Challenge - Implement Map and Filter with Foldr
;;; ============================================================

;;; Part A: Implement map using foldr.
;;;
;;; Examples:
;;;   (my-map add1 '(1 2 3))                  => '(2 3 4)
;;;   (my-map (lambda (x) (* x x)) '(1 2 3))  => '(1 4 9)
;;;   (my-map car '((a b) (c d) (e f)))       => '(a c e)

; Your code here:
; (define (my-map f lst)
;   (foldr ??? ??? ???))


;;; Part B: Implement filter using foldr.
;;;
;;; Examples:
;;;   (my-filter even? '(1 2 3 4 5 6))        => '(2 4 6)
;;;   (my-filter (lambda (x) (> x 3)) '(1 5 2 4 3))  => '(5 4)
;;;   (my-filter positive? '(-1 2 -3 4))      => '(2 4)

; Your code here:
; (define (my-filter pred lst)
;   (foldr ??? ??? ???))


;;; ============================================================
;;; PROBLEM 6: Wildcard - Foldr with Non-Numeric Accumulators
;;; ============================================================

;;; Write (longest-string lst) that returns the longest string in
;;; a list of strings. If the list is empty, return "".
;;; If there's a tie, return the first one encountered.
;;;
;;; Examples:
;;;   (longest-string '("cat" "elephant" "dog"))  => "elephant"
;;;   (longest-string '("hi" "hello" "hey"))      => "hello"
;;;   (longest-string '("a" "bb" "ccc" "dd"))     => "ccc"
;;;   (longest-string '())                        => ""
;;;
;;; Hint: The accumulator tracks the longest string seen so far.
;;;       Use string-length to compare.

; Your code here:

