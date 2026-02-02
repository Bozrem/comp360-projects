#lang racket
;;; COMP 360 - Day 8 Practice Problems
;;; Foldr

;;; Here is foldr for reference:
;(define (foldr func base lst)
;  (if (null? lst) base
;      (func (car lst)
;            (foldr func base (cdr lst)))))

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
  (foldr * 1 lst))

"product test"
(product '(1 2 3))
(product '(5 0 10))
(product '(5 6 2))

;;; Part B: Convert this function to use foldr.
;;; Hint: Try using a lambda which increments a counter!

;(define (count-evens lst)
;  (if (null? lst) 0
;      (+ (if (even? (car lst)) 1 0)
;         (count-evens (cdr lst)))))

;;; Your foldr version:
(define (count-evens lst)
  (foldr (lambda (value count) (if (even? value) (+ 1 count) count))
         0
         lst))

"count-evens test"
(count-evens '(1 2 3 4 5 6 7))
(count-evens '(5 0 10 9 7))
(count-evens '())


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
(define (all-positive? lst)
  (foldr (lambda (value all-pos) (and (> value 0) all-pos))
         #t
         lst))

"all-positive? test"
(all-positive? '(1 2 3 4 5 6 7))
(all-positive? '(1 2 3 4 5 -6 7))
(all-positive? '(5 0 -10 9 -7))
(all-positive? '())


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
(define (my-append lst1 lst2)
  (foldr (lambda (value lst) (cons value lst))
         lst2
         lst1))

"my-append test"
(my-append '(1 2 3) '(4 5 6))
(my-append '(1 2 3) '())
(my-append '() '(4 5 6))
(my-append '(1 3) '(7 9 5 6))


;;; ============================================================
;;; PROBLEM 3: Trace Through Foldr
;;; ============================================================

;;; What does this expression evaluate to?
;;; Trace through the evaluation step by step.

"trace test"
(foldr (lambda (x acc) (cons (* x 2) acc)) '() '(1 2 3))

;;; Step-by-step:
;;; foldr will compute: (f 1 (f 2 (f 3 base)))
;;; where f = (lambda (x acc) (cons (* x 2) acc)) and base = '()
;;;
;;; Innermost first:
;;;   (f 3 '())          = '(6)
;;;   (f 2 '(6))         = '(4 6)
;;;   (f 1 '(4 6))       = '(2 4 6)
;;;
;;; Final answer:
'(2 4 6)

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
(define (string-concat lst)
  (foldr string-append "" lst))

"string-concat test"
(string-concat '("hey" "there" "mister" "blue"))


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
(define (count-if pred lst)
  (foldr (lambda (value count) (if (pred value) (+ 1 count) count))
         0
         lst))

"count-if test"
(count-if even? '(1 2 3 4 5 6))
(count-if (lambda (x) (> x 0)) '(-1 2 -3 4)) 
(count-if string? '(1 "a" 2 "b" 3))


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
(define (my-map f lst)
  (foldr (lambda (value others) (cons (f value) others)) '() lst))

"my-map tests"
(my-map add1 '(1 2 3)) 
(my-map (lambda (x) (* x x)) '(1 2 3))
(my-map car '((a b) (c d) (e f)))


;;; Part B: Implement filter using foldr.
;;;
;;; Examples:
;;;   (my-filter even? '(1 2 3 4 5 6))        => '(2 4 6)
;;;   (my-filter (lambda (x) (> x 3)) '(1 5 2 4 3))  => '(5 4)
;;;   (my-filter positive? '(-1 2 -3 4))      => '(2 4)

; Your code here:
(define (my-filter pred lst)
  (foldr (lambda (value others) (if (pred value) (cons value others) others))
         '()
         lst))

"my-filter tests"
(my-filter even? '(1 2 3 4 5 6))
(my-filter (lambda (x) (> x 3)) '(1 5 2 4 3))
(my-filter positive? '(-1 2 -3 4))


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
;;; Hint: Use lambda with an accumulator to track the longest string seen so far.
;;;       Use string-length to compare.

; Your code here:
(define (longest-string lst)
  (foldr (lambda (new best) (if (> (string-length new) (string-length best)) new best))
         ""
         lst))

"longest-string test"
(longest-string '("cat" "elephant" "dog"))
(longest-string '("hi" "hello" "hey"))
(longest-string '("a" "bb" "ccc" "dd"))
(longest-string '()) 
