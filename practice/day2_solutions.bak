#lang racket
;;; COMP 360 - Day 2 Practice Problems
;;; Functions, Pairs, Lists, and Recursion

;;; ============================================================
;;; PROBLEM 1: Simple Functions
;;; ============================================================
;;; Write a function called `double` that takes a number and returns
;;; twice that number.
;;;
;;; Examples:
;;;   (double 5)  => 10
;;;   (double -3) => -6
;;;   (double 0)  => 0

; Your code here:
(define (double x) (* x 2))

;;; Write a function called `pair-average` that takes a pair of two
;;; numbers and returns their average.
;;;
;;; Example:
;;;   (pair-average (cons 10 20)) => 15
;;;
;;; Hints:
;;;   - Use (car p) to get the first element of pair p
;;;   - Use (cdr p) to get the second element of pair p

; Your code here:
(define (pair-average p)
  (/ (+ (car p) (cdr p)) 2))


;;; Now write TWO test cases for your pair-average function.
;;; A test case is just calling the function and seeing if it
;;; produces the expected result.
;;;
;;; Example test case (you need to write two MORE):
;;;   (pair-average (cons 0 100))  ; should return 50

; Your test cases here:
"test pair-average:"
(pair-average (cons 40 50)) ; should be 45
(pair-average (cons 10 20)) ; should be 15


;;; ============================================================
;;; PROBLEM 2: Working with Pairs
;;; ============================================================
;;; Write a function called `make-point` that takes two numbers x and y
;;; and returns a pair representing a 2D point.
;;;
;;; Examples:
;;;   (make-point 3 4) => '(3 . 4)
;;;   (make-point 0 0) => '(0 . 0)

; Your code here:
(define (make-point x y)
  (cons x y))


;;; Write a function called `distance-from-origin` that takes a point
;;; (a pair of x and y coordinates) and returns the distance from the
;;; origin (0,0). Recall: distance = sqrt(x^2 + y^2)
;;; Hint: Use (sqrt n) to compute square roots.
;;;
;;; Examples:
;;;   (distance-from-origin (cons 3 4)) => 5
;;;   (distance-from-origin (cons 0 0)) => 0
;;;   (distance-from-origin (cons 1 1)) => 1.4142135623730951

; Your code here:
(define (distance-from-origin p)
  (sqrt (+ (* (car p) (car p))
           (* (cdr p) (cdr p))
           )
        )
  )
"test distance-from-origin"
(distance-from-origin (cons 1 1))
(distance-from-origin (cons 3 4))
(distance-from-origin (cons 5 12))

;;; ============================================================
;;; PROBLEM 3: Basic List Operations
;;; ============================================================
;;; Write a function called `list-length` that takes a list and returns
;;; the number of elements in it. Do NOT use the built-in `length` function.
;;; Use recursion!
;;;
;;; Examples:
;;;   (list-length '())        => 0
;;;   (list-length '(1 2 3))   => 3
;;;   (list-length '('a 'b 'c 'd)) => 4
;;;
;;; Hints:
;;;   - What is the length of an empty list?
;;;   - If you know the length of (cdr lst), what is the length of lst?

; Your code here:
(define (list-length lst)
  (if (null? lst)
      0
      (+ 1 (list-length (cdr lst)))
      )
  )

"test list-length"
(list-length '())
(list-length '(a b c d))
(list-length '(10 20 30 40 50 60))

;;; Write a function called `contains?` that takes a value and a list,
;;; and returns #t if the value is in the list, #f otherwise.
;;;
;;; Examples:
;;;   (contains? 3 '(1 2 3 4)) => #t
;;;   (contains? 5 '(1 2 3 4)) => #f
;;;   (contains? 'a '())       => #f
;;;
;;; Hints:
;;;   - Is the value ever in an empty list?
;;;   - Check if the value equals the first element (car), or
;;;     recursively check if it's in the rest (cdr)

; Your code here:
(define (contains? a lst)
  (if (null? lst)
      #f
      (if (eq? (car lst) a)
          #t
          (contains? a (cdr lst)))))

"test contains?"
(contains? 3 '(1 2 3 4))
(contains? 5 '(1 2 3 4))
(contains? 'a '())
(contains? 'a '('f 'g 'h 'i 'j 'k 'l 'm 'a))

;;; ============================================================
;;; PROBLEM 4: Recursive List Building
;;; ============================================================
;;; Write a function called `repeat` that takes a value and a number n,
;;; and returns a list containing n copies of that value.
;;;
;;; Examples:
;;;   (repeat 'x 3)   => '(x x x)
;;;   (repeat 0 5)    => '(0 0 0 0 0)
;;;   (repeat 'hi 0)  => '()
;;;   (repeat 'a 1)   => '(a)
;;;
;;; Hints:
;;;   - Base case: what if n is 0 (or less)?
;;;   - Recursive case: cons the value onto a list of (n-1) copies

; Your code here:
(define (repeat a n)
  (if (= n 0)
      '()
      (cons a (repeat a (- n 1)))))

"test repeat"
(repeat 4 12)
(repeat 'a 6)

;;; Write a function called `range` that takes two numbers `start` and `end`
;;; and returns a list of integers from start up to (but not including) end.
;;;
;;; Examples:
;;;   (range 1 5)  => '(1 2 3 4)
;;;   (range 0 3)  => '(0 1 2)
;;;   (range 5 5)  => '()
;;;   (range 3 1)  => '()
;;;
;;; Hints:
;;;   - Base case: what if start >= end?
;;;   - Recursive case: cons start onto the range from (start+1) to end

; Your code here:
(define (range start end)
  (if (>= start end)
      '()
      (cons start (range (+ start 1) end))))

"test range"
(range 1 5)
(range 2 10)
(range 3 3)
(range 4 3)


;;; ============================================================
;;; PROBLEM 5: Lists of Pairs (Challenge!)
;;; ============================================================
;;; A "grade entry" is a pair where the car is a student name (a symbol)
;;; and the cdr is their score (a number).
;;; Example: (cons 'alice 95) represents Alice with a score of 95.

;;; Write a function called `lookup-grade` that takes a student name
;;; and a list of grade entries, and returns the student's score.
;;; If the student is not found, return #f.
;;;
;;; Examples:
;;;   (define grades (list (cons 'alice 95) (cons 'bob 87) (cons 'carol 92)))
;;;   (lookup-grade 'alice grades) => 95
;;;   (lookup-grade 'bob grades)   => 87
;;;   (lookup-grade 'dave grades)  => #f
;;;
;;; Hints:
;;;   - Base case: empty list means student not found
;;;   - Check if (car (car lst)) equals the name you're looking for
;;;   - Use `equal?` to compare symbols: (equal? 'alice 'alice) => #t

; Your code here:
(define grades (list (cons 'alice 95) (cons 'bob 87) (cons 'carol 92)))
(define (lookup-grade name grades)
  (if (null? grades)
      #f
      (if (eq? (car (car grades)) name) ; the name we want is in the first grade entry
          (cdr (car grades)) ; evaluate to the grade for the first grade entry
          (lookup-grade name (cdr grades)))))

"test lookup-grade"
(lookup-grade 'alice grades)          
(lookup-grade 'bob grades)
(lookup-grade 'dave grades)

;;; Write a function called `count-passing` that takes a list of grade
;;; entries and a passing threshold, and returns the number of students
;;; who scored at or above the threshold.
;;;
;;; Examples:
;;;   (define grades (list (cons 'alice 95) (cons 'bob 67) (cons 'carol 72)))
;;;   (count-passing grades 70) => 3  ; alice and carol passed
;;;   (count-passing grades 90) => 2  ; only alice
;;;   (count-passing grades 50) => 3  ; everyone passed
;;;   (count-passing '() 70)    => 0
;;;
;;; Hints:
;;;   - Base case: empty list has 0 passing students
;;;   - Check if the first student's score (cdr (car lst)) >= threshold
;;;   - If passing, add 1 to the recursive count; otherwise just recurse

; Your code here:
(define (count-passing grades g)
  (if (null? grades)
      0
      (if (>= (cdr (car grades)) g)
          (+ 1 (count-passing (cdr grades) g))
          (count-passing (cdr grades) g))))

"test count-passing"
(count-passing grades 70)
(count-passing grades 90)
(count-passing grades 50)
(count-passing '() 50)

