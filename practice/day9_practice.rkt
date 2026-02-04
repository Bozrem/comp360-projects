#lang racket
;;; COMP 360 - Day 9 Practice Problems
;;; Tail Recursion

;;; Remember the methodology for converting to tail recursion:
;;; 1. Create a helper function that takes an accumulator
;;; 2. Old base case's return value becomes initial accumulator value
;;; 3. Final accumulator value becomes new base case return value
;;; 4. Do the computation FIRST, then make the recursive call LAST


;;; ============================================================
;;; PROBLEM 1: Convert to Tail Recursion
;;; ============================================================

;;; Convert this function to be tail-recursive.
;;; It computes the length of a list.

(define (length-not-tr lst)
  (if (null? lst)
      0
      (+ 1 (length-not-tr (cdr lst)))))

;;; Your tail-recursive version:
;;; Hint: What should the initial accumulator be?
;;;       What gets accumulated at each step?

(define (length-tr lst)
  'x)

;;; Test cases (uncomment to test):
; (length-tr '())           ; => 0
; (length-tr '(a b c))      ; => 3
; (length-tr '(1 2 3 4 5))  ; => 5


;;; ============================================================
;;; PROBLEM 2: Write Tail-Recursive from Scratch
;;; ============================================================

;;; Write a tail-recursive function (power base exp) that computes
;;; base raised to the exp power (assume exp >= 0).
;;;
;;; Examples:
;;;   (power 2 0)   => 1
;;;   (power 2 3)   => 8
;;;   (power 3 4)   => 81
;;;   (power 5 2)   => 25
;;;
;;; Hint: Think about what a while loop version would look like.
;;;       What values would you track? Those become your accumulator(s).

(define (power base exp)
  'x)


;;; ============================================================
;;; PROBLEM 3: Spot the Tail Recursion
;;; ============================================================

;;; For each function below, determine if it is tail-recursive.
;;; Write "YES" or "NO" and explain why in a comment.

;;; Function A:
(define (func-a lst)
  (if (null? lst)
      '()
      (cons (car lst) (func-a (cdr lst)))))

;;; Is func-a tail-recursive?
;;; Answer:
;;; Why:


;;; Function B:
(define (func-b lst acc)
  (if (null? lst)
      acc
      (func-b (cdr lst) (cons (car lst) acc))))

;;; Is func-b tail-recursive?
;;; Answer:
;;; Why:


;;; Function C:
(define (func-c n)
  (cond
    ((= n 0) 1)
    ((even? n) (func-c (/ n 2)))
    (else (* n (func-c (- n 1))))))

;;; Is func-c tail-recursive?
;;; Answer:
;;; Why:


;;; ============================================================
;;; PROBLEM 4: Visualize the Stack (Demo/Discussion)
;;; ============================================================

;;; Run this code to see the difference between regular and
;;; tail recursion in action!

;;; Helper to count stack depth (only counting our functions)
(define tracked-functions '(sum-list sum-list-tail))

(define (get-relevant-stack)
  (filter
   (lambda (frame)
     (and (car frame)
          (member (car frame) tracked-functions)))
   (continuation-mark-set->context (current-continuation-marks))))

(define (show-stack-info label args)
  (define stack (get-relevant-stack))
  (printf "~a~a - stack depth: ~a~n" label args (length stack))
  (for ([frame stack])
    (printf "    ~a~n" (car frame)))
  (newline))

;;; Non-tail-recursive sum
(define (sum-list lst)
  (show-stack-info 'sum-list (format "(~a)" lst))
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

;;; Tail-recursive sum
(define (sum-list-tail lst [acc 0])
  (show-stack-info 'sum-list-tail (format "(~a, ~a)" lst acc))
  (if (null? lst)
      acc
      (sum-list-tail (cdr lst) (+ (car lst) acc))))

;;; Uncomment to run the demos:
; (displayln "=== Non-tail-recursive ===\n")
; (sum-list '(1 2 3 4))

; (displayln "=== Tail-recursive ===\n")
; (sum-list-tail '(1 2 3 4))

;;; Questions to think about:
;;; 1. How does the stack depth change in each version?
;;; 2. Why does the tail-recursive version stay at depth 1?
;;; 3. What would happen with a list of 10,000 elements in each version?


;;; ============================================================
;;; ADDITIONAL CHALLENGE: Reverse
;;; ============================================================

;;; Here's a non-tail-recursive reverse:

(define (reverse-not-tr lst)
  (if (null? lst)
      '()
      (append (reverse-not-tr (cdr lst)) (list (car lst)))))

;;; This is actually doubly inefficient:
;;; 1. It's not tail-recursive (stack grows with list length)
;;; 2. append is O(n), so overall it's O(n^2)!

;;; Part A: Write a tail-recursive reverse that is O(n).
;;; Hint: Think about what happens if you cons each element
;;;       onto an accumulator as you traverse the list.

(define (reverse-tr lst)
  'x)

;; Can you adapt the stack trace code from above to verify
;; your answer is actually tail recursive?

;;; Test cases:
; (reverse-tr '())          ; => '()
; (reverse-tr '(1 2 3))     ; => '(3 2 1)
; (reverse-tr '(a b c d e)) ; => '(e d c b a)


;;; Part B: Now write reverse using foldr.
;;;
;;; Hint: Remember that (foldr f base '(a b c)) computes (f a (f b (f c base)))
;;;       What function f would build the reversed list?
;;;       Think about where each element needs to end up.
;;;
;;; Note: This version is actually O(n^2) like reverse-not-tr above.
;;;       Can you see why? (Think about what append does at each step.)

(define (reverse-foldr lst)
  (foldr 'x 'x 'x))

;;; Test cases:
; (reverse-foldr '())          ; => '()
; (reverse-foldr '(1 2 3))     ; => '(3 2 1)
; (reverse-foldr '(a b c d e)) ; => '(e d c b a)

;;; Discussion: Why is the tail-recursive version more efficient than
;;; the foldr version for reverse? (This is a case where foldr isn't
;;; the best tool for the job!)
