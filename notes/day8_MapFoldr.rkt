#lang racket

(lambda (a b) (
                (sqrt (+ (expt (- (car b) (car a)) 2) (expt (- (cdr b) (cdr a)))))
                )) ; should return the distance between a and b
; d = sqrt((x2 - x1)^2 + (y2 - y1)^2)
; (sqrt x)
; (expt a b) gives a^b

; you could also use this instead of expt
;(let ((square (lambda (x) (* x x)))))

; this function maps pairs of numbers to single numbers
; according to some function f
(define (pairs-to-nums f pairs) ; use recursion, pick a base case
  (if (null? pairs) '()
      (cons (f (car pairs))
            (pairs-to-nums f (cdr pairs)))))

(pairs-to-nums (lambda (p) (if (>= (car p) (cdr p))
                               (car p)
                               (cdr p)))
               '((1 . 2) (3 . 4) (1 . 7)))

(pairs-to-nums (lambda (p) (+ (car p) (cdr p)))
               '((1 . 2) (3 . 4) (1 . 7)))


"foldr test"
(foldr (lambda (item acc) (+ 1 acc)) 0 '(1 5 7 9))
;(+ 1 0)
;(+ 1 rec-call on 579) -> (+ 1 (+ 1 rec-call 79)) -> (+ 1 (+ 1 (+ 1 (+ 1 0))))

(define lst '(10 20 40 10 0 0))
(foldr + 4 lst)
(foldr (lambda (item acc) (+ 1 acc)) -1 lst) ; counts the length of the list

(define (reverse lst)
  (foldr (lambda (a b) (append b (list a))) '() lst))

(reverse lst)

(define (max lst)
  (foldr (lambda (candidate current-max)
           (if (> candidate current-max)
               candidate
               current-max))
         (car lst) lst))

(max lst)
;(max '())