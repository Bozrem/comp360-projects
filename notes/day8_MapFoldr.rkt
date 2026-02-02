#lang racket

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