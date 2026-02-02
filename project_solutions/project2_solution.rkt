#lang racket
(require 2htdp/image)

(define (midpoint a b)
  (cons (* 1.0 (/ (+ (car a) (car b)) 2)) (* 1.0 (/ (+ (cdr a) (cdr b)) 2))))

'midpoint
(midpoint (cons 0 0) (cons 100 100))   ; => (cons 50 50)
(midpoint (cons 0 0) (cons 50 0))      ; => (cons 25 0)
(midpoint (cons 0 0) (cons 0 80))      ; => (cons 0 40)
(midpoint (cons 10 20) (cons 30 40))   ; => (cons 20 30)
(midpoint (cons -10 -10) (cons 10 10)) ; => (cons 0 0)
(midpoint (cons 5 5) (cons 5 5))       ; => (cons 5 5)

(define (point-at-fraction a b t)
  (define (result x1 x2 t)
    (+ x1 (* t (- x2 x1))))
  (cons (result (car a) (car b) t)
        (result (cdr a) (cdr b) t)))

'point-at-fraction
(point-at-fraction (cons 0 0) (cons 100 0) 0)     ; => (cons 0 0)
(point-at-fraction (cons 0 0) (cons 100 0) 0.25)  ; => (cons 25 0)
(point-at-fraction (cons 0 0) (cons 100 0) 0.5)   ; => (cons 50 0)
(point-at-fraction (cons 0 0) (cons 100 0) 0.75)  ; => (cons 75 0)
(point-at-fraction (cons 0 0) (cons 100 0) 1.0)   ; => (cons 100 0)
(point-at-fraction (cons 0 0) (cons 100 100) 0.5) ; => (cons 50 50)
(point-at-fraction (cons 0 0) (cons 0 200) 0.25)  ; => (cons 0 50)
(point-at-fraction (cons 10 10) (cons 50 90) 0.5) ; => (cons 30 50)
(point-at-fraction (cons 100 0) (cons 0 0) 0.5)   ; => (cons 50 0)


(define (rotate-point p center theta)
  (define (translate p d) ; shift point p by vector d
    (cons (+ (car p) (car d)) (+ (cdr p) (cdr d))))
  (define (rotate p theta)
    (cons (- (* (car p) (cos theta)) (* (cdr p) (sin theta)))
          (+ (* (car p) (sin theta)) (* (cdr p) (cos theta)))))
  ; translate the point so that the center is at the origin
  (let* (
         (neg-center (cons (- (car center)) (- (cdr center))))
         (translated (translate p neg-center))
         (rotated (rotate translated theta))
         )
  (translate rotated center)))

'rotate-point
; Rotate around the origin (no rotation)
(rotate-point (cons 1 0) (cons 0 0) 0)            ; => (cons 1.0 0.0)

; 90-degree rotations around origin
(rotate-point (cons 1 0) (cons 0 0) (/ pi 2))     ; => (cons 0.0 1.0) approximately
(rotate-point (cons 1 0) (cons 0 0) pi)           ; => (cons -1.0 0.0) approximately
(rotate-point (cons 1 0) (cons 0 0) (* 3/2 pi))   ; => (cons 0.0 -1.0) approximately
(rotate-point (cons 0 1) (cons 0 0) (/ pi 2))     ; => (cons -1.0 0.0) approximately

; Larger radius
(rotate-point (cons 5 0) (cons 0 0) (/ pi 2))     ; => (cons 0.0 5.0) approximately
(rotate-point (cons 10 0) (cons 0 0) pi)          ; => (cons -10.0 0.0) approximately

; Rotate around a different center
(rotate-point (cons 2 0) (cons 1 0) (/ pi 2))     ; => (cons 1.0 1.0) approximately
(rotate-point (cons 3 1) (cons 1 1) pi)           ; => (cons -1.0 1.0) approximately
(rotate-point (cons 10 5) (cons 5 5) (/ pi 2))    ; => (cons 5.0 10.0) approximately
(rotate-point (cons 6 4) (cons 4 4) (/ pi 2))     ; => (cons 4.0 6.0) approximately

; 60-degree rotation (important for Koch curve!)
(rotate-point (cons 1 0) (cons 0 0) (/ pi 3))     ; => (cons 0.5 0.866) approximately
(rotate-point (cons 1 0) (cons 0 0) (- (/ pi 3))) ; => (cons 0.5 -0.866) approximately
  