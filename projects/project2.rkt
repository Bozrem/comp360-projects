#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(provide (all-defined-out))

(define (show img)
  (big-bang 0 (to-draw (lambda (w) img)))
  )
;;; Provided Helpers:

;; draw-polygon: list of points (pairs), mode ("solid" or "outline"), color -> image
;; Draws a polygon using a list of (cons x y) points
(define (draw-polygon image points mode color)
  (add-polygon image (map (lambda (p) (make-posn (car p) (cdr p))) points)
               mode
               color))

;; Turtle Helpers
;; Constructor (provided)
(define (make-turtle x y angle) (list x y angle))

;; Accessors
(define (turtle-x t) (car t))
(define (turtle-y t) (cadr t))
(define (turtle-angle t) (caddr t))

;; Movement
(define (turtle-forward t distance)
  (make-turtle (+ (turtle-x t) (* distance (cos (turtle-angle t))))
               (+ (turtle-y t) (* distance (sin (turtle-angle t))))
               (turtle-angle t)))

(define (turtle-turn t angle)
  (make-turtle (turtle-x t)
               (turtle-y t)
               (+ (turtle-angle t) angle)))

(define (turtle-point t)
  (cons (car t) (cadr t)))

;;; Part 1:

; midpoint
(define (midpoint p1 p2)
  (define x1 (car p1))
  (define y1 (cdr p1))
  (define x2 (car p2))
  (define y2 (cdr p2))

  (cons (/ (+ x1 x2) 2) (/ (+ y1 y2) 2) )
  )


; point-at-fraction
(define (point-at-fraction p1 p2 frac)
  (define x1 (car p1))
  (define y1 (cdr p1))
  (define x2 (car p2))
  (define y2 (cdr p2))

  (define x-mid (+ x1 (* frac (- x2 x1))))
  (define y-mid (+ y1 (* frac (- y2 y1))))

  (cons (exact-round x-mid) (exact-round y-mid))
  )


; rotate-point
(define (rotate-point p center ang)
  (define x-offset (- 0 (car center)))
  (define y-offset (- 0 (cdr center)))

  (define x (+ (car p) x-offset))
  (define y (+ (cdr p) y-offset))

  ; x' = x*cos(angle) - y*sin(angle), y' = x*sin(angle) + y*cos(angle)
  (define new-x (- (* x (cos ang)) (* y (sin ang))))
  (define new-y (+ (* x (sin ang)) (* y (cos ang))))

  (cons (- new-x x-offset) (- new-y y-offset))
  )


; draw-line
(define (draw-line p1 p2 color image)
  (define x1 (car p1))
  (define y1 (cdr p1))
  (define x2 (car p2))
  (define y2 (cdr p2))

  (add-line image x1 y1 x2 y2 color)
  )
; tests


;;; Part 2: Fractals

; sierpinski

; test


; Koch curve

; test


; Koch snowflake

; test


;;; Part 3: L-systems w/ Turtle Graphics

; apply-rule

; test


; l-system-step

; test


; l-system-generate

; test


; interpret-l-system

; test


; L-system Examples

; Sierpinski


; Plant


; Dragon


;;; Part 5: Come up with your own fractal!

