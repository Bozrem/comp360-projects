#lang racket
(require 2htdp/image)
(require lang/posn)

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

; tests


; point-at-fraction

; tests


; rotate-point

; tests


; draw-line

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

