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
(define (sierpinski-triangle size depth)
  (cond
    [(eq? depth 0) (triangle size "solid" "green")]
    [else
      (define tri (sierpinski-triangle (/ size 2) (- depth 1)))
      (above
        tri
        (beside
          tri
          tri
          )
        )
      ]
    )
  )
; test


; Koch curve
(define (koch-curve size depth)
  (define (helper size depth)
    (cond
      [(eq? depth 0)  (line size 0 "black")]
      [else
        (define curverec (koch-curve (/ size 3) (- depth 1)))

        (define leftside
          (beside/align "bottom" curverec (rotate 60 curverec))
          )

        (beside leftside (flip-horizontal leftside))
        ]
      )
    )

  (define fullimg (helper size depth))
  (define width (image-width fullimg))
  (scale (/ size width) fullimg)
  )
; test


; Koch snowflake
(define (koch-snowflake size depth)
  (define curve (koch-curve (/ size 2) depth))
  (define left (rotate 60 curve))
  (define bottom (flip-vertical curve))
  (define right (flip-horizontal left))

  (above
    (beside left right)
    bottom
    )
  )
; test


;;; Part 3: L-systems w/ Turtle Graphics

; apply-rule
(define (apply-rule char rules)
  (define (rule-applies? rule)
    (equal? char (car rule))
    )

  (cond
    [(empty? rules) (string char)]
    [else (cond
      [(rule-applies? (car rules))   (cdr (car rules))]
      [else     (apply-rule char (cdr rules))]
      )]
    )
  )


; l-system-step
(define (l-system-step str rules)
  (foldr
    (lambda (c l) (string-append (apply-rule c rules) l))
    ""
    (string->list str)
    )
  )

; l-system-generate
(define (l-system-generate axiom rules its)
  (cond
    [(zero? its) axiom]
    [else        (l-system-step (l-system-generate axiom rules (- its 1)) rules)]
    )
  )


; interpret-l-system
(define (interpret-l-system str turtle step-size turn-angle stack background)
  (define (t-draw t img)
    (define p1 (cons (first t) (second t)))
    (define p2_flat (cons (+ step-size (first t)) (second t)))
    (define p2 (rotate-point p2_flat p1 (third t)))

    (draw-line p1 p2 "black" img)
    )


  (define (internal str t stack)
    (cond
      [(string=? str "") background]
      [else
        (define rest-str (substring str 1))
        (match (string-ref str 0)
          [#\F  (t-draw t (internal rest-str (turtle-forward t step-size) stack))] ;; Forward
          [#\G  (t-draw t (internal rest-str (turtle-forward t step-size) stack))] ;; Forward
          [#\+  (internal rest-str (turtle-turn t turn-angle) stack)] ;; Turn right
          [#\-  (internal rest-str (turtle-turn t (* -1 turn-angle)) stack)] ;; Turn left
          [#\[  (internal rest-str t (cons t stack))] ;; Push
          [#\]  (internal rest-str (car stack) (cdr stack))] ;; Pop
          [_    (internal rest-str t stack)] ;; else
          )
        ]
      )
    )

  (internal str turtle stack)
  )
; test

(define (draw-l-system axiom rules iterations step-size turn-angle x y a)
  (let ((str (l-system-generate axiom rules iterations)))
    (interpret-l-system str (make-turtle x y a) step-size turn-angle '() (rectangle 300 300 "solid" "white"))))

; L-system Examples

; Sierpinski


; Plant


; Dragon


;;; Part 5: Come up with your own fractal!

