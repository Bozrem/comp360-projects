#lang racket
(require 2htdp/image)
(require lang/posn)

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


'draw-line
(define (draw-line a b color background)
  (add-line background (car a) (cdr a) (car b) (cdr b) color))

(draw-line (cons 0 0) (cons 100 100) "black" (rectangle 200 200 "solid" "white"))
(draw-line (cons 50 0) (cons 50 100) "red" (rectangle 100 100 "solid" "white"))
(draw-line (cons 0 50) (cons 100 50) "blue" (rectangle 100 100 "solid" "white"))
(draw-line (cons 10 10) (cons 90 90) "green" (rectangle 100 100 "solid" "gray"))

'sierpinski
(define (draw-polygon image points mode color)
  (add-polygon image (map (lambda (p) (make-posn (car p) (cdr p))) points)
               mode
               color))
    
(define (sierpinski-triangle p1 p2 p3 depth)
  (define background (let ((min-x (foldr (lambda (v acc) (min (car v) acc)) (car p1) (list p1 p2 p3)))
                           (max-x (foldr (lambda (v acc) (max (car v) acc)) (car p1) (list p1 p2 p3)))
                           (min-y (foldr (lambda (v acc) (min (cdr v) acc)) (cdr p1) (list p1 p2 p3)))
                           (max-y (foldr (lambda (v acc) (max (cdr v) acc)) (cdr p1) (list p1 p2 p3)))
                           (pad 0))

                       (rectangle (+ max-x pad) (+ max-y pad) "solid" "white")))
  (define (sierpinski-helper p1 p2 p3 depth image)
    (if (= depth 0)
        (draw-polygon image (list p1 p2 p3) "solid" "forest green")
        (let* ((t1 (sierpinski-helper p3 (midpoint p3 p1) (midpoint p2 p3) (- depth 1) image))
               (t2 (sierpinski-helper p2 (midpoint p1 p2) (midpoint p2 p3) (- depth 1) t1))
               (t3 (sierpinski-helper p1 (midpoint p1 p2) (midpoint p3 p1) (- depth 1) t2))
               )
          t3)))                                
  (sierpinski-helper p1 p2 p3 depth background))
(sierpinski-triangle (cons 100 0) (cons 0 300) (cons 400 400) 7)
(sierpinski-triangle (cons 200 0) (cons 0 350) (cons 400 350) 5)


'koch
(define (koch-curve p1 p2 depth image)
  (if (= depth 0)
      (draw-line p1 p2 "black" image)
      (let* ((a (point-at-fraction p1 p2 1/3))
             (b (point-at-fraction p1 p2 2/3))
             (peak (rotate-point b a (- (/ pi 3)))))
        (koch-curve p1 a (- depth 1)
                    (koch-curve a peak (- depth 1)
                                (koch-curve peak b (- depth 1)
                                            (koch-curve b p2 (- depth 1) image)))))))

(koch-curve (cons 0 200) (cons 300 200) 1 (rectangle 300 300 "solid" "white"))
(koch-curve (cons 0 50) (cons 200 300) 1 (rectangle 300 300 "solid" "white"))

(define (koch-snowflake center size depth)
  (let* ((p1 (cons (car center) (- (cdr center) size)))
         (p2 (rotate-point p1 center (/ (* 2 pi) 3)))
         (p3 (rotate-point p1 center (/ (* 4 pi) 3)))
         (background (rectangle (* 2 size) (* 2 size) "solid" "white")))
    (koch-curve p1 p2 depth
                (koch-curve p2 p3 depth
                            (koch-curve p3 p1 depth
                                        background)))))
;  (p2 (cons 0 100)) (p3 (cons 0 200))             
(koch-snowflake (cons 100 100) 100 6)


'L-systems
(define rules1 (list (cons #\F "F+F-F")))
(define rules2 (list (cons #\F "FF") (cons #\X "F+X")))
(define (apply-rule c rules)
  (foldr (lambda (rule acc) (if (eq? c (car rule))
                                (cdr rule)
                                acc))
         (string c)
         rules))

(apply-rule #\F rules1)  ; => "F+F-F"
(apply-rule #\+ rules1)  ; => "+"
(apply-rule #\- rules1)  ; => "-"
(apply-rule #\X rules1)  ; => "X"

(apply-rule #\F rules2)  ; => "FF"
(apply-rule #\X rules2)  ; => "F+X"
(apply-rule #\+ rules2)  ; => "+"

(define (l-system-step s rules)
  (let ((lst (string->list s)))
    (foldr (lambda (value acc) (string-append (apply-rule value rules) acc)) "" lst)))

'l-system-steps
(l-system-step "F" (list (cons #\F "FF")))           ; => "FF"
(l-system-step "F+F" (list (cons #\F "FF")))         ; => "FF+FF"
(l-system-step "F-F-F" (list (cons #\F "FF")))       ; => "FF-FF-FF"
(l-system-step "+" (list (cons #\F "FF")))           ; => "+"
(l-system-step "" (list (cons #\F "FF")))            ; => ""
(l-system-step "FX" (list (cons #\F "FF") (cons #\X "FXF")))  ; => "FFFXF"
(l-system-step "F+F" (list (cons #\F "F-F") (cons #\+ "-")))  ; => "F-F-F-F"

(define (l-system-generate axiom rules iterations)
  (if (= iterations 0)
      axiom
      (l-system-generate (l-system-step axiom rules) rules (- iterations 1))))

'l-system-generate-tests
(l-system-generate "F" (list (cons #\F "F+F")) 0)  ; => "F"
(l-system-generate "F" (list (cons #\F "F+F")) 1)  ; => "F+F"
(l-system-generate "F" (list (cons #\F "F+F")) 2)  ; => "F+F+F+F"
(l-system-generate "F" (list (cons #\F "F+F")) 3)  ; => "F+F+F+F+F+F+F+F"
(l-system-generate "F" (list (cons #\F "FF")) 0)   ; => "F"
(l-system-generate "F" (list (cons #\F "FF")) 1)   ; => "FF"
(l-system-generate "F" (list (cons #\F "FF")) 2)   ; => "FFFF"
(l-system-generate "F" (list (cons #\F "FF")) 3)   ; => "FFFFFFFF"
(l-system-generate "X" (list (cons #\X "XY") (cons #\Y "X")) 0)  ; => "X"
(l-system-generate "X" (list (cons #\X "XY") (cons #\Y "X")) 1)  ; => "XY"
(l-system-generate "X" (list (cons #\X "XY") (cons #\Y "X")) 2)  ; => "XYX"
(l-system-generate "X" (list (cons #\X "XY") (cons #\Y "X")) 3)  ; => "XYXXY"

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

(define (interpret-l-system str turtle step-size turn-angle stack background)
  (if (string=? str "")
      background
      (let ((first-character (string-ref str 0))
            (rest-str (substring str 1)))
        (cond
          [(or (eq? first-character #\F) (eq? first-character #\G))
           (let ((new-turtle (turtle-forward turtle step-size)))
             (interpret-l-system rest-str
                                 new-turtle
                                 step-size
                                 turn-angle
                                 stack
                                 (draw-line (turtle-point turtle) (turtle-point new-turtle) "black" background)))]
          [(eq? first-character #\+)
           (let ((new-turtle (turtle-turn turtle turn-angle)))
             (interpret-l-system rest-str
                                 new-turtle
                                 step-size
                                 turn-angle
                                 stack
                                 background))]
          [(eq? first-character #\-)
           (let ((new-turtle (turtle-turn turtle (- turn-angle))))
             (interpret-l-system rest-str
                                 new-turtle
                                 step-size
                                 turn-angle
                                 stack
                                 background))]
          [(eq? first-character #\[)
           (interpret-l-system rest-str
                               turtle
                               step-size
                               turn-angle
                               (cons turtle stack)
                               background)]
          [(eq? first-character #\])
           (interpret-l-system rest-str
                               (car stack)
                               step-size
                               turn-angle
                               (cdr stack)
                               background)]
          [else (interpret-l-system rest-str turtle step-size turn-angle stack background)]))))

'l-system-examples
(define (draw-l-system axiom rules iterations step-size turn-angle x y a)
  (let ((str (l-system-generate axiom rules iterations)))
    (interpret-l-system str (make-turtle x y a) step-size turn-angle '() (rectangle 300 300 "solid" "white"))))

(define square-koch (draw-l-system
                     "F"
                     (list (cons #\F "F-F+F+F-F"))
                     3
                     10
                     (/ pi 2)
                     20
                     200
                     0))
square-koch

(define sierpinski (draw-l-system
                     "F-G-G"
                     (list (cons #\F "F-G+F+G-F") (cons #\G "GG"))
                     3
                     10
                     (/ (* 2 pi) 3)
                     20
                     200
                     0))
sierpinski

(define fractal-plant (draw-l-system
                       "X"
                       (list (cons #\X "F+[[X]-X]-F[-FX]+X") (cons #\F "FF"))
                       4
                       6
                       (/ (* 25 pi) 180)
                       20 200 0))
fractal-plant

(define dragon (draw-l-system
                "FX"
                (list (cons #\X "X+YF") (cons #\F "FX-Y"))
                4
                10
                (/ pi 2)
                200 200 0))
dragon