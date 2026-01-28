#lang racket
(require 2htdp/image)

;;; Part 1
(define (colored-circle r c)
  (circle r "solid" c))

(colored-circle 30 "purple")

(define (bullseye-simple r)
  (overlay (colored-circle (/ r 2) "white")
           (colored-circle r "red")))

(bullseye-simple 100)

(define (rgb-square r g b s)
  (rectangle s s "solid" (make-color r g b)))

(rgb-square 10 100 200 40)

(define (grayscale-square g s)
  (rectangle s s "solid" (make-color g g g)))

(grayscale-square 170 60)
(grayscale-square 240 60)


;;; Part 2

(define top (circle 20 "solid" "red"))
(define middle (circle 20 "solid" "yellow"))
(define bottom (circle 20 "solid" "green"))
(define background (rectangle 50 160 "solid" "brown"))
(define (traffic-light)
  (overlay (overlay middle (overlay/offset bottom 0 -100 top)) background)); middle bottom background))
(traffic-light)


(define (roof size)
  (triangle (* size 1.2) "solid" (make-color 20 180 50)))
(define (base size)
  (rectangle size size "solid" (make-color 180 20 50)))
(define (simple-house size)
  (overlay/offset (roof size) 0 size (base size)))
(simple-house 50)
(simple-house 80)

(define leaves (overlay/offset (triangle 40 "solid" "green") 0 20
                               (triangle 40 "solid" "green")))
(define (trunk height)
  (rectangle 20 height "solid" "brown"))
(define (tree height)
  (overlay/offset leaves 0 (* height 0.7) (trunk height)))
(tree 100)
(tree 60)

(define (make-car color)
  (define window (rectangle 40 20 "solid" "lightblue"))
  (define body (rectangle 150 50 "solid" color))
  (define wheels (overlay/offset (circle 15 "solid" "gray") 100 0
                                 (circle 15 "solid" "gray")))
  (overlay/offset (overlay/offset window -50 10 body) 0 30 wheels))
(make-car "red")


;;; Part 3

(define (color->make-color color-list)
  (make-color (car color-list) ; R
              (car (cdr color-list)) ; G
              (car (cdr (cdr color-list))))) ; B
(color->make-color '(30 40 50))

(define (darker color-list by)
  (make-color (floor (* (car by) (car color-list))) ; R
              (floor (* (car (cdr by)) (car (cdr color-list)))) ; G
              (floor (* (car (cdr (cdr by))) (car (cdr (cdr color-list))))))) ; B

(darker '(30 40 50) '(0.8 0.8 0.8))

(define (blend c1 c2)
  (define R (/ (+ (car c1) (car c2)) 2))
  (define G (/ (+ (car (cdr c1)) (car (cdr c2))) 2))
  (define B (/ (+ (car (cdr (cdr c1))) (car (cdr (cdr c2)))) 2))
  (make-color R G B))

(blend '(0 100 0) '(100 0 200))

(define (get-x p) (car p))
(define (get-y p) (cdr p))

(define (place-image-at img p bg)
  (place-image img (get-x p) (get-y p) bg))

(place-image-at (make-car "green") '(10 . 100) (rectangle 200 200 "solid" "white"))

(define (place-all points img background)
  (cond ((null? points) background)
        ;        ((= (length points) 1) img)
        (#t (place-image-at img
                            (car points)
                            (place-all (cdr points) img background)))))
(define star-img (star 20 "solid" "gold"))
(define bg (rectangle 300 200 "solid" "navy"))
(define points (list (cons 50 50) (cons 150 100) (cons 250 50)))
(place-all points star-img bg)

; Part 4

(define (row-of n img)
  (if (= n 1)
      img
      (overlay/align/offset "left" "middle" img 50 0 (row-of (- n 1) img)) ))

(row-of 5 (circle 20 "solid" "red"))

(define (column-of n img)
  (if (= n 1)
      img
      (overlay/align/offset "middle" "top" img 0 50 (column-of (- n 1) img)) ))

(column-of 5 (circle 20 "solid" "green"))

(define (grid-of rows cols img)
  (if (= rows 1)
      (row-of cols img)
      (overlay/align/offset "middle" "top" (row-of cols img) 0 50
                            (grid-of (- rows 1) cols img))))

(grid-of 6 6 (star 10 "solid" "yellow"))


(define (forest number-of-trees)
  (define (row-of-random n img)
    (if (= n 1)
        img
        (overlay/align/offset "left" "middle"
                              img
                              (+ (* (random) 40) 20)
                              (* (random) 15)
                              (row-of-random (- n 1) img)) ))
  (row-of-random number-of-trees (tree (+ (* (random) 40) 40))))

(forest 10)

; Part 5: My Scene
; this will be a series of nested squares of darker and darker colors
; each slightly rotated but inscribed in the next biggest square


(define (scene width depth color rotation)
  ; scale-factor is a function written by me but
  ; mathematically described by Claude AI
  (define scale-factor (/ 1 (+
                          (cos (degrees->radians (abs rotation)))
                          (sin (degrees->radians (abs rotation))))))
  (define new-width (* width scale-factor))
  (displayln scale-factor)
  (define (color->color-list c)
    (list (color-red c) (color-green c) (color-blue c)))
  (if (= depth 0)
      (rectangle width width "solid" color)
      (overlay (rotate rotation (scene new-width
                                  (- depth 1)
                                  (darker (color->color-list color) '(0.7 0.7 0.95)) rotation))
               (rectangle width width "solid" color))))

(scene 200 6 (make-color 100 100 255) -15)