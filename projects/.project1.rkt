#lang racket
(require 2htdp/image)

;;; Part 1: Racket and Image Basics

; 1.1: Colored Circle


; 1.2: Bullseye


; 1.3: Colored Square


; 1.4: Gray Square




; Test your functions before moving on!!


;;; Part 2: Combining Shapes

; 2.1: Traffic Light


; 2.2: Simple House


; 2.3: Tree


; 2.4: Car




; Test your functions before moving on!!


;;; Part 3:

; 3.1: color->make-color


; 3.2: darker


; 3.3: blend


; 3.4: get-x and get-y


; 3.5: place-image-at


; 3.6: place-all




; Test all of your functions before moving on!!


;;; Part 4: Recursion with Graphics

; 4.1: Row of Images


; 4.2: Column of Images


; 4.3: Grid of Images


; 4.4: Forest




; Test your functions before moving on!!


;;; Part 5: Your Scene

; Make something cool!

;;; Example: I used AI for a very small slice of my scene.
;;;          If you want to see my conversation, I am happy to show it to you.

(define (scene width depth color rotation)
  (define (darker color-list by) ; rewritten for more control
    (make-color (floor (* (car by) (car color-list))) ; R
                (floor (* (car (cdr by)) (car (cdr color-list)))) ; G
                (floor (* (car (cdr (cdr by))) (car (cdr (cdr color-list))))))) ; B
  
  ; scale-factor is a function written by me but
  ; mathematically described by Claude AI
  (define scale-factor (/ 1 (+
                             (cos (degrees->radians (abs rotation)))
                             (sin (degrees->radians (abs rotation))))))
  
  (define new-width (* width scale-factor))

  (define (color->color-list c) ; for use with darker
    (list (color-red c) (color-green c) (color-blue c)))
  
  (if (= depth 0) ; main
      (rectangle width width "solid" color)
      (overlay (rotate rotation (scene new-width
                                       (- depth 1)
                                       (darker (color->color-list color) '(0.7 0.7 0.95)) rotation))
               (rectangle width width "solid" color)))
  ; possible extensions:
  ; each square has a random color
  ; each square has a random rotation
  ; the randomness is a tunable parameter: ex, color-randomness ranges from 0 to 1
  ; different shapes
  )


(scene 200 6 (make-color 100 100 255) -15)