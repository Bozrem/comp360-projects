#lang racket
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(provide (all-defined-out))

;; Outside of Dr Racket, images don't show automatically. I need to define a universe to show it
(define (show img)
  (big-bang 0 (to-draw (lambda (w) img)))
  )
;; got a bit of help defining this (https://gemini.google.com/share/a3e3ef55a044)


;;; Part 1: Racket and Image Basics

; 1.1: Colored Circle
(define (colored-circle rad color)
  (circle rad "solid" color)
  )

; 1.2: Bullseye
(define (make-bullseye inrad inr ing inb outrad outr outg outb)
  (overlay
    (circle inrad "solid" (make-color inr ing inb))
    (circle outrad "solid" (make-color outr outg outb))
    )
  )

; 1.3: Colored Square
(define (make-square r g b side)
  (square side "solid" (make-color r g b))
  )

; 1.4: Gray Square
(define (grayscale-square lum side)
  (square side "solid" (make-color lum lum lum))
  )



;;; Part 2: Combining Shapes

; 2.1: Traffic Light
(define (traffic-light)
  (above
    (circle 100 "solid" "red")
    (circle 100 "solid" "yellow")
    (circle 100 "solid" "green")
    )
  )

; 2.2: Simple House
(define (simple-house)
  (above
    (isosceles-triangle 100 120 "solid" "red")
    (square 100 "solid" "brown")
    )
  )

; 2.3: Tree
(define (tree)
  (define trunk (rectangle 50 150 "solid" "brown"))
  (define leaves (isosceles-triangle 200 55 "solid" "medium forest green")) ;; Found the fancy list https://docs.racket-lang.org/draw/color-database___.html
  (define leaves-outline (isosceles-triangle 200 55 "outline" "dark green"))

  (define full-leaves (overlay leaves-outline leaves))

  (overlay/offset full-leaves 0 50 trunk)
  )

; 2.4: Car
(define (make-car color)
  (define wheel (make-bullseye 35 0 0 0 45 100 100 100))

  (define body (polygon (list
        (make-posn 0 280)   ;; Front bumper
        (make-posn 0 220)   ;; Hood front
        (make-posn 100 190) ;; Hood slant ends
        (make-posn 180 190) ;; Base of windshield
        (make-posn 260 150) ;; Top of windshield
        (make-posn 340 150) ;; End of roof
        (make-posn 440 170) ;; End of back window
        (make-posn 500 170) ;; Spoiler
        (make-posn 500 250) ;; Trunk
        (make-posn 430 300) ;; Slope back to tire
        (make-posn 30 300)  ;; Base of car
      )
      "solid"
      color)) ;; Couldn't not use this name

  (define window (polygon (list
        (make-posn 170 200) ;; Base of windshield
        (make-posn 250 160) ;; Top of windshield
        (make-posn 330 160) ;; End of roof
        (make-posn 420 180) ;; End of back window
        (make-posn 410 200)
      )
      "solid"
      "white smoke"))

  (define bw (overlay/offset window -55 45 body))

  (define bw_lwheel (overlay/offset wheel 175 -55 bw))

  (overlay/offset wheel -150 -45 bw_lwheel)
  )



;;; Part 3:

; 3.1: color->make-color
(define (color->make-color rgb)
  (make-color (first rgb) (second rgb) (third rgb))
  )

; 3.2: darker
(define (darker rgb)
  (define (darken val) (inexact->exact (floor (* 0.8 val))))

  (cond
    [(empty? rgb) rgb]
    [else         (cons (darken (first rgb)) (darker (rest rgb)))]
    )
  )

; 3.3: blend
(define (blend rgb_one rgb_two)
  (define (blend_val val_one val_two) (inexact->exact (floor (/ (+ val_one val_two) 2))))

  (cond
    [(empty? rgb_one) empty] ;; Technically I should account for different lengths, but idk exceptions yet
    [else             (cons (blend_val (first rgb_one) (first rgb_two)) (blend (rest rgb_one) (rest rgb_two)))]
    )
  )

; 3.4: get-x and get-y
(define (get-x pair) (car pair))
(define (get-y pair) (cdr pair))

; 3.5: place-image-at
(define (place-image-at img1 point img2)
  (place-image img1 (get-x point) (get-y point) img2)
  )

; 3.6: place-all
(define (place-all points img1 img2)
  (cond
    [(empty? points)  img2]
    [else             (place-image-at img1 (first points) (place-all (rest points) img1 img2))]
    )
  )


; Test all of your functions before moving on!!


;;; Part 4: Recursion with Graphics

; 4.1: Row of Images
(define (row-of num img)
  (cond
    [(= 1 num)  img]
    [else       (above img (row-of (- num 1) img))]
    )
  )

; 4.2: Column of Images
(define (column-of num img)
  (cond
    [(= 1 num)  img]
    [else       (beside img (column-of (- num 1) img))]
    )
  )

; 4.3: Grid of Images
(define (grid-of rows cols img)
  (define row (row-of rows img))

  (column-of cols row)
  )

; 4.4: Forest

;; I want to do sort of scattered rows that make it actually look like you're looking into a forest
;; I'm thinking:
;; 
;;  We start at some small value
;;  That value is the distance scale
;;  Need a function that translates a distance to a scale and y offset
;;  Get a random value for the X position
;;  Start from the back so that they get layered correctly
;;  
;;
(define (forest num-trees)
  (define Y_OFFSET_PER 2.5) ;; The additional Y offset added each tree
  (define TREE (tree)) ;; Just to avoid calling it over and over
  (define IMAGE_HEIGHT (+ (image-height TREE) (* Y_OFFSET_PER num-trees)))
  (define IMAGE_WIDTH 500)

  (define START_SCALE 0.2)

  (define BACKGROUND (rectangle IMAGE_WIDTH IMAGE_HEIGHT 255 "white")) ;; 255 makes it transparent background

  (define (random-x) (* (random) IMAGE_WIDTH))
  (define (scale->y scl)
    (- (/ IMAGE_HEIGHT 2)
      (* Y_OFFSET_PER 
         (/ (- 1 scl) SCALE_PER_TREE)
        )
      )
    )
  ;; _ is the amount of trees I've gone up
  ;; Smaller scales should make a higher Y
  ;; I have the current scale
  ;; The scale traveled so far is 1 - current scale
  ;; If we devide that by how many scales we have per tree, we get how many trees up we are
  ;; We can multiply that to get the Y

  (define SCALE_PER_TREE (/ (- 1 START_SCALE) num-trees))

  (define (add-trees scl)
    (cond
      [(> START_SCALE scl)  BACKGROUND]
      [else                 (place-image (scale scl TREE) (random-x) (scale->y scl) (add-trees (- scl SCALE_PER_TREE)))]
      )
    )

  (add-trees 1.0)
  )



; Test your functions before moving on!!


;;; Part 5: Your Scene

; Make something cool!

;; Idea scratchpad for my scene:
;;
;; I want to use the big bang universe
;; I want to have a continuously moving background of:
;;    Road
;;        Do yellow dashes (rhombuses to give sense of distance)
;;    Sky
;;        Maybe add a cloud thing that generates random cloud shapes
;;    Ground
;;        Can probably just do a plain earthy green, or a brown
;;    Trees
;;        Very similar to the forest
;;        Continual generation
;;        Sparsity value determines trees added per movement
;;
;; The foreground:
;;    Car
;;        I can do my car, and have it bob a little bit up and down for movement
;;        If I want to get really fancy, I can do two stages of tire to roll them
;;
;;    Additional front facing things
;;        Maybe some bushes in front?
;;
;;
;; Strategy:
;;    I'd like to abstract away the rendering to only take in a single object for existing images
;;    An added image should have the following properties
;;        Image
;;        X val
;;        Y val
;;        Age (ticks)
;;
;;    If we keep each type of image in a separate list of just those
;;    we can access the "time since last sent" as just the age of the first in that list
;;
;;    Each type of thing added to game should consist of three parts:
;;        List of currently added objects
;;        A renderer (how to draw each one of those objects)
;;          Given some list of these objects and the background image, draw them onto it
;;        An Updater (how it should change with each tick)
;;          Given a list of objects, update each one

;; I used this chat for some strategizing: https://gemini.google.com/share/fe7102f784b8

(define (draw-driving )
  )
