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
(define (make-car color wheel-angle)
  (define wheel
    ;(define pen (pen "black" 20 "solid" "butt" "bevel"))

    (rotate wheel-angle
      (overlay
        (circle 40 "outline" (pen "black" 10 "solid" "butt" "bevel"))
        (rectangle 70 5 "solid" "dark gray")
        (rectangle 5 70 "solid" "dark gray")
        (circle 35 "solid" "dim gray")
        )
      )
    )

  ;;(define wheel (make-bullseye 35 0 0 0 45 100 100 100))

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

; Scratchpad:
;   I moved my rendering project to a different repo so that I could worry a little less about AI usage
;   Is there a way I can do an animated driving scene easily here?
;   My main worry is how to do the forest.
;   For the car and road, I can simply make some static amount of images, and switch between them to "animate" motion
;   But for my forest, it's randomly generated.
;
;   I think to start, I'm going to just make a random new forest, and see how bad that looks.

;;; Part 5: Your Scene

(struct state (tick car_pos)) ;; Indivual things can use (mod tick num), where num is how many different images it has

(define HEIGHT 720)
(define WIDTH 1280)
(define horizon 500)

(define BACKGROUND
  (above
    (rectangle WIDTH (- HEIGHT horizon) "solid" "sky blue")
    (rectangle WIDTH horizon "solid" "dark olive green")
    )
  )

(define TRANS_BACKGROUND (rectangle WIDTH HEIGHT 0 "white"))


(define (render-sky tick)
  (define sky
    (scale/xy
      44 1280
      (color-list->bitmap
        (list "midnight blue" "blue" "light sky blue" "dark orange" "orange red")
        5 1
        )
      )
    )

  (place-image
    (circle 50 "solid" "dark orange")
    1000 225
    (rotate 270 sky)
    )
  )


(define (render-road tick)
  ;; 10 stage animation

  ;; Some way to determine dashes
  (define road-base (rectangle WIDTH 500 "solid" "dim gray"))

  ;(define road-dash (rectangle 40 20 "solid" "gold"))

  (define road-dash
    (polygon
      (list
        (make-posn 0 20)
        (make-posn 5 0)
        (make-posn 45 0)
        (make-posn 40 20)
        )
      "solid"
      "gold"
      )
    )

  (define dashes 10)
  (define stages 20)
  (define dist-per-dash (/ WIDTH dashes))
  (define dist-per-tick (/ dist-per-dash stages))

  (define stage (modulo tick stages))

  (define (place-dashes dash)
    ;; The dash count total should define the range within each dash
    ;; It should therefore move through that range within the stages
    ;; But then the reset should line up with the next one resetting to appear infinite

    ;; (dist-per-dash * dash) becomes it's base x
    ;; + (dist-per-tick * stage) becomes offset within the tick
    (define dash-x (+ (* dist-per-dash dash) (- dist-per-dash (* dist-per-tick stage))))

    (cond
      [(> dash dashes)  road-base]
      [else
        (place-image
          road-dash
          dash-x 175
          (place-dashes (+ dash 1))
          )
        ]
      )
    )

  (place-dashes 0)
  )

;; Individual renderers
(define (render-car tick)
  (define rotation-per-tick 7)

  (define wheel-rotation
    (modulo (* tick rotation-per-tick) 360)
    )

  (define car-img (flip-horizontal (make-car "dodger blue" wheel-rotation)))

  car-img
  )


(define (render-trees tick)
  (define TREE (tree))

  (define trees 10)
  (define stages 30)
  (define dist-per-tree (/ WIDTH trees))
  (define dist-per-tick (/ dist-per-tree stages))

  (define stage (modulo tick stages))

  (define (place-trees tree)
    ;; The dash count total should define the range within each dash
    ;; It should therefore move through that range within the stages
    ;; But then the reset should line up with the next one resetting to appear infinite

    ;; (dist-per-dash * dash) becomes it's base x
    ;; + (dist-per-tick * stage) becomes offset within the tick
    (define tree-x (+ (* dist-per-tree tree) (- dist-per-tree (* dist-per-tick stage))))

    (cond
      [(> tree trees)  TRANS_BACKGROUND]
      [else
        (place-image
          TREE 
          tree-x 175
          (place-trees (+ tree 1))
          )
        ]
      )
    )

  (place-trees -1)
  )


(define (render-animation s)
  (define tick (state-tick s))

  (place-image
    (render-car tick)
    300 (+ (state-car_pos s) 500)
    (place-image
      (render-trees tick)
      640 400
      (place-image
        (render-road tick)
        640 600
        (place-image
          (render-sky tick)
          640 110
          BACKGROUND
         )
        )
      )
    )
  )


(define (inc-tick s)
  (state (+ 1 (state-tick s)) (state-car_pos s))
  )



(define (handle-key s key)
  (define curr_tick (state-tick s))
  (define curr_pos  (state-car_pos s))

  (cond
    [(string=? key "s")
      (cond
        [(< curr_pos 100) (state curr_tick (+ curr_pos 5))]
        [else s]
        )
      ]

    [(string=? key "w")
      (cond
        [(> curr_pos -100) (state curr_tick (- curr_pos 5))]
        [else s]
        )
      ]

    [(string=? key "q")   (stop-with s)]
    [else s]
    )
  )


(define init-state (state 0 0))

(big-bang init-state
  (to-draw render-animation)
  (on-tick inc-tick 0.017)
  (on-key  handle-key)
  (close-on-stop #t) ;; lets handle quit work
  )
