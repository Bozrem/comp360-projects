#lang racket

(require "defs.rkt")
(require 2htdp/image)

(provide (all-defined-out))

;; Base Classes
(define scene-obj-interface
  (interface ()
    [render   (->m number? image? image?)] ;; ->m is saying it has to have a method (m) of the following args and return
    [update   (->m void?)]
    [visible? (->m number? boolean?)]
    )
  )

;; NOTE: 
;;  Gemini helped substantially in three aspects:
  ;;  1. Helping me understand Racket classes and syntax
  ;;  2. Explaining the graphics concepts and math to make rendering work
  ;;  3. Some general idea help with the OOP design
  ;;  .
  ;;  Gemini provided no assistance in any code writing or debugging. That was (painfully) on my own
  ;;  .
  ;;  https://gemini.google.com/share/b3bc3a9c6f2f

;; This should handle all of the rendering logic
;; so that the implementations (individual objects) only need to define the image it's using
;; and define any special way it gets updated
(define base-obj%
  (class* object% (scene-obj-interface)
    (super-new)

    (init-field z-pos) ;; Required
    (init-field x-world) ;; Just the x-camera unless this is init
    (field [scl (/ FOCAL_LENGTH (+ FOCAL_LENGTH z-pos))])

    (field [img empty-image]) ;; To be set by the implementations

    ;; Can be overriden for dynamic objects
    (define/public (update)
      (void) ;; Does nothing normally
      )

    ;; Can be overriden for background objects (like road)
    (define/public (render x-camera scene)
      (define x-screen-center (/ SCENE_WIDTH 2))
      (define x-screen (+ x-screen-center (* scl (- x-world x-camera)))) ;; (world-x - camera-x) * scale + screen-center

      (define y-screen (- HORIZON_HEIGHT (* scl CAMERA_HEIGHT))) ;; horizon-y + (camera-y * scale)

      (place-image/align
        (scale scl img)
        x-screen y-screen "center" "bottom"
        scene
        )
      )

    ;; To know when objects are out of frame so they can be removed from the list
    ;; TODO:
    ;; This feels incorrect. Gemini wrote the logic, I don't trust it.
    (define/public (visible? x-camera)
      (define x-screen-center (/ SCENE_WIDTH 2))
      (define x-screen (+ x-screen-center (* scl (- x-world x-camera)))) ;; (world-x - camera-x) * scale + screen-center

      (> (+ x-screen (* SCENE_WIDTH scl)) -1)
      )

    )
  )
