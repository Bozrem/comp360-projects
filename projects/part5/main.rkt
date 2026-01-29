#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "defs.rkt")
(require "road.rkt")
;; (require "car.rkt")

(struct scene (objs x-cam))

(define INIT_SCENE
  (scene
    (list
      (new road-base% [z-pos 200] [x-world 0]) ;; x doesn't matter
      )
    SCENE_WIDTH ;; Initial x-cam
    )
  )

;; high-level renderer
(define (render-scene s)
  (define (render-objects objs x-cam)
    (cond
      [(empty? objs)  (get-background)]
      [else           (send (first objs) render x-cam (render-objects (rest objs) x-cam))]
      )
    )

  (render-objects (scene-objs s) (scene-x-cam s))
  )


;; high-level updater
(define (update-scene s)
  (define (update-objects objs)
    (cond
      [(empty? objs) empty]
      [else 
        (send (first objs) update) ;; On it's own line because it's a side effect of sorts
        (cons (first objs) (update-objects (rest objs)))
        ]
      )
    )

  (scene (update-objects (scene-objs s)) (+ (scene-x-cam s) CAM_SPEED)) ;; Update the objects and the camera pos
  )


;; Be able to quit with just q instead of ctrl+c
(define (handle-key s key)
  (cond
    ;; Quitting
    [(string=? key "q")   (stop-with s)]

    ;; Camera height adjust
    [(string=? key "w")   (begin (CAMERA_HEIGHT (+ (CAMERA_HEIGHT) 10)) s)]
    [(string=? key "s")   (begin (CAMERA_HEIGHT (- (CAMERA_HEIGHT) 10)) s)]
 
    ;; Camera tilt
    [(string=? key "up")    (begin (HORIZON_HEIGHT (+ (HORIZON_HEIGHT) 10)) s)]
    [(string=? key "down")  (begin (HORIZON_HEIGHT (- (HORIZON_HEIGHT) 10)) s)]

    ;; Zoom
    [(string=? key "=")   (begin (FOCAL_LENGTH (+ (FOCAL_LENGTH) 10)) s)] ;; = is just + without having to shift
    [(string=? key "-")   (begin (FOCAL_LENGTH (- (FOCAL_LENGTH) 10)) s)] ;; 

    [else   s]
    )
  )

;; Scene loop handler
(big-bang INIT_SCENE
  (to-draw render-scene)
  (on-tick update-scene 0.5)
  (on-key  handle-key)
  (close-on-stop #t) ;; lets handle quit work
  )

