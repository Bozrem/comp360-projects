#lang racket

(require 2htdp/image)
(require "defs.rkt")
(require "base.rkt")

(provide road-base%)

(define road-base%
  (class* base-obj% (scene-obj-interface)
    (super-new) ;; Constructor ish

    (inherit-field z-pos)

    (define/override (render x-camera scene)
      (define scl (/ (FOCAL_LENGTH) (+ (FOCAL_LENGTH) z-pos)))
      (define y-screen (- (HORIZON_HEIGHT) (* scl (CAMERA_HEIGHT)))) ;; horizon-y + (camera-y * scale)

      (overlay/align
        "middle" "bottom"
        (rectangle SCENE_WIDTH y-screen "solid" ROAD_COLOR)
        scene
        )
      )
 
    (define/override (visible? x-camera)
      #t ;; Background object. Always visible
      )
    )
  )
