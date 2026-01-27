#lang racket

(provide (all-defined-out))

(require 2htdp/image)
(require 2htdp/universe)

;; High definition
(define SCENE_HEIGHT 720)
(define SCENE_WIDTH  1280)

(define BACKGROUND (empty-scene SCENE_WIDTH SCENE_HEIGHT))

;; Base Classes
(define scene-obj-interface
  (interface ()
    [render (-> image?)] ;; Should return an image that is SCENE_WIDTH * SCENE_HEIGHT (based on background)
    [update (-> void?)]  ;; Not sure if this is right. It changes the obj itself
    )
  )


;; TODO: Move these to args
(define CAR_COLOR "dodger blue")



;; Simple show for debug
(define (show img)
  (big-bang 0 (to-draw (lambda (w) img)))
  )
