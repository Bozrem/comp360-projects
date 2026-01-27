#lang racket

(require "2htdp/universe")
(require "defs.rkt")

;; high-level renderer
(define (render-scene objs)
  (cond
    [(empty? objs)  BACKGROUND]
    [else           (overlay (send (first objs) render) (render-scene (rest objs)))]
    )
  )


;; high-level updater
(define (update-scene objs)
  (cond
    [(empty? objs)  empty]
    [else           (cons (send (first objs) update) (update-scene (rest objs)))]
    )
  )


;; Be able to quit with just q instead of ctrl+c
(define (handle-quit objs key)
  (cond
    [(string=? key "q")   (close-on-stop #t)]
    [else                 objs]
    )
  )

;; Scene loop handler
(big-bang driving-scene
  (to-draw render-scene)
  (on-tick update-scene)
  (on-key  handle-quit)
  )

