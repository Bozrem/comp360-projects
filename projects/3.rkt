#lang racket

(provide (all-defined-out))

(require racket/class)
(require 2htdp/image)
(require 2htdp/universe)

(struct position (x y) #:mutable)
(struct velocity (dxdt dydt) #:mutable)
(struct forceDir (d2xdt2 d2ydt2))

;;; 1: Particle Basics
(define particle%
  (class object%
    (super-new)

    (define SIZE 5)

    (init-field p) ;; Public and mutable
    (init-field v)
    (init-field lifetime)
    (define t 0.0) ;; Private and mutable


    (define/public (update! dt active-forces)
      (define (compute-forces f)
        (cond
          [(empty? f) empty]
          [else       (cons ((first f) p v) (compute-forces (rest f)))]
        )
      )

      (define (update-velocity! force-values)
        (define (apply-force f)
          (set-velocity-dxdt! v (+ (velocity-dxdt v) (* (forceDir-d2xdt2 f) dt)))
          (set-velocity-dydt! v (+ (velocity-dydt v) (* (forceDir-d2ydt2 f) dt)))
        )
        (for-each apply-force force-values)
      )

      (define (update-position!)
        (set-position-x! p (+ (position-x p) (* (velocity-dxdt v) dt)))
        (set-position-y! p (+ (position-y p) (* (velocity-dydt v) dt)))
      )

      (update-velocity! (compute-forces active-forces))
      (update-position!)
      (set! t (+ t dt))
    )


    (define/public (alive?)
      (< t lifetime))


    (define/private (draw)
      (define perc-age  (/ t lifetime))
      (define alpha     (max 0 (* (- 1 perc-age) 255))) ;; To avoid negative alphas
      (define c         (color 0 0 0 (exact-floor alpha)))

      (circle SIZE "solid" c)
    )


    (define/public (draw-on background)
      (define img (draw))
      (place-image/align img (position-x p) (position-y p) "center" "center" background)
    )
  )
)


; 2: Closures

; make-spawner
(define (make-spawner x y d-min d-max life)
  (define (rand-d) (+ d-min (random (- d-max d-min))))

  (lambda ()
    (new particle%
         [p (position x y)]
         [v (velocity (rand-d) (rand-d))]
         [lifetime life]
    )
  )
)


(define (make-gravity val)
  (lambda (pos vel) (forceDir 0 val))
  )


(define (make-wind d2xdt2 d2ydt2)
  (lambda (pos vel) (forceDir d2xdt2 d2ydt2))
  )


; make-friction
(define (make-friction coeff)
  (lambda (pos vel) (forceDir (* (velocity-dxdt vel) coeff -1) (* (velocity-dydt vel) coeff -1)))
  )

;; Gemini defined the math I used in this one
(define (make-air-resistance coeff)
  (lambda (pos vel)
    (define vx (velocity-dxdt vel))
    (define vy (velocity-dydt vel))

    (define speed (sqrt (+ (* vx vx) (* vy vy)))) ;; Speed = sqrt(vx^2 + vy^2)

    (forceDir (* -1 coeff speed vx) (* -1 coeff speed vy))
    )
  )


; make-attractor
(define (make-attractor ax ay strength)
  (lambda (pos vel)
    (define px (position-x pos))
    (define py (position-y pos))

    ;; Need to define the vector pointing from px, py to ax, ay
    (define dx (- ax px))
    (define dy (- ay py))
    ;; Then figure out distance from px, py to ax, ay
    (define dist (sqrt (+ (* dx dx) (* dy dy))))
    (define safe-dist (max dist 1.0)) ;; Gemini suggested to avoid paricles shooting off as force approaches inf

    ;; Normalize
    (define dx-normalized (/ dx safe-dist)) ;; With the idea that dist becomes 1
    (define dy-normalized (/ dy safe-dist))

    (define force-mag (/ strength (* safe-dist safe-dist))) ;; Inverse square
    (forceDir (* dx-normalized force-mag) (* dy-normalized force-mag))
    )
  )


; 3: Tail Recursion
; filter-alive
(define (filter-alive particles)
  (define (internal unchecked verified)
    (cond
      [(empty? unchecked)             verified]
      [(send (car unchecked) alive?)  (internal (cdr unchecked) (cons (car unchecked) verified))]
      [else                           (internal (cdr unchecked) verified)] ;; Not alive, don't add
      )
    )

  (internal particles empty) ;; Technically this also reverses the list, but I don't think that's a problem
  )


; draw-all-particles
(define (draw-all-particles particles background)
  (cond
    [(empty? particles) background]
    [else               (draw-all-particles (cdr particles) (send (car particles) draw-on background))]
    )
  )



; 4: Simulation!
(define (make-burst-spawner particle-maker n-per-frame)
  (lambda ()
    (build-list n-per-frame (lambda (i) (particle-maker)))))


(define (make-delay-spawner particle-maker n-frames)
  (define frames-since-last 0) ;; This becomes a state of the lambda, weird

  (lambda ()
    (set! frames-since-last (add1 frames-since-last))
    (cond
      [(>= frames-since-last n-frames)
        (set! frames-since-last 0)
        (list (particle-maker))]
      [else empty]
      )
    )
  )




; simulation-step
(define simulation%
  (class object%
    (super-new)

    (init-field dt)
    (init-field fps)
    (init-field background)
    (init-field [particles empty]) ;; Optional

    (define spawners empty)
    (define global-forces empty)
    (define t 0.0)

    (define tick_time (/ 1.0 fps))


    (define/public (add-spawner! s)
      (set! spawners (cons s spawners))
    )


    (define/public (add-force! f)
      (set! global-forces (cons f global-forces))
    )


    (define/private (apply-spawners!)
      (define new-particles (append-map (lambda (s) (s)) spawners)) ;; Append map runs and appends them all into one list
      (set! particles (append new-particles particles))
    )


    (define/public (step!)
      (apply-spawners!)
      (set! particles (filter-alive particles))
      (for-each (lambda (p) (send p update! dt global-forces)) particles)
      (set! t (+ t dt))
      this
    )


    (define/public (run)
      (big-bang this
        [on-tick (lambda (sim) (send sim step!)) tick_time]
        [to-draw (lambda (sim) (draw-all-particles particles background))]
      )
    )
  )
)
