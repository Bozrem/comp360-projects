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
    (init-field force-funcs) ;; TODO: Contract to show this is a list of procedures that take in position and velocity
    (init-field lifetime)
    (define t 0.0) ;; Private and mutable


    (define/public (add-force force-func)
      (set! force-funcs (cons force-func force-funcs))
    )


    (define/public (update! dt)
      (define (compute-forces f)
        (cond
          [(empty? f) empty]
          [else       (cons ((first f) p v) (compute-forces (rest f)))]
        )
      ) ;; This generates a list of the current forces

      (define (update-velocity! force-values)
        (define (apply-force f)
          (set-velocity-dxdt! v (+ (velocity-dxdt v) (* (forceDir-d2xdt2 f) dt)))
          (set-velocity-dydt! v (+ (velocity-dydt v) (* (forceDir-d2ydt2 f) dt)))
        )
        (for-each apply-force force-values)
      ) ;; Updates the velocity based on the current forces

      (define (update-position!)
        (set-position-x! p (+ (position-x p) (* (velocity-dxdt v) dt)))
        (set-position-y! p (+ (position-y p) (* (velocity-dydt v) dt)))
      )

      (update-velocity! (compute-forces force-funcs))
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
         [force-funcs empty]
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

; update-all-particles
(define (update-all-particles particles dt)
  (for-each (lambda (p) (send p update! dt)) particles)
  )


; apply-force-to-all
(define (apply-force-to-all force-func particles)
  (for-each (lambda (p) (send p add-force force-func)) particles)
  )


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

; simulation-step


; simulation-step-with-spawning


; run-simulation


; 5: Your Scene!



;;; My Scene: I got extensive help from Claude AI.
;;; even so, the vast majority of this code is my own.
;;; I used Claude for guidance, hints, and debugging.

;(require racket/random)
;(define WIDTH 800)
;(define HEIGHT 800)

;(define (make-random-spawner x y v-min v-max life images)
;  (lambda () (make-particle x y (random v-min v-max) (random v-min v-max) life (car (random-sample images 1)))))
;
;(define shape-makers
;  (list
;   (lambda (a) (circle 5 "solid" (color 0 255 0 a)))
;   (lambda (a) (star 10 "solid" (color 255 255 0 a)))
;   (lambda (a) (rectangle 10 10 "solid" (color 255 0 0 a)))))
;
;(define my-spawner (curryr make-random-spawner -5 5 80 shape-makers))
;
;  
;(define my-forces (compose-forces (make-gravity 0.3) (make-friction 0.90)))
;
;(define (tick-handler state)
;  (list (simulation-step-with-spawning (first state)
;                                       (apply compose-forces (fourth state))
;                                       (my-spawner (second state) (third state)) ; move the spawner according to the state
;                                       5)
;        (second state)
;        (third state)
;        (fourth state)))
;
;
;
;(define (draw-handler state)
;  (draw-all-particles (first state) (rectangle WIDTH HEIGHT "solid" "black")))
;
;(define (normalize p)
;  (let ((magnitude (sqrt (+ (expt (car p) 2) (expt (cdr p) 2)))))
;    (if (zero? magnitude)
;        (cons 0 0)
;        (cons (/ (car p) magnitude) (/ (cdr p) magnitude)))))
;
;(define (make-repeller x y size)
;  (lambda (p)
;    ; calculate the direction from x, y to the particle
;    ; normalize
;    ; push the particle by the normalized * size
;    (let* ((direction (cons (- (particle-x p) x) (- (particle-y p) y)))
;           (normalized (normalize direction)))
;      (make-particle (particle-x p)
;                     (particle-y p)
;                     (+ (particle-vx p) (* (car normalized) size))
;                     (+ (particle-vy p) (* (cdr normalized) size))
;                     (particle-life p)
;                     (particle-image p)))))
;           
;(define (mouse-handler state x y event)
;  (let ((repeller (make-repeller x y 1)))
;    (cond
;      ((equal? event "button-down")
;       (list (first state) x y (cons repeller (fourth state))))
;      ((equal? event "button-up")
;       (list (first state) x y (cdr (fourth state))))
;      (else
;       (list (first state) x y (fourth state))))))
;  
;
;(big-bang (list '() 0 0 (list my-forces))  ; initial state: no particles
;  [on-tick tick-handler]
;  [to-draw draw-handler]
;  [on-mouse mouse-handler])
