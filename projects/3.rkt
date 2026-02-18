#lang racket

(require racket/class)
(require 2htdp/image)
(require 2htdp/universe)

;;; 1: Particle Basics
(define particle%
  (class object%
    (super-new)

    (define SIZE 5)

    (init-field x)
    (init-field y)
    (init-field dxdt)
    (init-field dydt)
    (init-field lifetime) ;; Public and mutable
    (define age 0.0) ;; Private and mutable

    (define/public (update! dt)
      (set! x (+ x (* dxdt dt)))
      (set! y (+ y (* dydt dt)))
      (set! age (+ age dt))
    )


    (define/public (alive?)
      (< age lifetime))


    (define/private (draw)
      (define perc-age  (/ age lifetime))
      (define alpha     (max 0 (* (- 1 perc-age) 255))) ;; To avoid negative alphas
      (define c         (color 0 0 0 (exact-floor alpha)))

      (circle SIZE "solid" c)
    )


    (define/public (draw-on background)
      (define img (draw))
      (place-image/align img x y "center" "center" background)
    )
  )
)


; 2: Closures

; make-spawner

; tests


; make-gravity

; tests


; make-wind

; tests


; make-friction

; tests


; make-attractor

; tests


; compose-forces

; tests


; 3: Tail Recursion

; update-all-particles

; tests


; apply-force-to-all

; tests


; filter-alive

; tests


; draw-all-particles

; tests


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
;
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
