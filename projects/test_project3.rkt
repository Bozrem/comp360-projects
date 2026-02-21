#lang racket

;; NOTE: WHOLLY AI GENERATED

(require rackunit)
(require rackunit/text-ui)
(require racket/class)
(require "3.rkt")

;; Suite 1: Mutable Struct Tests
(define-test-suite struct-suite
  (test-case "Position mutability"
    (define pos (position 0 0))
    (set-position-x! pos 10.5)
    (set-position-y! pos -5.0)
    (check-equal? (position-x pos) 10.5 "X should update")
    (check-equal? (position-y pos) -5.0 "Y should update"))
  
  (test-case "Velocity mutability"
    (define vel (velocity 1 2))
    (set-velocity-dxdt! vel 3)
    (set-velocity-dydt! vel 4)
    (check-equal? (velocity-dxdt vel) 3 "dxdt should update")
    (check-equal? (velocity-dydt vel) 4 "dydt should update")))

;; Suite 2: Closure Generators
(define-test-suite forces-suite
  (test-case "make-gravity"
    (define gravity (make-gravity 9.8))
    (define dummy-pos (position 0 0))
    (define dummy-vel (velocity 0 0))
    (define force (gravity dummy-pos dummy-vel))
    (check-equal? (forceDir-d2xdt2 force) 0 "Gravity should not affect X")
    (check-equal? (forceDir-d2ydt2 force) 9.8 "Gravity should apply to Y"))

  (test-case "make-wind"
    (define wind (make-wind 2.5 -1.0))
    (define dummy-pos (position 10 10))
    (define dummy-vel (velocity 5 5))
    (define force (wind dummy-pos dummy-vel))
    (check-equal? (forceDir-d2xdt2 force) 2.5 "Wind should apply to X")
    (check-equal? (forceDir-d2ydt2 force) -1.0 "Wind should apply to Y"))

  (test-case "make-friction"
    ;; Testing with a negative coefficient to simulate opposition to movement
    (define friction (make-friction 0.5)) 
    (define dummy-pos (position 0 0))
    (define dummy-vel (velocity 10 -4))
    (define force (friction dummy-pos dummy-vel))

    (check-equal? (forceDir-d2xdt2 force) -5.0 "Friction X should be vel-x * coeff")
    (check-equal? (forceDir-d2ydt2 force) 2.0  "Friction Y should be vel-y * coeff"))

  (test-case "make-air-resistance"
    (define air-res (make-air-resistance 0.1))
    (define dummy-pos (position 0 0))

    ;; A 3-4-5 right triangle yields a clean integer speed of 5
    (define dummy-vel (velocity 3 4)) 
    (define force (air-res dummy-pos dummy-vel))

    ;; F_x = -1 * 0.1 * 5 * 3 = -1.5
    ;; F_y = -1 * 0.1 * 5 * 4 = -2.0
    (check-equal? (forceDir-d2xdt2 force) -1.5 "Air resistance X calculation")
    (check-equal? (forceDir-d2ydt2 force) -2.0 "Air resistance Y calculation"))

  (test-case "make-attractor (normal distance)"
    ;; Using a 3-4-5 triangle: dist = 5, safe-dist = 5
    (define attractor (make-attractor 3 4 250)) 
    (define dummy-pos (position 0 0))
    (define dummy-vel (velocity 0 0)) ;; Velocity shouldn't matter here
    (define force (attractor dummy-pos dummy-vel))

    ;; dx-norm = 3/5 = 0.6, dy-norm = 4/5 = 0.8
    ;; force-mag = 250 / (5^2) = 10
    ;; F_x = 0.6 * 10 = 6.0
    ;; F_y = 0.8 * 10 = 8.0
    (check-within (forceDir-d2xdt2 force) 6.0 0.001 "Attractor X calculation")
    (check-within (forceDir-d2ydt2 force) 8.0 0.001 "Attractor Y calculation"))

  (test-case "make-attractor (safe-dist clamping)"
    ;; Distance is 0.5, which is less than 1.0, triggering safe-dist logic
    (define attractor (make-attractor 0.5 0 100))
    (define dummy-pos (position 0 0))
    (define dummy-vel (velocity 0 0))
    (define force (attractor dummy-pos dummy-vel))

    ;; dx = 0.5. safe-dist = 1.0.
    ;; dx-norm = 0.5 / 1.0 = 0.5
    ;; force-mag = 100 / (1.0^2) = 100
    ;; F_x = 0.5 * 100 = 50.0
    (check-within (forceDir-d2xdt2 force) 50.0 0.001 "Clamped attractor X calculation")
    (check-within (forceDir-d2ydt2 force) 0.0 0.001 "Clamped attractor Y calculation")))

;; Suite 3: Particle Lifecycle
(define-test-suite lifecycle-suite
  (test-case "alive? respects lifetime and dt updates"
    (define p (new particle% 
                   [p (position 0 0)] 
                   [v (velocity 0 0)] 
                   [force-funcs empty] 
                   [lifetime 2.0]))
    
    (check-true (send p alive?) "Particle starts alive")
    
    (send p update! 1.0)
    (check-true (send p alive?) "Particle alive at t=1.0")
    
    (send p update! 1.0)
    (check-false (send p alive?) "Particle dead at t=2.0 (t < lifetime is false)")
    
    (send p update! 0.5)
    (check-false (send p alive?) "Particle remains dead after lifetime exceeded")))

;; Suite 4: Particle Physics and Integration
(define-test-suite physics-suite
  (test-case "update! with zero forces (constant velocity)"
    (define p (new particle% 
                   [p (position 0 0)] 
                   [v (velocity 10 -5)] 
                   [force-funcs empty] 
                   [lifetime 5.0]))
    (send p update! 1.0)
    
    (define current-pos (get-field p p))
    (define current-vel (get-field v p))
    
    (check-equal? (position-x current-pos) 10.0 "Position X should be x + vx*dt")
    (check-equal? (position-y current-pos) -5.0 "Position Y should be y + vy*dt")
    (check-equal? (velocity-dxdt current-vel) 10 "Velocity X remains unchanged")
    (check-equal? (velocity-dydt current-vel) -5 "Velocity Y remains unchanged"))

  (test-case "update! with forces applied"
    (define gravity (make-gravity 10))
    (define p (new particle% 
                   [p (position 0 0)] 
                   [v (velocity 0 0)] 
                   [force-funcs (list gravity)] 
                   [lifetime 5.0]))
    
    ;; Step 1: dt = 1.0. 
    ;; Velocity updates first: vy = 0 + 10(1) = 10. 
    ;; Position updates second: y = 0 + 10(1) = 10.
    (send p update! 1.0)
    (check-equal? (velocity-dydt (get-field v p)) 10.0)
    (check-equal? (position-y (get-field p p)) 10.0)
    
    ;; Step 2: dt = 1.0. 
    ;; Velocity: vy = 10 + 10(1) = 20. 
    ;; Position: y = 10 + 20(1) = 30.
    (send p update! 1.0)
    (check-equal? (velocity-dydt (get-field v p)) 20.0)
    (check-equal? (position-y (get-field p p)) 30.0)))

;; Run all suites
(run-tests struct-suite)
(run-tests forces-suite)
(run-tests lifecycle-suite)
(run-tests physics-suite)
