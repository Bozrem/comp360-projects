#lang racket

;; NOTE: WHOLLY AI GENERATED

(require rackunit)
(require rackunit/text-ui)
(require racket/class)
(require 2htdp/universe)
(require 2htdp/image)
(require "3.rkt")


; visual-check

(define (visual-check description img)
  (displayln "\n--- VISUAL TEST ---")
  (displayln description)
  (displayln "Opening window... Press ANY KEY in the graphic window to close it, then return here.")

  (big-bang img
    [to-draw (lambda (state) state)]
    [on-key (lambda (state key) (stop-with state))]
    [name "Visual Test"])

  (display "Does the image match the description? (y/n): ")
  (flush-output)

  (define response (string-trim (read-line)))
  (cond
    [(string-ci=? response "y") #t]
    [(string-ci=? response "n") #f]
    [else (displayln "Invalid input. Failing test by default.")
     #f]))


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
                   [lifetime 2.0]))
    
    (check-true (send p alive?) "Particle starts alive")
    
    (send p update! 1.0 empty)
    (check-true (send p alive?) "Particle alive at t=1.0")
    
    (send p update! 1.0 empty)
    (check-false (send p alive?) "Particle dead at t=2.0 (t < lifetime is false)")
    
    (send p update! 0.5 empty)
    (check-false (send p alive?) "Particle remains dead after lifetime exceeded")))

;; Suite 4: Particle Physics and Integration
(define-test-suite physics-suite
  (test-case "update! with zero forces (constant velocity)"
    (define p (new particle% 
                   [p (position 0 0)] 
                   [v (velocity 10 -5)] 
                   [lifetime 5.0]))
    (send p update! 1.0 empty)
    
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
                   [lifetime 5.0]))
    
    ;; Step 1: dt = 1.0. 
    (send p update! 1.0 (list gravity))
    (check-equal? (velocity-dydt (get-field v p)) 10.0)
    (check-equal? (position-y (get-field p p)) 10.0)
    
    ;; Step 2: dt = 1.0. 
    (send p update! 1.0 (list gravity))
    (check-equal? (velocity-dydt (get-field v p)) 20.0)
    (check-equal? (position-y (get-field p p)) 30.0)))

;; Suite 5: System Management Tests
(define-test-suite system-suite
  (test-case "filter-alive removes dead particles"
    ;; p1 has lifetime 1.0, p2 has lifetime 5.0
    (define p1 (new particle% [p (position 0 0)] [v (velocity 0 0)] [lifetime 1.0]))
    (define p2 (new particle% [p (position 0 0)] [v (velocity 0 0)] [lifetime 5.0]))
    (define particles (list p1 p2))
    
    ;; Advance time by 2.0 to kill p1
    (send p1 update! 2.0 empty)
    (send p2 update! 2.0 empty)
    
    (define living-particles (filter-alive particles))
    
    (check-equal? (length living-particles) 1 "Only one particle should remain")
    (check-true (send (first living-particles) alive?) "Remaining particle is alive")))

;; Suite 6: Visual Rendering Tests
(define bg (empty-scene 200 200))

(define-test-suite visual-suite
  (test-case "Base Particle Rendering (Opaque)"
    (define p (new particle% 
                   [p (position 100 100)] 
                   [v (velocity 0 0)] 
                   [lifetime 10.0]))
    (define img (send p draw-on bg))
    (check-true 
     (visual-check "You should see a solid black circle in the center of the white canvas." img)))

  (test-case "Alpha Fading Logic (Semi-Transparent)"
    (define p (new particle% 
                   [p (position 100 100)] 
                   [v (velocity 0 0)] 
                   [lifetime 10.0]))
    (send p update! 5.0 empty) 
    (define img (send p draw-on bg))
    (check-true 
     (visual-check "You should see a semi-transparent (gray) circle in the center." img)))

  (test-case "End of Life Transparency (Invisible)"
    (define p (new particle% 
                   [p (position 100 100)] 
                   [v (velocity 0 0)] 
                   [lifetime 10.0]))
    (send p update! 10.0 empty) 
    (define img (send p draw-on bg))
    (check-true 
     (visual-check "You should see a completely blank white canvas (particle is invisible)." img)))

  (test-case "System Rendering (Multiple Particles)"
    (define p1 (new particle% [p (position 50 50)] [v (velocity 0 0)] [lifetime 10.0]))
    (define p2 (new particle% [p (position 100 100)] [v (velocity 0 0)] [lifetime 10.0]))
    (define p3 (new particle% [p (position 150 150)] [v (velocity 0 0)] [lifetime 10.0]))

    ;; Set different ages
    (send p2 update! 5.0 empty)  ; Half faded
    (send p3 update! 9.0 empty)  ; Almost invisible

    (define particles (list p1 p2 p3))
    (define img (draw-all-particles particles bg))

    (check-true 
     (visual-check "You should see three circles diagonally: Top-left is solid black, center is gray, bottom-right is very faint." img))))


(run-tests struct-suite)
(run-tests forces-suite)
(run-tests lifecycle-suite)
(run-tests physics-suite)
(run-tests system-suite)
;(run-tests visual-suite)


;; SIMULATION TESTS


;; Suite 7: Simulation Basics
(define-test-suite simulation-basics-suite
  (test-case "Simulation initialization and single step"
    (define bg (empty-scene 200 200))
    ;; Create a single particle to inject manually
    (define p (new particle% [p (position 0 0)] [v (velocity 10 5)] [lifetime 5.0]))

    ;; Initialize simulation with dt=1.0 and 60 FPS (FPS doesn't affect manual steps)
    (define sim (new simulation% 
                     [dt 1.0] 
                     [fps 60] 
                     [background bg] 
                     [particles (list p)]))

    ;; Execute a single step manually
    (send sim step!)

    (define current-particles (get-field particles sim))
    (define stepped-p (first current-particles))

    (check-equal? (length current-particles) 1 "Particle should still be in the list")
    (check-equal? (position-x (get-field p stepped-p)) 10.0 "Particle X should update based on velocity and dt")
    (check-equal? (position-y (get-field p stepped-p)) 5.0 "Particle Y should update based on velocity and dt"))

  (test-case "Simulation time tracking"
    (define bg (empty-scene 200 200))
    (define sim (new simulation% [dt 0.5] [fps 60] [background bg]))

    (send sim step!)
    (send sim step!)

    ;; t is private, but we can infer the system steps correctly by checking a particle's lifespan
    (define p (new particle% [p (position 0 0)] [v (velocity 0 0)] [lifetime 1.0]))
    (set-field! particles sim (list p))

    (send sim step!) ;; Advances particle by 0.5 (total sim time doesn't affect individual particles, but dt does)
    (check-equal? (length (get-field particles sim)) 1 "Particle alive")

    (send sim step!) ;; Advances particle by another 0.5 (now at 1.0, should die)
    (check-equal? (length (get-field particles sim)) 0 "Dead particle should be filtered out")))


;; Visual Simulation Helper
(define (visual-sim-check description sim)
  (displayln "\n--- VISUAL SIMULATION TEST ---")
  (displayln description)
  (displayln "Opening simulation window... Close the window manually (e.g., click the 'X') to conclude the test and return here.")
  
  ;; This will block the thread until the user closes the window
  (send sim run)

  (display "Did the simulation behave as described? (y/n): ")
  (flush-output)
  
  (define response (string-trim (read-line)))
  (cond
    [(string-ci=? response "y") #t]
    [(string-ci=? response "n") #f]
    [else 
     (displayln "Invalid input. Failing test by default.")
     #f]))

;; Suite 8: Dynamic Visual Simulation Tests
(define sim-bg (empty-scene 400 400))

(define-test-suite visual-sim-suite
  
  (test-case "The Faucet (Delay Spawner + Gravity)"
    (define sim (new simulation% [dt 1.0] [fps 60] [background sim-bg]))
    (define base-maker (make-spawner 200 50 -2 2 100)) ;; Drops from near top-center
    (define faucet-spawner (make-delay-spawner base-maker 10)) ;; 1 particle every 10 frames
    (define gravity (make-gravity 0.5))
    
    (send sim add-spawner! faucet-spawner)
    (send sim add-force! gravity)
    
    (check-true 
     (visual-sim-check 
      "You should see particles drip from the top center one by one and accelerate downwards off the screen." 
      sim)))

  (test-case "The Explosion (Burst Spawner + Friction)"
    (define sim (new simulation% [dt 1.0] [fps 60] [background sim-bg]))
    (define base-maker (make-spawner 200 200 -15 15 50)) ;; High random velocity from center
    (define burst-spawner (make-burst-spawner base-maker 5)) ;; 5 particles per frame
    (define friction (make-friction 0.05)) ;; Slows them down over time
    
    (send sim add-spawner! burst-spawner)
    (send sim add-force! friction)
    
    (check-true 
     (visual-sim-check 
      "You should see a continuous explosion from the center. Particles should shoot out fast but slow down noticeably before fading out." 
      sim)))

  (test-case "The Wind Tunnel (Spawners + Wind + Air Resistance)"
    (define sim (new simulation% [dt 1.0] [fps 60] [background sim-bg]))
    ;; Spawns on middle-left with random velocities mostly biased up/down
    (define base-maker (make-spawner 50 200 -5 5 150)) 
    (define delay-spawner (make-delay-spawner base-maker 2)) 
    
    (define wind (make-wind 0.5 0)) ;; Blows right
    (define drag (make-air-resistance 0.02)) ;; Caps max speed
    
    (send sim add-spawner! delay-spawner)
    (send sim add-force! wind)
    (send sim add-force! drag)
    
    (check-true 
     (visual-sim-check 
      "Particles spawn on the left, get blown to the right by wind, but their speed should visibly cap/stabilize due to air resistance." 
      sim)))

  (test-case "The Orbit (Burst Spawner + Attractor)"
    (define sim (new simulation% [dt 1.0] [fps 60] [background sim-bg]))
    ;; Spawns at top-left with a slight initial rightward velocity to encourage orbiting
    (define base-maker (make-spawner 100 100 0 5 200)) 
    (define delay-spawner (make-delay-spawner base-maker 5))
    
    ;; Strong attractor in the dead center
    (define black-hole (make-attractor 200 200 500)) 
    
    (send sim add-spawner! delay-spawner)
    (send sim add-force! black-hole)
    
    (check-true 
     (visual-sim-check 
      "Particles spawn top-left and should be pulled into a curved orbit around the exact center of the screen." 
      sim))))


;; Suite 9: Orbital Mechanics Math
(define-test-suite orbit-math-suite
  
  (test-case "Ideal circular orbit force calculation"
    ;; To maintain a circular orbit at distance R=100 with velocity V=10, 
    ;; the required centripetal acceleration is a = V^2 / R = 100 / 100 = 1.0
    ;; Inverse square force is F = strength / R^2. 
    ;; Therefore, 1.0 = strength / 10000, so strength must be 10000.
    (define attractor (make-attractor 0 0 10000))
    
    (define p-pos (position 100 0)) ;; 100 units to the right of center
    (define p-vel (velocity 0 10))  ;; Moving perfectly tangentially UP
    (define force (attractor p-pos p-vel))
    
    ;; The force vector should point purely left (-X) towards the origin
    (check-within (forceDir-d2xdt2 force) -1.0 0.001 "Force provides exact centripetal acceleration")
    (check-within (forceDir-d2ydt2 force) 0.0 0.001 "No force along the tangent line"))

  (test-case "Euler integration drift over a single step"
    ;; Setup the perfect orbital conditions from the previous test
    (define p (new particle% [p (position 100 0)] [v (velocity 0 10)] [lifetime 10.0]))
    (define attractor (make-attractor 0 0 10000))
    
    ;; Step forward by dt = 1.0
    (send p update! 1.0 (list attractor))
    
    (define new-x (position-x (get-field p p)))
    (define new-y (position-y (get-field p p)))
    (define new-dist (sqrt (+ (* new-x new-x) (* new-y new-y))))
    
    ;; Euler math breakdown:
    ;; v_new = (0, 10) + (-1, 0)*1.0 = (-1, 10)
    ;; p_new = (100, 0) + (-1, 10)*1.0 = (99, 10)
    ;; new-dist = sqrt(99^2 + 10^2) = sqrt(9801 + 100) = sqrt(9901)
    (check-within new-dist (sqrt 9901) 0.001 "Euler integration inherently alters the orbital radius"))

  (test-case "The slingshot effect (large dt near center)"
    ;; Particle gets close to a strong attractor
    (define p (new particle% [p (position 2 0)] [v (velocity 0 5)] [lifetime 10.0]))
    (define attractor (make-attractor 0 0 500)) ;; strength 500
    
    ;; Force magnitude = 500 / 2^2 = 125.
    ;; v_new_x = 0 + (-125 * 1.0) = -125. 
    (send p update! 1.0 (list attractor))
    
    (define vx (velocity-dxdt (get-field v p)))
    (check-equal? vx -125.0 "Velocity spikes massively when close to the center due to inverse-square law")))



(run-tests orbit-math-suite)
(run-tests visual-sim-suite)
(run-tests simulation-basics-suite)
