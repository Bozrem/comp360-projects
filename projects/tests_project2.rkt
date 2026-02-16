#lang racket

(require 2htdp/image)
(require lang/posn)
(require rackunit)
(require rackunit/text-ui)
(require "project2.rkt")

; Idea was mine, but I had gemini grind out the formatting for these
; https://gemini.google.com/share/f358f4c3a78f


;; Suite 1: Midpoint Tests
(define-test-suite midpoint-suite
  (check-equal? (midpoint (cons 0 0) (cons 100 100)) (cons 50 50) "Diagonal from origin")
  (check-equal? (midpoint (cons 0 0) (cons 50 0)) (cons 25 0) "Horizontal segment from origin")
  (check-equal? (midpoint (cons 0 0) (cons 0 80)) (cons 0 40) "Vertical segment from origin")
  (check-equal? (midpoint (cons 10 20) (cons 30 40)) (cons 20 30) "Arbitrary positive segment")
  (check-equal? (midpoint (cons -10 -10) (cons 10 10)) (cons 0 0) "Symmetric across origin")
  (check-equal? (midpoint (cons 5 5) (cons 5 5)) (cons 5 5) "Zero length segment"))

;; Suite 2: Point at Fraction Tests
(define-test-suite point-at-fraction-suite
  (check-equal? (point-at-fraction (cons 0 0) (cons 100 0) 0) (cons 0 0) "Start of segment (t=0)")
  (check-equal? (point-at-fraction (cons 0 0) (cons 100 0) 0.25) (cons 25 0) "Quarter way horizontal")
  (check-equal? (point-at-fraction (cons 0 0) (cons 100 0) 0.5) (cons 50 0) "Midpoint horizontal")
  (check-equal? (point-at-fraction (cons 0 0) (cons 100 0) 0.75) (cons 75 0) "Three-quarters horizontal")
  (check-equal? (point-at-fraction (cons 0 0) (cons 100 0) 1.0) (cons 100 0) "End of segment (t=1)")
  (check-equal? (point-at-fraction (cons 0 0) (cons 100 100) 0.5) (cons 50 50) "Midpoint diagonal")
  (check-equal? (point-at-fraction (cons 0 0) (cons 0 200) 0.25) (cons 0 50) "Quarter way vertical")
  (check-equal? (point-at-fraction (cons 10 10) (cons 50 90) 0.5) (cons 30 50) "Midpoint arbitrary offset")
  (check-equal? (point-at-fraction (cons 100 0) (cons 0 0) 0.5) (cons 50 0) "Midpoint reversed direction"))

;; Suite 3: Rotation Tests (using check-within for tolerance)
(define-test-suite rotate-point-suite
  (check-within (rotate-point (cons 1 0) (cons 0 0) 0) 
                (cons 1.0 0.0) 0.001 "No rotation")
  
  ;; 90-degree rotations around origin
  (check-within (rotate-point (cons 1 0) (cons 0 0) (/ pi 2)) 
                (cons 0.0 1.0) 0.001 "90 deg from (1,0)")
  (check-within (rotate-point (cons 1 0) (cons 0 0) pi) 
                (cons -1.0 0.0) 0.001 "180 deg from (1,0)")
  (check-within (rotate-point (cons 1 0) (cons 0 0) (* 3/2 pi)) 
                (cons 0.0 -1.0) 0.001 "270 deg from (1,0)")
  (check-within (rotate-point (cons 0 1) (cons 0 0) (/ pi 2)) 
                (cons -1.0 0.0) 0.001 "90 deg from (0,1)")

  ;; Larger radius
  (check-within (rotate-point (cons 5 0) (cons 0 0) (/ pi 2)) 
                (cons 0.0 5.0) 0.001 "90 deg radius 5")
  (check-within (rotate-point (cons 10 0) (cons 0 0) pi) 
                (cons -10.0 0.0) 0.001 "180 deg radius 10")

  ;; Rotate around a different center
  (check-within (rotate-point (cons 2 0) (cons 1 0) (/ pi 2)) 
                (cons 1.0 1.0) 0.001 "Rotate (2,0) around (1,0)")
  (check-within (rotate-point (cons 3 1) (cons 1 1) pi) 
                (cons -1.0 1.0) 0.001 "Rotate (3,1) around (1,1)")
  (check-within (rotate-point (cons 10 5) (cons 5 5) (/ pi 2)) 
                (cons 5.0 10.0) 0.001 "Rotate (10,5) around (5,5)")
  (check-within (rotate-point (cons 6 4) (cons 4 4) (/ pi 2)) 
                (cons 4.0 6.0) 0.001 "Rotate (6,4) around (4,4)")

  ;; 60-degree rotation
  (check-within (rotate-point (cons 1 0) (cons 0 0) (/ pi 3)) 
                (cons 0.5 0.866) 0.001 "60 deg rotation")
  (check-within (rotate-point (cons 1 0) (cons 0 0) (- (/ pi 3))) 
                (cons 0.5 -0.866) 0.001 "Negative 60 deg rotation"))


(define rules1 (list (cons #\F "F+F-F")))
(define rules2 (list (cons #\F "FF") (cons #\X "F+X")))
(define rules3 '())

(define-test-suite apply-rule-suite
  (test-case "Rules Set 1: Simple expansion"
    (check-equal? (apply-rule #\F rules1) "F+F-F" "Rule exists for F")
    (check-equal? (apply-rule #\+ rules1) "+" "Default: + stays +")
    (check-equal? (apply-rule #\- rules1) "-" "Default: - stays -")
    (check-equal? (apply-rule #\X rules1) "X" "Default: X stays X"))

  (test-case "Rules Set 2: Multiple rules"
    (check-equal? (apply-rule #\F rules2) "FF" "Rule F -> FF")
    (check-equal? (apply-rule #\X rules2) "F+X" "Rule X -> F+X")
    (check-equal? (apply-rule #\+ rules2) "+" "No rule for +"))

  (test-case "Rules Set 3: Empty rules"
    (check-equal? (apply-rule #\F rules3) "F" "Empty rules returns symbol as string")))

(define-test-suite l-system-step-suite
  (check-equal? (l-system-step "F" (list (cons #\F "FF"))) "FF" "Single char expansion")
  (check-equal? (l-system-step "F+F" (list (cons #\F "FF"))) "FF+FF" "Expand multiple symbols")
  (check-equal? (l-system-step "F-F-F" (list (cons #\F "FF"))) "FF-FF-FF" "Expand with separators")
  (check-equal? (l-system-step "+" (list (cons #\F "FF"))) "+" "No expansion needed")
  (check-equal? (l-system-step "" (list (cons #\F "FF"))) "" "Empty string returns empty")
  (check-equal? (l-system-step "FX" (list (cons #\F "FF") (cons #\X "FXF"))) "FFFXF" "Multiple distinct rules")
  (check-equal? (l-system-step "F+F" (list (cons #\F "F-F") (cons #\+ "-"))) "F-F-F-F" "Symbol transformation"))

(define-test-suite l-system-generate-suite
  (test-case "Koch-like expansion (F -> F+F)"
    (let ([koch-rules (list (cons #\F "F+F"))])
      (check-equal? (l-system-generate "F" koch-rules 0) "F" "Depth 0")
      (check-equal? (l-system-generate "F" koch-rules 1) "F+F" "Depth 1")
      (check-equal? (l-system-generate "F" koch-rules 2) "F+F+F+F" "Depth 2")
      (check-equal? (l-system-generate "F" koch-rules 3) "F+F+F+F+F+F+F+F" "Depth 3")))

  (test-case "Exponential expansion (F -> FF)"
    (let ([exp-rules (list (cons #\F "FF"))])
      (check-equal? (l-system-generate "F" exp-rules 0) "F" "Depth 0")
      (check-equal? (l-system-generate "F" exp-rules 1) "FF" "Depth 1")
      (check-equal? (l-system-generate "F" exp-rules 2) "FFFF" "Depth 2")
      (check-equal? (l-system-generate "F" exp-rules 3) "FFFFFFFF" "Depth 3")))

  (test-case "Complex expansion (X -> XY, Y -> X)"
    (let ([complex-rules (list (cons #\X "XY") (cons #\Y "X"))])
      (check-equal? (l-system-generate "X" complex-rules 0) "X" "Depth 0")
      (check-equal? (l-system-generate "X" complex-rules 1) "XY" "Depth 1")
      (check-equal? (l-system-generate "X" complex-rules 2) "XYX" "Depth 2")
      (check-equal? (l-system-generate "X" complex-rules 3) "XYXXY" "Depth 3"))))



;; Custom check for visual verification
;; NOTE: This is AI Generated, see gemini chat at top of file
(define (check-visual image description)
  (printf "\n--- Visual Inspection ---\n")
  (printf "Expecting: ~a\n" description)
  (printf "Opening image...\n")
  
  ;; Call your show function
  (show image) 
  
  (printf "Does the image look correct? (y/n): ")
  (flush-output)
  
  ;; Read user input and assert it is "y"
  (let ([response (string-trim (read-line))])
    (check-equal? response "y" (format "Visual inspection failed for: ~a" description))))

(define-test-suite line-drawing-suite
  (test-case "Diagonal line"
    (check-visual 
     (draw-line (cons 0 0) (cons 100 100) "black" (rectangle 200 200 "solid" "white"))
     "Black diagonal line from top-left to center"))

  (test-case "Vertical Red Line"
    (check-visual 
     (draw-line (cons 50 0) (cons 50 100) "red" (rectangle 100 100 "solid" "white"))
     "Red vertical line down the middle"))

  (test-case "Horizontal Blue Line"
    (check-visual 
     (draw-line (cons 0 50) (cons 100 50) "blue" (rectangle 100 100 "solid" "white"))
     "Blue horizontal line across the middle"))

  (test-case "Green Offset Line"
    (check-visual 
     (draw-line (cons 10 10) (cons 90 90) "green" (rectangle 100 100 "solid" "gray"))
     "Green diagonal line on a gray background")))

(define-test-suite recursive-fractal-suite
  
  ;; --- Sierpinski Triangle Tests ---
  (test-case "Sierpinski Depth 0"
    (check-visual (sierpinski-triangle 200 0) 
                  "Sierpinski Depth 0: A single solid green triangle"))

  (test-case "Sierpinski Depth 1"
    (check-visual (sierpinski-triangle 200 1) 
                  "Sierpinski Depth 1: Three green triangles forming a larger triangle with a hole"))

  (test-case "Sierpinski Depth 4"
    (check-visual (sierpinski-triangle 400 4) 
                  "Sierpinski Depth 4: Complex recursive triangle pattern"))

  ;; --- Koch Curve Tests ---
  (test-case "Koch Curve Depth 0"
    (check-visual (koch-curve 200 0) 
                  "Koch Curve Depth 0: A single black line"))

  (test-case "Koch Curve Depth 1"
    (check-visual (koch-curve 200 1) 
                  "Koch Curve Depth 1: A line with a triangular 'bump' in the middle"))

  (test-case "Koch Curve Depth 3"
    (check-visual (koch-curve 300 3) 
                  "Koch Curve Depth 3: Detailed fractal edge (coastline look)"))

  ;; --- Koch Snowflake Tests ---
  (test-case "Koch Snowflake Depth 3"
    (check-visual (koch-snowflake 300 3) 
                  "Koch Snowflake Depth 3: A six-pointed star/snowflake shape with fractal edges")))

(define-test-suite l-system-examples-suite
  
  (test-case "Square Koch (L-System)"
    (check-visual 
     (draw-l-system 
      "F" 
      (list (cons #\F "F-F+F+F-F")) 
      3 
      10 
      (/ pi 2) 
      20 200 0)
     "Square Koch: Boxy, geometric fractal curve (90 degree turns)"))

  (test-case "Sierpinski Triangle (L-System)"
    ;; Rules: F-> F-G+F+G-F, G-> GG
    (check-visual 
     (draw-l-system 
      "F-G-G" 
      (list (cons #\F "F-G+F+G-F") (cons #\G "GG")) 
      3
      10 
      (/ (* 2 pi) 3) ; 120 degrees
      20 200 0)
     "L-System Sierpinski: Should look identical to the recursive triangle"))

  (test-case "Fractal Plant"
    ;; Rules: X-> F+[[X]-X]-F[-FX]+X, F-> FF
    (check-visual 
     (draw-l-system 
      "X" 
      (list (cons #\X "F+[[X]-X]-F[-FX]+X") (cons #\F "FF")) 
      4 
      5 
      (* 25 (/ pi 180)) ; 25 degrees converted to radians
      150 200 (/ pi 2)) ; Start pointing up
     "Fractal Plant: Organic looking weed/bush structure"))

  (test-case "Dragon Curve"
    ;; Rules: X-> X+YF, Y-> FX-Y
    (check-visual 
     (draw-l-system 
      "FX" 
      (list (cons #\X "X+YF") (cons #\Y "FX-Y")) 
      8 
      5 
      (/ pi 2) ; 90 degrees
      150 150 0)
     "Dragon Curve: Complex, dense rectangular spiral pattern"))
)





(define-test-suite active-suites
  midpoint-suite
  point-at-fraction-suite
  rotate-point-suite
  apply-rule-suite
  l-system-step-suite
  l-system-generate-suite
  line-drawing-suite
  recursive-fractal-suite
  l-system-examples-suite
  )

(run-tests
  active-suites
  )
