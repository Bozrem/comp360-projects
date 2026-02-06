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








(define-test-suite active-suites
  midpoint-suite
  point-at-fraction-suite
  rotate-point-suite
  ;; line-drawing-suite
  )

(run-tests
  active-suites
  )
