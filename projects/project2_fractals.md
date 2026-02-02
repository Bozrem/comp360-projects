# Project 2: Fractal Explorations

**COMP 360: Programming Languages**

---

## Expected Effort

If you aren't able to complete the project, I expect you to spend around 5 hours and as many as 10 hours. If you can't complete the project within 10 hours, you should let yourself stop working. 

Your effort now will pay off in the future.

---

## AI Assistance Disclosure

**You must disclose use of AI on this assignment.** If you opt to use AI for this project, I will ask to see your conversation history.

---

## Grading and Submission

This project is **due the week of Feb 9**. You can bring me your code anytime that week during my office hours. Grading should take no more than 10 minutes.

Your grade will be based on how much of the project you completed, how much time you spent, and how well you understand your own code.

My solutions will become avaiable on Feb 13.

---

## Overview

In this project, you'll explore fractals using recursion, higher-order functions, and functional programming in Racket. You'll implement multiple fractal generation techniques.

Fractals are geometric patterns that repeat at every scale. They appear throughout nature (ferns, coastlines, snowflakes) and have deep connections to mathematics and computer science. More importantly for us, they're a perfect playground for recursive thinking!

---

## Getting Started

Open `project2.rkt`. You'll see:

```racket
#lang racket
(require 2htdp/image)
```

The `2htdp/image` library provides the graphics primitives you'll need. Key functions you'll use:

- `(add-line img x1 y1 x2 y2 color)` - draw a line on an image
- `(place-image img x y background)` - place an image on a background
- `(circle radius mode color)` - create a circle
- `(rectangle width height mode color)` - create a rectangle
- `(overlay img1 img2)` - layer images
- `(scene+polygon img points mode color)` - draw a filled polygon

---

## Part 1: Foundation - Point and Line Helpers

Before building fractals, you need utility functions for working with points and lines. A **point** is represented as a cons pair: `(cons x y)`.

### Problem 1.1: midpoint

Write a function `midpoint` that takes two points and returns the point exactly between them.

```racket
(midpoint (cons 0 0) (cons 100 100))   ; => (cons 50 50)
(midpoint (cons 0 0) (cons 50 0))      ; => (cons 25 0)
(midpoint (cons 0 0) (cons 0 80))      ; => (cons 0 40)
(midpoint (cons 10 20) (cons 30 40))   ; => (cons 20 30)
(midpoint (cons -10 -10) (cons 10 10)) ; => (cons 0 0)
(midpoint (cons 5 5) (cons 5 5))       ; => (cons 5 5)
```

### Problem 1.2: point-at-fraction

Write a function `point-at-fraction` that takes two points and a fraction `t` (between 0 and 1), returning the point that is `t` of the way from the first point to the second.

```racket
(point-at-fraction (cons 0 0) (cons 100 0) 0)     ; => (cons 0 0)
(point-at-fraction (cons 0 0) (cons 100 0) 0.25)  ; => (cons 25 0)
(point-at-fraction (cons 0 0) (cons 100 0) 0.5)   ; => (cons 50 0)
(point-at-fraction (cons 0 0) (cons 100 0) 0.75)  ; => (cons 75 0)
(point-at-fraction (cons 0 0) (cons 100 0) 1.0)   ; => (cons 100 0)
(point-at-fraction (cons 0 0) (cons 100 100) 0.5) ; => (cons 50 50)
(point-at-fraction (cons 0 0) (cons 0 200) 0.25)  ; => (cons 0 50)
(point-at-fraction (cons 10 10) (cons 50 90) 0.5) ; => (cons 30 50)
(point-at-fraction (cons 100 0) (cons 0 0) 0.5)   ; => (cons 50 0)
```

*Hint:* The formula is: `result = p1 + t * (p2 - p1)`, applied to both x and y components.

### Problem 1.3: rotate-point

Write a function `rotate-point` that takes a point, a center point, and an angle in radians. It returns the point rotated around the center by that angle.

*Note:* Results involve floating-point arithmetic. Values shown are approximate.

```racket
; Rotate around the origin (no rotation)
(rotate-point (cons 1 0) (cons 0 0) 0)            ; => (cons 1.0 0.0)

; 90-degree rotations around origin
(rotate-point (cons 1 0) (cons 0 0) (/ pi 2))     ; => (cons 0.0 1.0) approximately
(rotate-point (cons 1 0) (cons 0 0) pi)           ; => (cons -1.0 0.0) approximately
(rotate-point (cons 1 0) (cons 0 0) (* 3/2 pi))   ; => (cons 0.0 -1.0) approximately
(rotate-point (cons 0 1) (cons 0 0) (/ pi 2))     ; => (cons -1.0 0.0) approximately

; Larger radius
(rotate-point (cons 5 0) (cons 0 0) (/ pi 2))     ; => (cons 0.0 5.0) approximately
(rotate-point (cons 10 0) (cons 0 0) pi)          ; => (cons -10.0 0.0) approximately

; Rotate around a different center
(rotate-point (cons 2 0) (cons 1 0) (/ pi 2))     ; => (cons 1.0 1.0) approximately
(rotate-point (cons 3 1) (cons 1 1) pi)           ; => (cons -1.0 1.0) approximately
(rotate-point (cons 10 5) (cons 5 5) (/ pi 2))    ; => (cons 5.0 10.0) approximately
(rotate-point (cons 6 4) (cons 4 4) (/ pi 2))     ; => (cons 4.0 6.0) approximately

; 60-degree rotation (important for Koch curve!)
(rotate-point (cons 1 0) (cons 0 0) (/ pi 3))     ; => (cons 0.5 0.866) approximately
(rotate-point (cons 1 0) (cons 0 0) (- (/ pi 3))) ; => (cons 0.5 -0.866) approximately
```

*Hint:* The rotation algorithm is:
1. Translate the point so the center is at the origin
2. Apply rotation: `x' = x*cos(angle) - y*sin(angle)`, `y' = x*sin(angle) + y*cos(angle)`
3. Translate back

### Problem 1.4: draw-line

Write a function `draw-line` that takes two points, a color, and a background image. It returns the background with a line drawn between the points.

```racket
; These produce images - visually inspect the output
(draw-line (cons 0 0) (cons 100 100) "black" (rectangle 200 200 "solid" "white"))
(draw-line (cons 50 0) (cons 50 100) "red" (rectangle 100 100 "solid" "white"))
(draw-line (cons 0 50) (cons 100 50) "blue" (rectangle 100 100 "solid" "white"))
(draw-line (cons 10 10) (cons 90 90) "green" (rectangle 100 100 "solid" "gray"))
```

*Hint:* Use `add-line` from `2htdp/image`. Note that `add-line` takes individual x,y coordinates, not points.

---

## Part 2: Classic Recursive Fractals

These fractals are defined recursively: the whole is made of smaller copies of itself.

### Problem 2.1: Sierpinski Triangle

The **Sierpinski triangle** is one of the most famous fractals. It's constructed by:
- **Base case (depth 0):** Draw a filled triangle
- **Recursive case:** Draw three smaller Sierpinski triangles at the three corners of the current triangle

![Sierpinski Triangle](https://upload.wikimedia.org/wikipedia/commons/thumb/4/45/Sierpinski_triangle.svg/220px-Sierpinski_triangle.svg.png)

Write a function `sierpinski-triangle` that takes three corner points and a depth:

```racket
(define (sierpinski-triangle p1 p2 p3 depth)
  ...)
```

**Strategy:**
1. Create a blank canvas using `(rectangle width height "solid" "white")`
2. At depth 0, draw a filled triangle using `scene+polygon`
3. Otherwise, calculate the midpoints of each edge
4. Recursively draw three smaller triangles and combine them with `overlay`

Test with:
```racket
(sierpinski-triangle (cons 250 50) (cons 50 400) (cons 450 400) 5)
```

### Problem 2.2: Koch Curve

The **Koch curve** replaces each line segment with four segments forming a "bump":

```
Before:  ___________

After:    __/\__
```

Each new segment is 1/3 the length of the original. The "bump" is an equilateral triangle pointing outward.

Write a function `koch-curve` that takes two endpoints, a depth, and a background image:

```racket
(define (koch-curve p1 p2 depth background)
  ...)
```

**Strategy:**
1. At depth 0, just draw a line from p1 to p2
2. Otherwise, find four key points:
   - `a`: 1/3 of the way from p1 to p2 (use `point-at-fraction`)
   - `b`: 2/3 of the way from p1 to p2
   - `peak`: the apex of the equilateral bump (rotate point `a` around point `a` by 60 degrees... wait, that's not right! Think carefully about what rotation gives you the peak.)
3. Recursively draw four Koch curves: p1→a, a→peak, peak→b, b→p2

*Challenge:* Getting the peak point right requires careful thinking. The peak is found by rotating point `b` around point `a` by -60 degrees (or `(- (/ pi 3))` radians).

### Problem 2.3: Koch Snowflake

A **Koch snowflake** is three Koch curves arranged as a triangle:

![Koch Snowflake](https://upload.wikimedia.org/wikipedia/commons/thumb/d/d9/KochFlake.svg/220px-KochFlake.svg.png)

Write a function `koch-snowflake` that takes a center point, size, and depth:

```racket
(define (koch-snowflake center size depth)
  ...)
```

*Hint:* Calculate the three vertices of an equilateral triangle centered at `center` with the given `size`, then draw three Koch curves.

---

## Part 3: L-Systems (30%)

**L-Systems** (Lindenmayer systems) generate fractals using string rewriting and turtle graphics. They were originally developed to model plant growth!

An L-system consists of:
- An **axiom**: a starting string
- **Rules**: how to replace each character
- **Interpretation**: what each character means for drawing

### 3.1: String Generation

#### Problem 3.1a: apply-rule

Write `apply-rule` that takes a character and a list of rules. Each rule is a pair `(cons char replacement-string)`. Return the replacement string if a rule matches, otherwise return the character as a single-character string.

```racket
(define rules1 (list (cons #\F "F+F-F")))
(apply-rule #\F rules1)  ; => "F+F-F"
(apply-rule #\+ rules1)  ; => "+"
(apply-rule #\- rules1)  ; => "-"
(apply-rule #\X rules1)  ; => "X"

(define rules2 (list (cons #\F "FF") (cons #\X "F+X")))
(apply-rule #\F rules2)  ; => "FF"
(apply-rule #\X rules2)  ; => "F+X"
(apply-rule #\+ rules2)  ; => "+"

(define rules3 '())
(apply-rule #\F rules3)  ; => "F"
```

*Hint:* Use recursion to search through the rules list.

#### Problem 3.1b: l-system-step

Write `l-system-step` that takes a string and rules, returning the string with all characters replaced according to the rules.

```racket
(l-system-step "F" (list (cons #\F "FF")))           ; => "FF"
(l-system-step "F+F" (list (cons #\F "FF")))         ; => "FF+FF"
(l-system-step "F-F-F" (list (cons #\F "FF")))       ; => "FF-FF-FF"
(l-system-step "+" (list (cons #\F "FF")))           ; => "+"
(l-system-step "" (list (cons #\F "FF")))            ; => ""
(l-system-step "FX" (list (cons #\F "FF") (cons #\X "FXF")))  ; => "FFFXF"
(l-system-step "F+F" (list (cons #\F "F-F") (cons #\+ "-")))  ; => "F-F-F-F"
```

*Hint:* Convert string to list with `string->list`, process each character, then combine the results. You can use `string-append` to join strings.

#### Problem 3.1c: l-system-generate

Write `l-system-generate` that takes an axiom, rules, and iteration count:

```racket
(l-system-generate "F" (list (cons #\F "F+F")) 0)  ; => "F"
(l-system-generate "F" (list (cons #\F "F+F")) 1)  ; => "F+F"
(l-system-generate "F" (list (cons #\F "F+F")) 2)  ; => "F+F+F+F"
(l-system-generate "F" (list (cons #\F "F+F")) 3)  ; => "F+F+F+F+F+F+F+F"
(l-system-generate "F" (list (cons #\F "FF")) 0)   ; => "F"
(l-system-generate "F" (list (cons #\F "FF")) 1)   ; => "FF"
(l-system-generate "F" (list (cons #\F "FF")) 2)   ; => "FFFF"
(l-system-generate "F" (list (cons #\F "FF")) 3)   ; => "FFFFFFFF"
(l-system-generate "X" (list (cons #\X "XY") (cons #\Y "X")) 0)  ; => "X"
(l-system-generate "X" (list (cons #\X "XY") (cons #\Y "X")) 1)  ; => "XY"
(l-system-generate "X" (list (cons #\X "XY") (cons #\Y "X")) 2)  ; => "XYX"
(l-system-generate "X" (list (cons #\X "XY") (cons #\Y "X")) 3)  ; => "XYXXY"
```

### 3.2: Turtle Graphics

A **turtle** has a position (x, y) and a heading (angle in radians). Represent it as a list: `(list x y angle)`.

#### Problem 3.2a: Turtle accessors and constructors

```racket
(define (make-turtle x y angle) (list x y angle))
(define (turtle-x t) (car t))
(define (turtle-y t) ...)  ; you write this
(define (turtle-angle t) ...)  ; you write this
```

Test cases for accessors:

```racket
(turtle-x (make-turtle 10 20 0.5))      ; => 10
(turtle-y (make-turtle 10 20 0.5))      ; => 20
(turtle-angle (make-turtle 10 20 0.5))  ; => 0.5
(turtle-x (make-turtle 0 0 0))          ; => 0
(turtle-y (make-turtle 100 200 pi))     ; => 200
(turtle-angle (make-turtle 5 5 (/ pi 4))) ; => approximately 0.785
```

#### Problem 3.2b: turtle-forward

Write `turtle-forward` that moves the turtle forward by a distance:

```racket
(turtle-forward (make-turtle 0 0 0) 10)              ; => (list 10 0 0)
(turtle-forward (make-turtle 0 0 0) 5)               ; => (list 5 0 0)
(turtle-forward (make-turtle 0 0 (/ pi 2)) 10)       ; => approximately (list 0 10 1.571)
(turtle-forward (make-turtle 0 0 pi) 10)             ; => approximately (list -10 0 3.14159)
(turtle-forward (make-turtle 5 5 0) 10)              ; => (list 15 5 0)
(turtle-forward (make-turtle 0 0 (/ pi 4)) 10)       ; => approximately (list 7.07 7.07 0.785)
(turtle-forward (make-turtle 100 100 0) 0)           ; => (list 100 100 0)
```

*Hint:* New x = x + distance * cos(angle), new y = y + distance * sin(angle)

#### Problem 3.2c: turtle-turn

Write `turtle-turn` that adjusts the turtle's heading:

```racket
(turtle-turn (make-turtle 0 0 0) (/ pi 2))           ; => (list 0 0 1.5707...)
(turtle-turn (make-turtle 0 0 0) pi)                 ; => (list 0 0 3.14159...)
(turtle-turn (make-turtle 0 0 (/ pi 2)) (/ pi 2))    ; => (list 0 0 3.14159...)
(turtle-turn (make-turtle 5 10 0) (/ pi 4))          ; => (list 5 10 0.785...)
(turtle-turn (make-turtle 0 0 0) (- (/ pi 2)))       ; => (list 0 0 -1.5707...)
(turtle-turn (make-turtle 0 0 pi) (/ pi 2))          ; => (list 0 0 4.712...)
```

### 3.3: L-System Interpreter

This is the most challenging function! Write `interpret-lsystem` that draws an L-system string.

The standard L-system commands are:
- `F`: move forward and draw a line
- `+`: turn right (add to angle)
- `-`: turn left (subtract from angle)
- `[`: save current turtle state (push to a stack)
- `]`: restore previous turtle state (pop from stack)
- Any other character: ignore (just continue)

```racket
(define (interpret-lsystem str turtle step-size turn-angle stack background)
  ...)
```

**Strategy:**
1. If the string is empty, return the background
2. Get the first character using `(string-ref str 0)`
3. Get the rest of the string using `(substring str 1)`
4. Based on the character:
   - `#\F`: Calculate new turtle position, draw line from old to new, recurse with new turtle
   - `#\+`: Recurse with turned turtle (add turn-angle)
   - `#\-`: Recurse with turned turtle (subtract turn-angle)
   - `#\[`: Recurse with current turtle pushed onto the stack
   - `#\]`: Recurse with turtle popped from the stack
   - Else: Just recurse (skip this character)

*Note:* The stack is a list. Push with `(cons turtle stack)`, pop by using `(car stack)` as the new turtle and `(cdr stack)` as the new stack.

### 3.4: L-System Examples

Once your interpreter works, create these classic L-system fractals:

#### Koch Curve (L-system version)
- Axiom: `"F"`
- Rules: `F -> "F+F-F-F+F"`
- Angle: 90 degrees

#### Sierpinski Triangle (L-system version)
- Axiom: `"F-G-G"`
- Rules: `F -> "F-G+F+G-F"`, `G -> "GG"`
- Angle: 120 degrees
- (Both F and G mean "draw forward")

#### Fractal Plant
- Axiom: `"X"`
- Rules: `X -> "F+[[X]-X]-F[-FX]+X"`, `F -> "FF"`
- Angle: 25 degrees
- (X is just for structure, only F draws)

#### Dragon Curve
- Axiom: `"FX"`
- Rules: `X -> "X+YF+"`, `Y -> "-FX-Y"`
- Angle: 90 degrees

Write wrapper functions that set up each L-system with appropriate parameters:

```racket
(define (lsystem-plant iterations)
  (let* ((axiom "X")
         (rules (list (cons #\X "F+[[X]-X]-F[-FX]+X")
                      (cons #\F "FF")))
         (str (l-system-generate axiom rules iterations))
         ...)
    (interpret-lsystem str ...)))
```

---

## Part 4: The Chaos Game & IFS (20%)

The **chaos game** is a surprisingly simple algorithm that produces complex fractals:

1. Start with a point
2. Randomly choose one of several "attractor" points
3. Move halfway toward the chosen attractor
4. Plot the point
5. Repeat thousands of times

### Problem 4.1: plot-point

Write a function that plots a single point on a background:

```racket
(define (plot-point pt color background)
  ...)
```

*Hint:* Use `place-image` with a very small circle (radius 1).

### Problem 4.2: chaos-game-step

Write a function that performs one step of the chaos game:

```racket
(define (chaos-game-step current-point attractors)
  ...)
```

*Hint:* Use `(list-ref attractors (random (length attractors)))` to pick a random attractor, then use your `midpoint` function.

### Problem 4.3: chaos-game

Write the main chaos game function:

```racket
(define (chaos-game attractors iterations start-point)
  ...)
```

Use an internal helper function with an accumulator for the image. Each iteration should update the point, plot it, and recurse.

### Problem 4.4: Sierpinski via Chaos Game

Using three points as attractors (forming a triangle), the chaos game produces... the Sierpinski triangle!

```racket
(define (chaos-sierpinski iterations)
  (let ((attractors (list (cons 250 50)
                          (cons 50 450)
                          (cons 450 450))))
    (chaos-game attractors iterations (cons 250 250))))
```

Try it with 10,000+ iterations.

### Problem 4.5: Barnsley Fern (Challenge)

The **Barnsley fern** is a famous IFS fractal that looks remarkably like a real fern. Instead of moving "halfway to an attractor," it uses **affine transformations**:

```
x' = ax + by + e
y' = cx + dy + f
```

Each transformation has a probability of being chosen. The fern uses four transformations:

| Name | a | b | c | d | e | f | probability |
|------|---|---|---|---|---|---|-------------|
| Stem | 0 | 0 | 0 | 0.16 | 0 | 0 | 1% |
| Left leaflet | 0.2 | -0.26 | 0.23 | 0.22 | 0 | 1.6 | 7% |
| Right leaflet | -0.15 | 0.28 | 0.26 | 0.24 | 0 | 0.44 | 7% |
| Main | 0.85 | 0.04 | -0.04 | 0.85 | 0 | 1.6 | 85% |

Implement the Barnsley fern:

1. Write `apply-transform` that applies an affine transformation to a point
2. Write `choose-transform` that picks a transform based on probabilities
3. Write `ifs-fractal` similar to `chaos-game` but using transforms
4. The fern's coordinates range from about -2.2 to 2.7 in x and 0 to 10 in y. Scale and translate to fit your canvas.

---

## Part 5: Your Fractal Creation (10%)

Create your own fractal exploration! Some ideas:

- **Design a new L-system:** Look up "L-system examples" for inspiration. Dragon curves, Hilbert curves, and various plant shapes are all possible.
- **Colored chaos game:** Color each point based on which attractor was chosen.
- **IFS experiments:** Design your own affine transformations to create new shapes.
- **Fractal combinations:** Layer multiple fractals or create a "fractal forest."
- **Animation:** Create a series of images at different depths/iterations.
- **3D projections:** Project 3D fractal coordinates onto 2D.

Your creation should demonstrate:
- Understanding of at least **two** fractal generation techniques from this project
- Effective use of helper functions and internal defines
- Creative use of `let`/`let*` for intermediate values
- Meaningful use of recursion
- At least one lambda expression

---

## Tips for Success

1. **Test incrementally.** Write and test each helper function before moving on. Don't try to write a whole fractal at once!

2. **Use DrRacket's stepper.** If recursion isn't working, step through your code to see what's happening.

3. **Start simple.** For each fractal, first get depth 0 and 1 working before trying higher depths.

4. **Mind your coordinates.** Remember that in `2htdp/image`, y increases downward. This affects rotations and angles!

5. **Let bindings are your friend.** Complex fractal calculations benefit greatly from naming intermediate results.

6. **Draw it on paper first.** Before coding a fractal, sketch what should happen at each recursive level.

---

## Function Summary

By the end of this project, you should have implemented:

**Part 1:**
- `midpoint`
- `point-at-fraction`
- `rotate-point`
- `draw-line`

**Part 2:**
- `sierpinski-triangle`
- `koch-curve`
- `koch-snowflake`

**Part 3:**
- `apply-rule`
- `l-system-step`
- `l-system-generate`
- Turtle functions (`make-turtle`, `turtle-x`, `turtle-y`, `turtle-angle`, `turtle-forward`, `turtle-turn`)
- `interpret-lsystem`
- At least two L-system wrapper functions

**Part 4:**
- `plot-point`
- `chaos-game-step`
- `chaos-game`
- `chaos-sierpinski`
- (Optional) Barnsley fern functions

**Part 5:**
- Your creative fractal

---

## Resources

- [2htdp/image documentation](https://docs.racket-lang.org/teachpack/2htdpimage.html)
- [L-system Wikipedia](https://en.wikipedia.org/wiki/L-system) - great examples and images
- [Chaos game Wikipedia](https://en.wikipedia.org/wiki/Chaos_game)
- [Barnsley fern Wikipedia](https://en.wikipedia.org/wiki/Barnsley_fern)
- [Fractal curves](https://en.wikipedia.org/wiki/Fractal_curve) - Koch, Sierpinski, and more
