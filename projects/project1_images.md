# Project 1: Building Images in Racket

**COMP 360: Programming Languages**

---
## Expected Effort

This project took me about 2 hours of concentrated effort. If you aren't able to complete the project, I expect you to spend at least 5 hours and as many as 10 hours. If you can't complete the project within 10 hours, you should let yourself stop working. 

Your effort now will pay off in the future.

---

## AI Assistance Disclosure

**You must disclose use of AI on this assignment.** If you opt to use AI for this project, I will ask to see your conversation history.

---

## Grading and Submission

This project is **due the week of Feb 2**. You can bring me your code anytime that week during my office hours. Grading should take no more than 10 minutes.

Your grade will be based on how much of the project you completed, how much time you spent, and how well you understand your own code.

My solutions will become avaiable on Feb 6.

---

## Overview

In this project, you'll learn Racket fundamentals while building increasingly complex images. You'll start with basic shapes and work your way up to creating an original scene that demonstrates your mastery of helper functions, data structures, recursion, and function composition.

**Getting Started:** Open `project1.rkt`. You'll see:

```racket
#lang racket
(require 2htdp/image)
```

This imports the `2htdp/image` library, which provides functions for creating and combining images.

---

## Part 1: Racket & Image Basics

### 1.1 Exploring Shapes

The `2htdp/image` library provides several shape primitives. Try these in DrRacket's interactions window:

```racket
(circle 50 "solid" "blue")
(circle 50 "outline" "black")
(rectangle 100 60 "solid" "green")
(triangle 80 "solid" "red")
(star 40 "solid" "gold")
```

**Problem 1.1:** Write a function `colored-circle` that takes a radius and a color, and returns a solid circle of that size and color.

```racket
; (colored-circle 30 "purple") should produce a purple circle of radius 30
```

**Problem 1.2:** Write a function `bullseye-simple` that takes a radius and returns a two-ring bullseye (red outer ring, white inner circle). The inner circle should have half the radius of the outer. Use the `colored-circle` function from Problem 1.1.

*Hint:* Use `(overlay a b)` to place one image `a` on top of another `b`. Feel free to look up the [documentation](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28part._.Overlaying_.Images%29) but DO NOT use AI assistance for this! I will ask you exactly how the overlay function works.

---

### 1.2 Working with Colors

Colors can be specified by name (`"red"`, `"blue"`) or constructed with `make-color`:

```racket
(make-color 255 0 0)      ; red (R=255, G=0, B=0)
(make-color 100 100 255)  ; light blue
(make-color 0 0 0)        ; black
```

**Problem 1.3:** Write a function `rgb-square` that takes three numbers (r, g, b) and a side length, and returns a solid square of that color and size.

**Problem 1.4:** Write a function `grayscale-square` that takes a single number (0–255) and a side length, and returns a square in that shade of gray.

*Hint:* A grayscale color has equal R, G, and B values. Higher numbers should result in a whiter square.

---

## Part 2: Combining Shapes

### 2.1 Positioning Images

The library provides several ways to combine images:

```racket
(beside img1 img2)           ; place img2 to the right of img1
(above img1 img2)            ; place img2 below img1
(overlay img1 img2)          ; place img1 on top of img2, centered
(overlay/offset img1 x y img2)   ; place img1 on img2, offset by (x, y)
```

**Problem 2.1:** Write a function `traffic-light` that takes no arguments and returns an image of a vertical traffic light (three circles—red, yellow, green—arranged vertically on a black rectangular background). I recommend at least helper functions: `top`, `middle`, `bottom`, and `background` to make your main `traffic-light` function simple. Use `overlay/offset` and proper function composition to place the lights in the correct places.

*Hint 1:* [`overlay/offset` documentation](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._overlay%2Foffset%29%29) 

*Hint 2:* Here is a function composition that works: `(overlay lights background)`, where `lights` is either a separate function or is an expression that produces the lights correctly, and `lights` could be thought of as a bottom light offset from a top light with a middle light overlayed on top of them. There are many compositions that will work!

**Problem 2.2:** Write a function `simple-house` that takes a size parameter and returns a house image (a triangle roof on top of a square base). Both the roof and base should scale proportionally with the size parameter. I recommend at least two helper functions: `base` and `roof`.

*Hint:* Programming and design are **iterative processes**. You need to try *something* and tweak it, over and over and over until you get something you like!

---

### 2.2 Function Composition

Good Racket style involves building complex images from simple, well-named helper functions.

**Problem 2.3:** Build a `tree` function that takes a height parameter and returns a simple tree (a green triangle on a brown rectangle trunk). Break this into at least two helper functions: one for the foliage, one for the trunk.

**Problem 2.4:** Build a `car` function that takes a body-color and returns a simple side-view car. Your car should include:
- A body (rectangle)
- Wheels (circles)
- At least one window

Decompose this into helper functions. Think about what parts of a car are reusable or worth naming.

*Warning:* this will **shadow** the built-in `car` function, which means you'll lose access to it. What is a good fix for this problem?

---

## Part 3: Data Structures for Graphics

### 3.1 Colors as Data

Instead of passing separate R, G, B values, we can represent colors as lists:

```racket
(define my-red (list 255 0 0))
(define my-blue (list 0 0 255))
```

This is useful for manipulating colors. 

**Problem 3.1:** Write a function `color->make-color` that takes a color list `(list R G B)` and returns a color created with `make-color`.

**Problem 3.2:** Write a function `darker` that takes a color list and returns a new color list where each component is reduced by 20% (multiplied by 0.8). Use `max` and `floor` to keep values as integers in the valid range.

**Problem 3.3:** Write a function `blend` that takes two color lists and returns their average (component-wise).

```racket
; (blend (list 255 0 0) (list 0 0 255)) => (list 127 0 127)  ; purple-ish
```

---

### 3.2 Points and Positions

We can represent positions as pairs:

```racket
(define origin (cons 0 0))
(define point1 (cons 100 50))
```

**Problem 3.4:** Write helper functions `get-x` and `get-y` that extract the x and y components from a point.

**Problem 3.5:** Write a function `place-image-at` that takes an image, a point (pair), and a background image, and places the image at that point on the background.

*Hint:* [`place-image` documentation.](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._place-image%29%29)

Example: `(place-image-at (make-car "green") '(10 . 100) (rectangle 200 200 "solid" "white"))` places a car halfway down the white background, cutting off the back half of the car.

**Problem 3.6:** Write a function `place-all` that takes a list of points, a single image, and a background, and places a copy of the image at each point. Use recursion.

```racket
; Example: place a star at three locations
(define star-img (star 20 "solid" "gold"))
(define bg (rectangle 300 200 "solid" "navy"))
(define points (list (cons 50 50) (cons 150 100) (cons 250 50)))
(place-all points star-img bg)
```

---

## Part 4: Recursion with Graphics

### 4.1 Rows and Grids

**Problem 4.1:** Write a function `row-of` that takes a number n and an image, and returns n copies of that image arranged in a horizontal row.

*Hint:* [`offset/align/offset' documentation](https://docs.racket-lang.org/teachpack/2htdpimage.html#%28def._%28%28lib._2htdp%2Fimage..rkt%29._overlay%2Falign%2Foffset%29%29)

*Warning:* Unless you want to get really fancy, you don't need to scale the spacing according to the images.

```
(row-of 5 (circle 20 "solid" "red"))
```

**Problem 4.2:** Write a function `column-of` that does the same vertically.

**Problem 4.3:** Write a function `grid-of` that takes rows, cols, and an image, and returns a grid of that image. Use your `row-of` and/or `column-of` functions.

*Hint:* One recursive structure that will work is:
* base case: draw a row of images
* otherwise: overlay a row of images on top of a smaller grid (recursion), aligned to the middle top, offset by a little bit

### 4.2 Adding Randomness

**Problem 4.4:** Write a function `forest` that takes a number-of-trees and produces an image of a forest. The trees should be placed at varied positions with varying height. Reuse your `tree` and modify your `row-of` using [`random`](https://docs.racket-lang.org/reference/generic-numbers.html#%28part._.Random_.Numbers%29) to space images randomly.


*Hint:* `(+ (* (random) range) shift)` is one way you can produce a random number which ranges between `shift` and `shift + range`, for examply if you want to guarantee a tree height of `60` pixels.

---

## Part 5: Your Scene

Using everything you've learned, compose an original scene. Create anything you want. Just make something you'll be proud to show off!

Your scene should demonstrate:
- **Helper functions:** Your code should be well-decomposed into meaningful, reusable pieces
- **Data structures:** Use pairs, lists, or color representations where appropriate
- **Recursion or repetition:** Include some element that uses recursive image construction
- **Composition:** Your scene should combine multiple distinct visual components

Some possible directions (or invent your own):
- A cityscape with buildings, windows, and a sky
- An underwater scene with fish, coral, and bubbles
- A space scene with planets, stars, and a spaceship
- A landscape with mountains, trees, and clouds
- An abstract geometric composition

I've provided my solution to Part 5 as inspiration. I did use AI assistance for a mathematical property, and I clearly disclosed which functions were inspired by AI. If you want to see my prompts/conversations, I can share those with you.

There's no right answer for this part. Focus on writing clean, well-organized code and creating an image that reflects your effort and creativity.
