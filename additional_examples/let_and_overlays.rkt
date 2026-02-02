#lang racket
(require 2htdp/image)

; make a circle of symbols
(define symbol-circle
  (let* ((one-symbol (rectangle 30 30 "solid" "blue"))
         (two-symbols (overlay/offset (triangle 30 "solid" "black") 50 0 one-symbol))
         (three-symbols (overlay/offset (circle 15 "solid" "green") 50 0 two-symbols))
         (four-symbols (overlay/offset (star 30 "solid" "purple") 50 0 three-symbols))
         )
    (overlay four-symbols (rectangle 100 100 "solid" "white"))))

symbol-circle
