#lang br/quicklang
(require 2htdp/image)
(require racket/math) ;; pi


;;; HELPERS
;; Canvas constants
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 500)
(define BLANK-CANVAS (rectangle CANVAS-WIDTH CANVAS-HEIGHT "solid" "white"))

;; Turtle movement math
(define (next-x x angle dist) (+ x (* dist (cos angle))))
(define (next-y y angle dist) (+ y (* dist (sin angle))))

;; Draw a line between two points on an image
(define (draw-line x1 y1 x2 y2 color image)
  (add-line image x1 y1 x2 y2 color)) ;; What's the point of this func? it just reorders the args?

;; Read all Racket-readable tokens from a single line of text
(define (tokenize line)
  (let loop ([port (open-input-string line)] [acc '()])
    (define tok (read port))
    (if (eof-object? tok)
        (reverse acc)
        (loop port (cons tok acc)))))


(struct state (x y angle pen? color image pending) #:transparent) ;; Transparent lets me do state-image to access directly

;; Gets are automated with state-, setting is just going to use struct-copy to be cleaner in each command

; 1.3: initial-state
(define initial-state (state (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) (/ pi -2) #f "black" BLANK-CANVAS empty))
; turtle at canvas center, pointing up (angle = -(pi/2)), pen up, color "black", blank canvas, no pending arg



;;; Part 2: Reading the Program

(define (good-line? line)
  (define trimmed (string-trim line))
  (cond
    [(string=? trimmed "") #f]
    [(string-prefix? trimmed ";") #f]
    [else #t]
  )
)

;; THE READER
;; read-syntax is called by Racket when a file beginning with #lang "project4.rkt" is opened.
;; Complete the two missing definitions below.
(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define filtered (filter good-line? src-lines))   ; filter out blank lines and lines starting with ";"
  (define src-datums (apply append (map tokenize filtered))) ; tokenize each filtered line, then flatten into one list
  (define module-datum
    `(module turtle-mod "project4.rkt"
       (handle-turtle-cmds ,@src-datums)))
  (datum->syntax #f module-datum))
(provide read-syntax)


;; THE EXPANDER
;; module-begin: calls handle-turtle-cmds, extracts the final image, displays it
(define-macro (turtle-module-begin EXPR)
  #'(#%module-begin
     (display (state-image EXPR))))
(provide (rename-out [turtle-module-begin #%module-begin]))


;;; Part 3: Command Dispatch

(define (forward s steps)
  
)

(define (back s steps)
  
)

(define (right s degs)
  
)

(define (left s degs)
  
)

; 3.1: handle-cmd
; Dispatch on: number, FORWARD, BACK, RIGHT, LEFT, PENDOWN, PENUP
; Return the updated state for each case.
(define (handle-cmd s token)
  (cond
    [(number? token) (case (state-pending s)
      [(FORWARD)  (forward s token)]
      [(BACK)     (back s token)]
      [(RIGHT)    (right s token)]
      [(LEFT)     (left s token)]
    )]
    [else (case token
      [(FORWARD)  (struct-copy state s [pending token])]
      [(BACK)     (struct-copy state s [pending token])]
      [(RIGHT)    (struct-copy state s [pending token])]
      [(LEFT)     (struct-copy state s [pending token])]
      [(PENDOWN)  (struct-copy state s [pen? #t])]
      [(PENUP)    (struct-copy state s [pen? #f])]
    )]
  )
)
; tests


; 3.2: handle-turtle-cmds
; Use for/fold to process all tokens left to right, starting from initial-state.
; (This is the direct parallel to handle-args in the funstacker example.)

(define (handle-turtle-cmds . tokens)
  (for/fold
    ([s initial-state])
    ([tok (in-list tokens)])
    (handle-cmd s tok)
  )
)
(provide handle-turtle-cmds)

; tests (write a .turtle file and run it!)


;;; Part 4: Extensions

; 4.1: COLOR
; Add a COLOR command. Decide how to handle color-name symbols in handle-cmd.

; 4.2: BACK and SETPOS

; 4.3: REPEAT (stretch goal)
; expand-repeats: list of tokens -> list of tokens with REPEAT...END blocks expanded

; (define (expand-repeats tokens) ...)


;;; Part 5: Your Logo Program
;;; Write your program in a separate .turtle file.

