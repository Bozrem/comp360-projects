#lang br/quicklang
(require 2htdp/image)
(require 2htdp/universe)
(require racket/math) ;; pi


;;; HELPERS
;; Canvas constants
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 500)
(define BLANK-CANVAS (rectangle CANVAS-WIDTH CANVAS-HEIGHT "solid" "white"))

;; Turtle movement math
(define (next-x x angle dist) (+ x (* dist (cos angle))))
(define (next-y y angle dist) (+ y (* dist (sin angle))))

;; Read all Racket-readable tokens from a single line of text
(define (tokenize line)
  (let loop ([port (open-input-string line)] [acc '()])
    (define tok (read port))
    (if (eof-object? tok)
        (reverse acc)
        (loop port (cons tok acc)))))


(struct state (x y angle pen? color image pending) #:transparent) ;; Transparent lets me do state-image to access directly

;; Gets are automated with state-, setting is just going to use struct-copy to be cleaner in each command

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
    `(module turtle-mod "4.rkt"
       (handle-turtle-cmds ',src-datums)))
  (datum->syntax #'read-syntax module-datum))
(provide read-syntax)

(define (turtle-show-image img)
  (big-bang img
    [to-draw (λ (w) w)]))
(provide turtle-show-image)

;; THE EXPANDER
;; module-begin: runs the program, then opens the result in big-bang
(define-macro (turtle-module-begin EXPR)
  #'(#%module-begin
     (turtle-show-image (state-image EXPR))))
(provide (rename-out [turtle-module-begin #%module-begin]))


;;; Part 3: Command Dispatch

(define (forward s steps)
  (define old-x (state-x s))
  (define old-y (state-y s))

  (define new-x (next-x old-x (state-angle s) steps))
  (define new-y (next-y old-y (state-angle s) steps))
  (define new-image (if (state-pen? s)
                        (add-line (state-image s) old-x old-y new-x new-y (state-color s))
                        (state-image s)))

  (struct-copy state s [x new-x] [y new-y] [image new-image])
)

(define (back s steps)
  (forward s (* -1 steps))
)

(define (right s degs)
  (define rads (* degs (/ pi 180)))
  (struct-copy state s [angle (+ (state-angle s) rads)])
)

(define (left s degs)
  (define rads (* degs (/ pi 180)))
  (struct-copy state s [angle (- (state-angle s) rads)])
)

(define (setpos s new-x new-y)
  (struct-copy state s [x new-x] [y new-y])
)


; 3.1: handle-cmd
(define (handle-cmd s token)
  (cond
    ;; Argument handling
    [(number? token)
      (define p (state-pending s))
      (cond
        [(empty? p) s]
        [else (case (first p)
          [(FORWARD)  (struct-copy state (forward s token) [pending empty])]
          [(BACK)     (struct-copy state (back s token) [pending empty])]
          [(RIGHT)    (struct-copy state (right s token) [pending empty])]
          [(LEFT)     (struct-copy state (left s token) [pending empty])]
          [(SETPOS)   (cond
            [(= (length p) 1) (struct-copy state s [pending (list 'SETPOS token)])]
            [(= (length p) 2) (struct-copy state (setpos s (second p) token) [pending empty])]
            [else s]
          )]
          [else s]
        )]
      )
    ]

    ;; Color argument handling
    [(and (symbol? token) (equal? (state-pending s) (list 'COLOR)))
      (struct-copy state s [color (symbol->string token)] [pending empty])
    ]

    ;; Command handling
    [else (case token
       [(FORWARD BACK RIGHT LEFT COLOR SETPOS) (struct-copy state s [pending (list token)])]
       [(PENDOWN) (struct-copy state s [pen? #t])]
       [(PENUP)   (struct-copy state s [pen? #f])]
       [else s]
    )]
  )
)
;; The logic here:
;;  If given PENUP or PENDOWN, change the state immediately
;;  If given other commands, set them to pending
;;  If given a number:
;;    If any single arg commands are pending run them
;;    If SETPOS is pending:
;;      If another number is also already pending, use both in a setpos command
;;      Otherwise, add this number to pending
;;  If given a token and COLOR is pending, set the token as color


; 3.2: handle-turtle-cmds
; Use for/fold to process all tokens left to right, starting from initial-state.
; (This is the direct parallel to handle-args in the funstacker example.)

(define (handle-turtle-cmds tokens)
  (for/fold
    ([s initial-state])
    ([tok (in-list tokens)])
    (handle-cmd s tok)
  )
)
(provide handle-turtle-cmds)
