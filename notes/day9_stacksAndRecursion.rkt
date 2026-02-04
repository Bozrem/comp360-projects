#lang racket
(require racket/trace)

"warmup"
;;; WARMUP
; average using foldr
(define (average a)
  '())
(average '(0 10 20))

(newline)
"The Full Stack"
;;; THE FULL STACK
(define (show-stack)
  (for ([frame (continuation-mark-set->context (current-continuation-marks))])
    (printf "  ~a~n" frame)))

(define (sum-list a)
  (printf "Stack at n=~a:~n" a)
  (show-stack)
  (if (null? a)
      0
      (+ (car a) (sum-list (cdr a)))))

(sum-list '(5 10 15 20))
(newline)

"The Stack"
;;; THE STACK (easier to look at)
(define SHOW-STACK #t)

;; Functions we care about
(define tracked-functions '(factorial factorial-tail))

;; Helper to get filtered stack frames
(define (get-relevant-stack)
  (filter
   (lambda (frame)
     (and (car frame)  ; filter out #f entries
          (member (car frame) tracked-functions)))
   (continuation-mark-set->context (current-continuation-marks))))

;; Helper to display stack info
(define (show-stack-info label n [acc #f])
  (define stack (get-relevant-stack))
  (if acc
      (printf "~a(~a, ~a) - stack depth: ~a~n" label n acc (length stack))
      (printf "~a(~a) - stack depth: ~a~n" label n (length stack)))
  (when SHOW-STACK
    (for ([frame stack])
      (printf "    ~a~n" (car frame)))
    (newline)))

;; Non-tail-recursive factorial
;(define (factorial n)
;  (show-stack-info 'factorial n)
;  (if (zero? n)
;      1
;      (* n (factorial (sub1 n)))))

;; Tail-recursive factorial
;(define (factorial-tail n [acc 1])
;  (show-stack-info 'factorial-tail n acc)
;  (if (zero? n)
;      acc
;      (factorial-tail (sub1 n) (* n acc))))

;(displayln "=== Non-tail-recursive ===\n")
;(factorial 5)
;(newline)

;(displayln "=== Tail-recursive ===\n")
;(factorial-tail 5)