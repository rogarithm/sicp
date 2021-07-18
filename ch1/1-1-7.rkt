#lang sicp

(define (sqr x) (* x x))
(define (avr x y) (/ (+ x y) 2))

; orig
; (define (ge? g x)
;  (< (abs (- (sqr g) x)) 0.001))

(define (i g x) (avr g (/ x g)))
; (define (i g x) (< (abs (- (i g x) g)) 0.001))

(define (ge? g x)
  (< (abs (- (i g x) g)) 0.00001))

(define (si g x)
  (if (ge? g x)
      g
      (si (i g x) x)))

(define (sqrt x)
  (si 1.0 x))

; when x gets bigger, its assumption goes inaccurate
; same thing happens when x gets smaller
; but when? To check, proper function is required:

; returns how far fianl guess val is from radicand
(define (si-check g x)
  (if (ge? g x)
      (abs (- (sqr g) x))
      (si-check (i g x) x)))

; wrapper
(define (sqrt-c x)
  (si-check 1.0 x))

; to check if final guess always gets further from its
; corresponding radicand. Even it's not, if it shows
; a tendency, we can use it to prove refined algorithm's
; more accurate than before.
(define (r-sqrt-c x)
    (if (< (sqrt-c x) (sqrt-c (+ x 1)))
        (r-sqrt-c (+ x 1))
        x))

; hard to visualize when guess gets smaller
(define (rsc2 x)
    (if (< (sqrt-c x) (sqrt-c (/ x 2.0)))
        (rsc2 (/ x 2.0))
        x))

#|
it's not linear, but it seems to have a tendency of getting
final guess value further from its input radicand.
> (r-sqrt-c 4)
9
> (r-sqrt-c 10)
30
> (r-sqrt-c 31)
99
> (r-sqrt-c 100)
330
> (r-sqrt-c 331)
1118
> (r-sqrt-c 1119)
3831
|#