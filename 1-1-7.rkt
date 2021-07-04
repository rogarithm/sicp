#lang sicp

(define (sqr x) (* x x))

(define (avr x y) (/ (+ x y) 2))

(define (ge? g x)
  (< (abs (- (sqr g) x)) 0.001))

(define (i g x) (avr g (/ x g)))

(define (si g x)
  (if (ge? g x)
      g
      (si (i g x) x)))

(define (sqrt x)
  (si 1.0 x))

; when x gets bigger, its assumption goes inaccurate
;> (sqrt 1000000)
;1000.0000000000118
;> (sqrt 100)
;10.000000000139897
;> (sqrt 10000)
;100.00000025490743
; same thing happens when x gets smaller
;> (sqrt 0.0001)
;0.03230844833048122
;> (sqrt 0.000001)
;0.031260655525445276
;> (sqrt 0.00000001)
;0.03125010656242753

; how to check when ge? does not work?
> (define (iac g x) (and (not (ge? g x)) (i g x)))
> (iac 1.0 100)
50.5
> (iac 50.5 100)
26.24009900990099
> (iac 26.24009900990099 100)
15.025530119986813
> (iac 15.025530119986813 100)
10.840434673026925
> (iac 10.840434673026925 100)
10.032578510960604
> (iac 1.0 1)
#f
> (iac 10.032578510960604 100)
10.000052895642693
> (iac 10.000052895642693 100)
10.000000000139897
> (iac 10.000000000139897 100)
#f