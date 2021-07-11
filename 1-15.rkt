#lang sicp

(define (inc x) (+ x 1))

(define (s a c)
  (if (not (> (abs a) 0.1))
      c
      (s (/ a 3.0) (inc c))))

each p procedure has 4 x expression, each of which uses p.
If an angle isn't in the range for use without its value changed,
each angle use another 4 p procedure. So,

(s 12.15 0) -> 4^5 times!
