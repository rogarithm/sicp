#lang sicp

#| null cond must set before not pair cond, because
when nil is input, since it's not pair, its value will
be set to 1, which is not true. So the order functions
as a filter.
|#
(define (count-leaves x)
  (cond ((null? x) 0) 
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))