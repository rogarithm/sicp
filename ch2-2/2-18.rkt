#lang sicp

; implementation without accumulator?
(define (reverse l)
  (define (rvs-iter lst acc)
    (if (null? lst)
        acc
        (rvs-iter (cdr lst) (cons (car lst) acc))))
  (rvs-iter l nil))