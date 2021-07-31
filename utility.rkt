#lang sicp

; utility for test
(define (r-diff x . xs)
  (define disp (lambda (x)
                 (and (display (reverse x)) (display " vs. ")
                      (display (deep-reverse x)) (newline))))
  (define (rcsv-apply proc l)
    (if (null? (cdr l))
        (proc (car l))
        (and (proc (car l)) (rcsv-apply proc (cdr l)))))
  (rcsv-apply disp (cons x xs)))