#lang sicp

; how to print nothing when l is null? Is it ok to do nothing?
(define (for-each proc l)
  (if (not (null? l))
      (and (proc (car l)) (for-each proc (cdr l)))))

(for-each (lambda (x) (newline) (display x))
          (list 18 29 10))

; what is result? Where should it be inserted?