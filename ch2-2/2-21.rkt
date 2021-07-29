#lang sicp

; utility
(define (sqr x) (* x x))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

; procedures
(define (sqr-l i)
  (if (null? i)
      nil
      (cons (sqr (car i)) (sqr-l (cdr i)))))

(define (map-sqr-l i)
  (map sqr i))