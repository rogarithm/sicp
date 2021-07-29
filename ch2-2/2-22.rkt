#lang sicp

; utility
(define (sqr x) (* x x))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (reverse l)
  (define (rvs-iter lst acc)
    (if (null? lst)
        acc
        (rvs-iter (cdr lst) (cons (car lst) acc))))
  (rvs-iter l nil))

; procedures
#|(define (sqr-l i)
  (if (null? i)
      nil
      (cons (sqr (car i)) (sqr-l (cdr i)))))|#

(define (map-sqr-l i)
  (map sqr i))

#|(define (sqr-l i)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons (sqr (car things)) answer)
              )))
  (iter i nil))

why the order of elements is reversed? Because...
(it '(2) (cons 1 nil))
(it nil (cons 4 (cons 1 nil)))
(cons 4 (cons 1 nil))|#

#|Another not-working code...
(define (sqr-l i)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons answer (sqr (car things)))
              )))
  (iter i nil))

At first, the answer var is set as nil, so this function always gives (cons nil number) ... no matter what its input is.
|#

; My solution is to reverse the input list, and then calculate.
(define (sqr-l i)
  (let ((inverted-i (reverse i)))
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) (cons (sqr (car things)) answer)
              )))
  (iter inverted-i nil)))