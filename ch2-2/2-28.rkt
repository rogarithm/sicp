#lang sicp
; can conditions simplified?

; utility
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

; fringe : list -> list
; flatten a list. If there're inner lists in a list,
; take the elements of the inner lists into the upper list
; (list (list 1 2) (list 3 4)) -> (list 1 2 3 4)
(define (fringe l)
  (cond ((null? l) l)
        ((null? (car l)) (append (car l) (fringe (cdr l))))
        ((not (pair? (car l))) (cons (car l) (fringe (cdr l))))
        ((pair? (car l)) (append (car l) (fringe (cdr l))))))

; test data
(define a '(1 2 3))
(define b '((1) (2 3)))
(define c '((1 2) (3 4) (5 6)))
(define d '((1 2) (3 4) () (5 6)))

; test function
(define (diff x . xs)
  (define disp (lambda (x)
                 (and (display (fringe x)) (newline))))
  (define (rcsv-apply proc l)
    (if (null? (cdr l))
        (proc (car l))
        (and (proc (car l)) (rcsv-apply proc (cdr l)))))
  (rcsv-apply disp (cons x xs)))