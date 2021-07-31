#lang sicp

; utility
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

; reverse : list -> list
; reverse the order of elements of a list.
; if an element is not a number (ex. list), the elements
; inside that element don't reversed.
; (reverse '(1 2 3 4)) is '(4 3 2 1)
; (reverse '((1 2) (3 4))) is '((3 4) (1 2))
(define (reverse l)
  (define (rvs-iter lst acc)
    (if (null? lst)
        acc
        (rvs-iter (cdr lst) (cons (car lst) acc))))
  (rvs-iter l nil))

; deep-reverse : list -> list
; reverse the order of elements of a list.
; if an element is not a number (ex. list), the elements
; inside that element also get reversed.
; (reverse '(1 2 3 4)) is '(4 3 2 1)
; (reverse '((1 2) (3 4))) is '((4 3) (2 1))
(define (deep-reverse lst)
  (define (drvs-iter l acc)
    (cond ((null? l) acc)
          ((pair? (car l)) (drvs-iter (cdr l) (cons (reverse (car l)) acc)))
          (else (drvs-iter (cdr l) (cons (car l) acc)))))
  (drvs-iter lst nil))
  
; test data
(define a '(1 2 3))
(define b '(1 2 3 4 5))
(define c '((1 2) (3 4)))
(define d '((1 2 3) (4 5 6) (7 8 9)))
(define e '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

; test function
(define (r-diff x . xs)
  (define disp (lambda (x)
                 (and (display (reverse x)) (display " vs. ")
                      (display (deep-reverse x)) (newline))))
  (define (rcsv-apply proc l)
    (if (null? (cdr l))
        (proc (car l))
        (and (proc (car l)) (rcsv-apply proc (cdr l)))))
  (rcsv-apply disp (cons x xs)))