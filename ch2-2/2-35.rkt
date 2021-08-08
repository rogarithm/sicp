#lang sicp

; utility
(define (inc x) (+ x 1))

; fringe : list -> list
; flatten a list. If there're inner lists in a list,
; take the elements of the inner lists into the upper list
; (list (list 1 2) (list 3 4)) -> (list 1 2 3 4)
(define (fringe l)
  (cond ((null? l) l)
        ((null? (car l)) (append (car l) (fringe (cdr l))))
        ((not (pair? (car l))) (cons (car l) (fringe (cdr l))))
        ((pair? (car l)) (append (car l) (fringe (cdr l))))))

(define (length sequence)
  (accumulate (lambda (x y) (if (not (null? x)) (inc y))) 0 sequence))

; procedures
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (inc low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

; answer is too complex. simplify.
(define (count-leaves t)  
  (accumulate + 0 (map (lambda (x) (if (pair? x)
                                       (length (fringe x))
                                       1))
                       t)))