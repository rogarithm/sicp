#lang sicp

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

; accumulate-n : op init sequence -> sequence
; input sequence has several sequences of the same length
; each other. Binds every nth element of inner sequences
; with given procedure, then gather each results as one
; element, finally gives one sequence as a result.
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; take row's elements to col, and swap.
(define (swap row col)
  (cond ((null? row) col)
        ((not (= (car row) (car col))) (cons (car row) (swap (cdr row) (cdr col))))
        (else (cons (car col) (swap (cdr row) (cdr col))))))

(define (transpose mat)
  (accumulate-n (lambda (row col) (swap row col)) nil mat))

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

; procedures

; map-2 : procedure list list -> list
; map procedure to every pair of elements in l1 and l2,
; and gives one list.
; l1 and l2 have the same number of elements
(define (map-2 proc l1 l2)
  (if (null? l1)
      nil
      (cons (proc (car l1) (car l2))
            (map-2 proc (cdr l1) (cdr l2)))))

(define (o-map proc l1 l2)
  (if (null? l1)
      nil
      (cons (map-2 proc (car l1) (car l2))
            (o-map proc (cdr l1) (cdr l2)))))

(define (dot-product v w)
  (accumulate + 0 (fringe (o-map * v w))))

; test
(define mat1 '((1 2) (3 4)))
(define mat2 '((5 6) (7 8)))
(define vec1 '(1 2))

; (accumulate-n + 0 s) should be (22 26 30)
; (o-map * '((1 2) (3 4)) '((5 6) (7 8)))
(dot-product '((1 2) (3 4)) '((5 6) (7 8)))


(define (matrix-*-vector m v)
  (map (lambda (l) (accumulate + 0 (map-2 * l v))) m))



#|
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map ? m)))
|#

; utility for test
(define (diff x . xs)
  (define disp (lambda (x)
                 (and (display ( x)) (display " vs. ")
                      (display ( x)) (newline))))
  (define (rcsv-apply proc l)
    (if (null? (cdr l))
        (proc (car l))
        (and (proc (car l)) (rcsv-apply proc (cdr l)))))
  (rcsv-apply disp (cons x xs)))