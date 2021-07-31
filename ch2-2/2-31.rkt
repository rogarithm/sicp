#lang sicp

; utility
(define (sqr x)
  (* x x))

; procedures
#|
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (sqr tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (sqr sub-tree)))
       tree))
|#
(define (tree-map proc arg)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       arg))
; test data
(define a '(1 (2 (3 4) 5) (6 7)))