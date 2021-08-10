#lang sicp

; utility
(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

; procedure
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

; if you want the same result from applying fold-right and applying fold-left, their op must not be affected by its application order.

(define (rev-r seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(define (rev-l seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))