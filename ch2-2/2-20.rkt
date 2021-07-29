#lang sicp

(define (same-parity x . xs)
  (cond ((null? xs) (list x))
        ((even? x) (cons x (filter-evens xs)))
        ((odd? x) (cons x (filter-odds xs)))
        ))

(define (filter-evens l)
  (cond ((null? l) nil)
        ((even? (car l)) (cons (car l) (filter-evens (cdr l))))
        ((odd? (car l)) (filter-evens (cdr l)))))

(define (filter-odds l)
  (cond ((null? l) nil)
        ((odd? (car l)) (cons (car l) (filter-odds (cdr l))))
        ((even? (car l)) (filter-odds (cdr l)))))

