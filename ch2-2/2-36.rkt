#lang sicp

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

(define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 s)