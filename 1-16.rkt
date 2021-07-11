#lang sicp

(define (even? n)
  (= (remainder n 2) 0))

(define (sqr x) (* x x))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (sqr (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; is this procedure's computation time grows in log of n?
(define (expt-iter b n)
  (define (fast-expt-iter a b n)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (* a (sqr b)) b (- n 2)))
          (else (fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter 1 b n))