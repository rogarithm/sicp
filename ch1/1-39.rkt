#lang sicp

(define (cont-frac n d k)
  (define (unit i) (/ (n i) (d i)))
  (define (sum-frac i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (sum-frac (inc i))))))
  (sum-frac 1))

(define (k-frac k) (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))

; after k=11, we can get correct value that fits to fourth number after decimal point

(define (e k) (+ 2
                 (cont-frac (lambda (i) 1.0)
                            (lambda (i) (cond ((= i 1) 1)
                                              ((= i 2) 2)
                                              ((= (remainder i 3) 2) (+ 2 (* 2 (/ (- i 2) 3))))
                                              (else 1)))
                            k)))

(define (tan-cf x k)
  (cont-frac (lambda (i) (- (* 2 i) 1))
             (lambda (x) (cond ((= x 1) x)
                               (else (* -1 x x))))
             k))