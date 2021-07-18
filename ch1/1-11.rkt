#lang sicp

; tree recursive process
(define (f n)
  (cond
    ((< n 3) n)
    (else (+ (f (- n 1))
             (* 2 (f (- n 2)))
             (* 3 (f (- n 3)))))))

; iterative process
(define (fi n)
  (define (make-sum bi mi sm)
     (+ bi (* 2 mi) (* 3 sm)))
   (define (f-iter n big mid small)
     (cond
       [(= (- n 2) 0) big]
       [else (f-iter (- n 1) (make-sum big mid small) big mid)]))
  (cond
    [(< n 3) n]
    [else (f-iter n 2 1 0)]))
       
  