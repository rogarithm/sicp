#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))
            (first (list (car s))))
        (append rest (map (lambda (l) (append first l))
                          rest)))))