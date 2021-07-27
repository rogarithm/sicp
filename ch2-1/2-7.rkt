#lang sicp

; utility
(define (recip x) (/ 1.0 x))

(define (calc-intv x tolerance)
  (define low (- x (* tolerance 0.01 x)))
  (define up (+ x (* tolerance 0.01 x)))
  (cons low up))

;intv constructor, selector
(define (make-intv a b) (cons a b))

(define (low-b intv)
  (car intv))

(define (up-b intv)
  (cdr intv))


; procedures
(define (add-intv x y)
  (make-intv (+ (low-b x) (low-b y))
             (+ (up-b x) (up-b y))))

(define (mul-intv x y)
  (let ((p1 (* (low-b x) (low-b y)))
        (p2 (* (low-b x) (up-b y)))
        (p3 (* (up-b x) (low-b y)))
        (p4 (* (up-b x) (up-b y))))
    (make-intv (min p1 p2 p3 p4)
               (max p1 p2 p3 p4))))

(define (div-intv x y)
  (mul-intv x
            (make-intv (/ 1.0 (up-b y))
                       (/ 1.0 (low-b y)))))
; ?
(define (parallel-r r1 r2)
  (cons (/ 1 (+ (recip (low-b r1)) (recip (low-b r2))))
        (/ 1 (+ (recip (up-b r1)) (recip (up-b r2))))))