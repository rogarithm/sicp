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

(define (sub-intv x y) ; is this logic valid?
  (let ((p1 (- (low-b x) (low-b y)))
        (p2 (- (low-b x) (up-b y)))
        (p3 (- (up-b x) (low-b y)))
        (p4 (- (up-b x) (up-b y))))
    (make-intv (min p1 p2 p3 p4)
               (max p1 p2 p3 p4))))

#| test data for sub-intv
(define x1 (make-intv 1 3))
(define y1 (make-intv 5 7))
(define x2 (make-intv 1 5))
(define y2 (make-intv 3 7))
(define x3 (make-intv 5 7))
(define y3 (make-intv 1 3))
(define x4 (make-intv 3 7))
(define y4 (make-intv 1 5))
|#