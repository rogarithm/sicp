#lang sicp

; define make-center-percent, percent.
; when tested, the percent selector gives unbounded value.
; how to improve it to bound a value?
; utility
(define (recip x) (/ 1.0 x))
(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (calc-intv x tolerance)
  (define low (- x (* tolerance 0.01 x)))
  (define up (+ x (* tolerance 0.01 x)))
  (cons low up))

(define (center intv)
  (/ (+ (low-b intv) (up-b intv))
     2))

(define (width i)
  (/ (- (up-b i) (low-b i))
     2))
;(define (width intv)
;  (/ (- (up-b intv) (low-b intv)) 2.0))

; intv constructor
(define (make-intv a b) (cons a b))

(define (make-center-width c w)
  (make-intv (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-intv (- c (* 0.01 c p)) (+ c (* 0.01 c p))))

; intv selector
(define (low-b intv)
  (car intv))

(define (up-b intv)
  (cdr intv))

(define (percent intv)
  (* (/ (- (up-b intv) (center intv)) (center intv))
     100))

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
  (define (up-b-1 y) (dec (up-b y))) ;if its upper bound is 0, dec by 1
  (define (low-b+1 y) (inc (low-b y))) ;if its lower bound is 0, inc by 1
  (mul-intv x
            (make-intv (/ 1.0 (if (zero? (up-b y))
                                  (up-b-1 y)
                                  (up-b y)))
                       (/ 1.0 (if (zero? (low-b y))
                                  (low-b+1 y)
                                  (low-b y))))))

(define (sub-intv x y) ; is this logic valid?
  (let ((p1 (- (low-b x) (low-b y)))
        (p2 (- (low-b x) (up-b y)))
        (p3 (- (up-b x) (low-b y)))
        (p4 (- (up-b x) (up-b y))))
    (make-intv (min p1 p2 p3 p4)
               (max p1 p2 p3 p4))))