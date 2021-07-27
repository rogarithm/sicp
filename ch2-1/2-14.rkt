#lang sicp

; from the test, it seems that adding each interval's tolerance gives the tolerance of processed one.

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

(define (par1 r1 r2)
  (div-intv
   (mul-intv r1 r2)
   (add-intv r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-intv 1 1)))
    (div-intv
     one
     (add-intv
      (div-intv one r1)
      (div-intv one r2)))))

; test data for sub-intv

#|(define x1 (make-intv 1 3))
(define y1 (make-intv 5 7))
(define x2 (make-intv 1 5))
(define y2 (make-intv 3 7))
(define x3 (make-intv 5 7))
(define y3 (make-intv 1 3))
(define x4 (make-intv 3 7))
(define y4 (make-intv 1 5))|#

(define x1 (make-center-percent 100 1))
(define y1 (make-center-percent 500 2))
(define x2 (make-center-percent 100 5))
(define y2 (make-center-percent 300 7))
(define x3 (make-center-percent 500 7))
(define y3 (make-center-percent 100 3))
(define x4 (make-center-percent 300 7))
(define y4 (make-center-percent 100 5))

; I've used make-intv for this test function
(define (varify-diff)
  (display (par1 x1 y1))
  (display " vs. ")
  (display (par2 x1 y1))
  (newline)
  #|(display (par1 x2 y2))
  (display " vs. ")
  (display (par2 x2 y2))
  (newline)
  (display (par1 x3 y3))
  (display " vs. ")
  (display (par2 x3 y3))
  (newline)
  (display (par1 x4 y4))
  (display " vs. ")
  (display (par2 x4 y4))
  (newline)|#)

#| from result of the test, it's clear that two equation
gives different value: |#

; I've used make-center-percent for this test function
(define (diff-div)
  (display (div-intv x1 x1))
  (newline)
  (display (div-intv x1 y1))
  (newline)
  #|(display (div-intv x2 x2))
  (newline)
  (newline)
  (display (div-intv x2 y2))
  (newline)
  (newline)
  (display (div-intv x3 x3))
  (newline)
  (newline)
  (display (div-intv x3 y3))
  (newline)
  (newline)
  (display (div-intv x4 x4))
  (newline)
  (newline)
  (display (div-intv x4 y4))|#)