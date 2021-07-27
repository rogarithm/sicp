#lang sicp

; utility
(define (recip x) (/ 1.0 x))

(define (calc-intv x tolerance)
  (define low (- x (* tolerance 0.01 x)))
  (define up (+ x (* tolerance 0.01 x)))
  (cons low up))

(define (width intv)
  (/ (- (up-b intv) (low-b intv)) 2.0))

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

; test data for sub-intv
(define x1 (make-intv 1 3))
(define y1 (make-intv 5 7))
(define x2 (make-intv 1 5))
(define y2 (make-intv 3 7))
(define x3 (make-intv 5 7))
(define y3 (make-intv 1 3))
(define x4 (make-intv 3 7))
(define y4 (make-intv 1 5))

(define (compare-width proc op x y)
  (display "each width is ")
  (display (width x))
  (display " and ")
  (display (width y))
  (display ".")
  (newline)
  (display "after compute, its width is ")
  (display (width (proc x y)))
  (display "->")
  (display (= (width (proc x y)) (op (width x) (width y))))
  (newline))

(display "-----when adding interval")
(newline)
(compare-width add-intv + x1 y1)
(compare-width add-intv + x2 y2)
(compare-width add-intv + x3 y3)
(compare-width add-intv + x4 y4)
(newline)
(display "-----when subtracting interval")
(newline)
(compare-width sub-intv + x1 y1)
(compare-width sub-intv + x2 y2)
(compare-width sub-intv + x3 y3)
(compare-width sub-intv + x4 y4)
(newline)
(display "-----when multiplying interval")
(newline)
(compare-width mul-intv + x1 y1)
(compare-width mul-intv + x2 y2)
(compare-width mul-intv + x3 y3)
(compare-width mul-intv + x4 y4)
(newline)
(display "-----when dividing interval")
(newline)
(compare-width div-intv + x1 y1)
(compare-width div-intv + x2 y2)
(compare-width div-intv + x3 y3)
(compare-width div-intv + x4 y4)