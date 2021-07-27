#lang sicp

; utility
(define (avr x y)
  (/ (+ x y) 2))

; for line data
(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

; for point data
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment line) ;this returns a point from a line
  (make-point
   (avr (x-point (start-segment line)) (x-point (end-segment line)))
   (avr (y-point (start-segment line)) (y-point (end-segment line)))))

; for printing
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; for rectangle
; u: upper / d: down / l: left / r: right
(define (make-rectangle ul ur dl dr)
  (cons (cons (make-segment ul ur)
              (make-segment ur dr))
        (cons (make-segment dl dr)
              (make-segment dl ul))))

; ul ur dl dr
(define (make-rec x1 x2 x3 x4 y1 y2 y3 y4)
  (define ul (make-point x1 y1))
  (define ur (make-point x2 y2))
  (define dl (make-point x3 y3))
  (define dr (make-point x4 y4))
  (cons (cons (make-segment ul ur)
              (make-segment ur dr))
        (cons (make-segment dl dr)
              (make-segment dl ul))))

(define (top-segment rectangle)
  (car (car rectangle)))
(define (bottom-segment rectangle)
  (car (cdr rectangle)))
(define (left-segment rectangle)
  (cdr (cdr rectangle)))
(define (right-segment rectangle)
  (cdr (car rectangle)))

(define (line-length line)
  (sqrt (+ [(lambda (x) (* x x))
            (- (x-point (start-segment line))
               (x-point (end-segment line)))]
           [(lambda (x) (* x x))
            (- (y-point (start-segment line))
               (y-point (end-segment line)))])))

(define (circumference rectangle)
  (+ (line-length (top-segment rectangle))
     (line-length (bottom-segment rectangle))
     (line-length (left-segment rectangle))
     (line-length (right-segment rectangle))))

(define (area rectangle)
  (* (line-length (top-segment rectangle))
     (line-length (left-segment rectangle))))

; from this simple test, verified that different implementations of data
; do not affect other procedures' result
#|
> (define ul (make-point 0 0))
> (define ur (make-point 4 0))
> (define dl (make-point 0 4))
> (define dr (make-point 4 4))
> (define r1 (make-rectangle ul ur dl dr))
> (circumference r1)
16
> (area r1)
16
> (define r2 (make-rec 0 4 0 4 0 0 4 4))
> (circumference r2)
16
> (area r2)
16
|#