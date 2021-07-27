#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (neg-rat? n d)
  (if (or (and (< n 0) (> d 0))
          (and (> n 0) (< d 0)))
      #t
      #f))

(define (make-neg x)
  (if (> x 0)
      (* -1 x)
      x))

(define (make-pos x)
  (if (< x 0)
      (* -1 x)
      x))
      
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (neg-rat? n d)
        (cons (make-neg (/ n g)) (make-pos (/ d g)))
        (cons (make-pos (/ n g)) (make-pos (/ d g))))))

; upper part
(define (numer x) (car x))

; lower part
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

