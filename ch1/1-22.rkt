#lang sicp

(define (sqr x) (* x x))
(define (inc x) (+ x 1))
(define (divides? a b) (= (remainder b a) 0))

(define (smallest-div n)
  (define (find-div n test-div)
    (cond ((> (sqr test-div) n) n)
          ((divides? test-div n) test-div)
          (else (find-div n (inc test-div)))))
  (find-div n 2))

(define (prime? n)
  (= n (smallest-div n)))

(define (timed-prime-test n)
  ;(newline)
  ;(display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (and (newline)
           (display n)
           (report-prime (- (runtime) start-time)))))
      ;(display " is not p. ")))

(define (report-prime elapsed-time)
  (display " is p; elapsed time is: ")
  (display elapsed-time))

(define (search-for-primes x y)
  (cond
    ((and (< x y) (even? x))
     (and (timed-prime-test (inc x)) (search-for-primes (+ x 2) y)))
    ((and (< x y) (odd? x))
     (and (timed-prime-test x) (search-for-primes (+ x 2) y)))
    (else
     (newline)
     (display " the end "))))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

#|
   1009,    1013,    1019
  10007,   10009,   10037
 100003,  100019,  100043
1000003, 1000033, 1000037

340, 121, 121
318,  55,  53
285,  67,  66
292, 128, 126
|#