#lang sicp

(define (sqr x) (* x x))
(define (inc x) (+ x 1))
(define (divides? a b) (= (remainder b a) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (sqr (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
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