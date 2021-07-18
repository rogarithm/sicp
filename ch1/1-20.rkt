#lang sicp

#|
applicative order: substitute each definition when it first appear
normal order: compute after expanding all expressions
|#
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206
     40)
(gcd 40
     (remainder 206 40))
(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))) -> 11 times

(gcd 206 40)
(gcd 40 (remainder 206 40)) -> (gcd 40 6)
(gcd 6 (remainder 40 6)) -> (gcd 6 4)
(gcd 4 (remainder 6 4)) -> (gcd 4 2)
(gcd 2 (remainder 4 2)) -> (gcd 2 0) -> 4 times in total