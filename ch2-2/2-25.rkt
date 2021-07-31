#lang sicp

(define x '(1 3 (5 7) 9))
(define y '((7)))
(define z '(1 (2 (3 (4 (5 (6 7)))))))
(define k '(1 (3 5)))
; k is the same with (cons 1 (cons (cons 3 (cons 5 nil)) nil))

#|
(car (cdr (car (cdr (cdr x)))))
(car (cdaddr x))

(car (car y))
(caar y)

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z))))))))))))
|#