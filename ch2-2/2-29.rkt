#lang sicp

; constructors
; binary mobile has two arms each on its left and right.
(define (make-mobile l r)
  (list l r))

; length must be a number. structure, which represents weight of pendulum,
; can be a number or other mobile.
(define (make-branch length structure)
  (list length structure))

; predicates
(define (mobile? obj)
  (and (list? (l-brc obj)) (list? (r-brc obj))))

(define (branch? obj)
  (list? obj))


; selectors
(define (l-brc mob)
  (car mob))

(define (r-brc mob)
  (car (cdr mob)))

(define (brc-length brc)
  (car brc))

(define (brc-structure brc)
  (car (cdr brc)))

; procedures

(define (total-weight mob)
  (let ((l-str (brc-structure (l-brc mob)))
        (r-str (brc-structure (r-brc mob))))
    (define (get-number struct)
      ; if a structure is a number, use it.
      ; if it's a mobile, get its total weight
      ; from its struct inside it.
      (cond ((number? struct) struct)
            ((mobile? struct) (total-weight struct))))
  (+ (get-number (brc-structure (l-brc mob)))
     (get-number (brc-structure (r-brc mob))))))

(define (balanced? mob)
  (let ((lb (l-brc mob))
        (rb (r-brc mob)))
        (=  (* (brc-length lb) (brc-structure lb))
            (* (brc-length rb) (brc-structure rb)))))

; test data
(define la (make-branch 2 3))
(define lb (make-branch 4 5))
(define ra (make-branch 4 5))
(define rb (make-branch 4 (make-mobile la ra)))
(define ba (make-mobile la ra))
(define bb (make-mobile lb rb))
