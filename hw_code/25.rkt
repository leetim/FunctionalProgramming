#! /usr/bin/env racket
#lang racket
(require racket/trace)

(define (my-cons a b)
  (* (expt 2 a) (expt 3 b))
)

(define (get-power v a)
  (cond
    ((not (= 1 (denominator v))) (- (get-power (numerator v) a) (get-power (denominator v) a)))
    ((= 0 (modulo v a)) (+ 1 (get-power (/ v a) a)))
    (else 0)
  )
)

(define (my-car p)
  (get-power p 2)
)

(define (my-cdr p)
  (get-power p 3)
)

(define p (my-cons -1000 -3000000))
(define (get_f arg)
  (not (= 1 (denominator arg)))
)
;(get_f p)
;(get_f (numerator p))
;(get_f (denominator p))
;p
;(numerator p)
;(denominator p)
;(numerator 2.34)
;(denominator 2.5)
(my-car p)
(my-cdr p)
