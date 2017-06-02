#! /usr/bin/env racket
#lang racket
(require racket/trace)

(define (variable? expr)
  (symbol? expr)
)
;(define (same-vars? var1 var2))
(define (same-vars? var1 var2)
  (eq? var1 var2)
)

(define (make-sum expr1 expr2)
  (list '+ expr1 expr2)
)

(define (make-mult expr1 expr2)
  (list '* expr1 expr2)
)

(define (make-power expr1 expr2)
  (list '^ expr1 expr2)
)

(define (sum? expr)
  (eq? (list-ref expr 0) '+ )
)

(define (mult? expr)
  (eq? (list-ref expr 0) '* )
)

(define (power? expr)
  (eq? (list-ref expr 0) '^ )
)

(define (left expr)
  (list-ref expr 1)
)

(define (right expr)
  (list-ref expr 2)
)

(define (diff expr var)
  (cond
    ((number? expr)
      0
    )
    ((variable? expr)
      (if (same-vars? expr var)
        1
        0
      )
    )
    ((sum? expr)
      (make-sum (diff (left expr) var) (diff (right expr) var))
    )
    ((mult? expr)
      (make-sum
        (make-mult (diff (left expr) var) (right expr))
        (make-mult (left expr) (diff (right expr) var))
      )
    )
    ((power? expr)
      (make-mult
        (right expr)
        (make-power
          (left expr)
          (- (right expr) 1)
        )
      )
    )
    (else 'ERROR!!!!)
  )
)

(define (get-simp expr)
  ;(display expr)
  ;(display "\n")
  (if (or (variable? expr) (number? expr))
    expr
    (let
      (
        (left-s (get-simp (left expr)))
        (right-s (get-simp (right expr)))
      )
      (cond
        ((sum? expr)
          (cond
            ((eq? left-s 0) right-s)
            ((eq? right-s 0) left-s)
            (else (make-sum right-s left-s))
          )
        )
        ((mult? expr)
          (cond
            ((eq? left-s 1) right-s)
            ((eq? right-s 1) left-s)
            ((eq? left-s 0) 0)
            ((eq? right-s 0) 0)
            (else (make-mult right-s left-s))
          )
        )
        ((power? expr)
          (cond
            ((eq? right-s 0) 1)
            (else (make-power left-s right-s))
          )
        )
        (else (list (car expr) left-s right-s))
      )
    )
  )
)
(get-simp (diff '(+ x 2) 'x))
(get-simp (diff '(+ x x) 'x))
(get-simp (diff '(* x 2) 'x))
(get-simp (diff '(* x x) 'x))
(get-simp (diff '(* x (* 2 x)) 'x))
(get-simp (diff '(^ x 2) 'x))
(get-simp (diff '(* x (^ x 3)) 'x))
