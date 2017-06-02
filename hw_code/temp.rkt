#! /usr/bin/env racket
#lang racket
(require racket/trace)

;Символьное дифференцирование
;const
;var
;+
;*
;Предикаторы
;(define (variable? expr))
(define (variable? expr)
  (symbol? expr)
)
;(define (same-vars? var1 var2))
(define (same-vars? var1 var2)
  (eq? var1 var2)
)
(define (sum? expr)
  (eq? (car expr) '+)
)
(define (left expr)
  (car (cdr expr))
)
(define (right expr)
  (car (cdr (cdr expr)))
)
;(define (make-sum expr1 expr2))
(define (make-sum expr1 expr2)
  (list '+ expr1 expr2)
)
(define (mult? expr)
  (eq? (car expr) '*)
)
;(define (multiplier expr))
;(define (multipler expr))
;(define (make-mult expr1 expr2))
(define (make-mult expr1 expr2)
  (list '* expr1 expr2)
)

(define (exp? expr)
  (eq? (car expr) 'e^)
)

(define (make-exp expr1)
  (list 'e^ expr1)
)

(define (power? expr)
  (eq? (car expr) 'e^)
)

(define (make-power expr1 expr2)
  (list '^ expr1 expr2)
)
;конструкторы
;selecter
(variable? 2)
(define (diff expr var)
  (cond
    ((number? expr) 0)
    ((variable? expr)
      (if (same-vars? expr var)
        1
        0
      )
    )
    ((sum? expr) (make-sum
      (diff (left expr) var)
      (diff (right expr) var)
    ))
    ((mult? expr) (make-sum
      (make-mult
        (diff (left expr) var)
        (right expr)
      )
      (make-mult
        (left expr)
        (diff (right expr) var)
      )
    ))
    ((exp? expr)
      ;(display expr)
      (make-mult
        expr
        (diff (left expr) var)
      )
    )
  )
)

(define (get_simp tree)
  (if (or (number? tree) (variable? tree))
    tree
    (let ((left-t (get_simp (left tree))) (right-t (get_simp (right tree))))
      (cond
        ((sum? tree)
          (cond
            ((eq? left-t 0)
              (get_simp right-t)
            )
            ((eq? right-t 0)
              (get_simp left-t)
            )
            (else (list '+ (get_simp left-t) (get_simp right-t)))
          )
        )
        ((mult? tree)
          (cond
            ((eq? left-t 1)
              (get_simp right-t)
            )
            ((eq? right-t 1)
              (get_simp left-t)
            )
            ((eq? left-t 0)
              0
            )
            ((eq? right-t 0)
              0
            )
            (else (list '* (get_simp left-t) (get_simp right-t)))
          )
        )
        (else tree)
      )
    )
  )
)

;Сгенерировать все тройки (i, j, k): 1 <= k < j < i <= n, i + j + k == s

;###############################################################################
;##################################Тесты########################################
;###############################################################################
;(diff '(* 5 x) 'x)
(diff '(e^ (+ (* 5 x) (* x x))) 'x)
(get_simp (get_simp (get_simp (diff '(e^ (+ (* 5 x) (* x x))) 'x))))
;(diff '(+ x (e^ (* x x))) 'x)
;(diff '(+ x (e^ x)) 'x)
;(get_simp (diff '(+ x (* x x)) 'x))

;инфексная форма записи со всеми скобками
