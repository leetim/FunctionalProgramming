#! /usr/bin/env racket
#lang racket
(require racket/trace)

;map
;filter
;foldr
;e = l1 l2 l3 ... ln
;
(define matr
  (list
    (list 1 2 3 4)
    (list 5 6 7 8)
    (list 9 10 11 12)
    (list 13 14 15 16)
  )
)

(define v (list 1 2 3 4))
(define u (list 1 2 3 4))

;Скалярное произведение
(define (dot u v)
  (define (make-pair u v)
    (if (null? u)
      null
      (cons
        (cons (car u) (car v))
        (make-pair (cdr u) (cdr v))
      )
    )
  )
  (foldr + 0
    (map (lambda (x) (* (car x) (cdr x)))
      (make-pair u v)
    )
  )
)

;Число на вектор
(define (a*m a vect)
  (map
    (lambda (x) (* x a))
    vect
  )
)

;Транспонирование
(define (transpose mat)
  (define (foldr-n f start arr)
    (if (null? (car arr))
      null
      (cons
        (foldr f start (map car arr))
        (foldr-n f start
          (map cdr arr)
        )
      )
    )
  )
  (foldr-n cons null mat)
)

;Умножение матриц
;(define (mxn m n)
;  (
;
;  )
;)

;flatmap
(define (flat-map f l)
  (foldr append null (map f l))
)

;перенумерация
(define (range start end)
  (if (>= start end)
    null
    (cons
      start
      (range (+ start 1) end)
    )
  )
)

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
);конструкторы
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
        (right expr)
        (diff (left expr) var)
      )
    ))
  )
)
(define (get_simp tree)
  (cond
    ((sum? tree)
      (cond
        ((eq? (left tree) 0)
          (get_simp (right tree))
        )
        ((eq? (right tree) 0)
          (get_simp (left tree))
        )
        (else (list '+ (get_simp (left tree)) (get_simp (right tree))))
      )
    )
    ((mult? tree)
      (cond
        ((eq? (left tree) 1)
          (get_simp (right tree))
        )
        ((eq? (right tree) 1)
          (get_simp (left tree))
        )
        ((eq? (left tree) 0)
          0
        )
        ((eq? (right tree) 0)
          0
        )
        (else (list '* (get_simp (left tree)) (get_simp (right tree))))
      )
    )
    (else tree)
  )
)

;Сгенерировать все тройки (i, j, k): 1 <= k < j < i <= n, i + j + k == s

;###############################################################################
;##################################Тесты########################################
;###############################################################################
(dot u v)
(transpose matr)
(range 2 7)
matr
(cdr '((x1 x2) (y1 y2)))
(diff '(+ x (* x x)) 'x)
(get_simp (diff '(+ x (* x x)) 'x))

;инфексная форма записи со всеми скобками
