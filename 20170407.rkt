#! /usr/bin/env racket
#lang racket
(require racket/trace)
;Элемент списка
(define (listRef l n)
  (if (= n 0)
    (car l)
    (listRef (cdr l) ( - n 1))
  )
)
;Длина списка
(define (Length l)
  (if (null? l)
    0
    (+ 1 (Length (cdr l)))
  )
)
;Склеивание списков
(define (Append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (Append (cdr l1) l2))
  )
)
;Разворот списка
(define (Revers l)
  (if (null? l)
    l
    (Append (Revers (cdr l)) (list (car l)))
  )
)
;Непонятная фигня
(define zero
  (lambda (f) (lambda (x) x))
)
(define one
  (lambda (f) (lambda (x) (f x)))
)
(define (add-1 n)
  (lambda (f) (lambda (x) (f (n f x))))
)
;(trace add-1)
;(add-1 (add-1 one))

;А теперь немного деревьев
;Пиздец как здоровоТ_Т
(define (atom? l)
(not (pair? l))
)
(list (list 1 2) 3 4)
(atom? (list 1 2))
(atom? 2)
;(atom? 2)
;Так то нужен атом
;
;)
;(one 1)
;(define (power e n)
;  (if (= n 0)
;    1
;    (* e (power e (- n 1)))
;  )
;)
;
;(define (n_cons a b)
;  (* (power 2 a) (power 3 b))
;)
;
;(% 4 2)
;(define l1 (list 1 2 3))
;(define l2 (list 4 5 6))
;(Length l1)
;(Append l1 l2)
;(listRef l1 1)
;(listRef l1 1)
;(Revers l1)
