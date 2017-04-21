#! /usr/bin/env racket
#lang racket
(require racket/trace)
;(define nil 0)
;cons -- Склейка
;car -- Получение первого элемента
;cdr -- 2-го
;nil -- конец списка
;null? -- проверка на конец списка
(define x
  (cons 1 2)
)
(define y
  (cons 3 4)
)
(define z
  (cons x y)
)
;Печать списка
(define (puts arg)
  (define (print-list-structure arg)
    (define (print-contents x)
      (print-list-structure (car x))
      (cond
        ((null? (cdr x)) null)
        ((not (pair? (cdr x)))
          (display " . ")
          (print-list-structure (cdr x))
        )
        (else
          (display " ")
          (print-list-structure (cdr x))
        )
      )

    )
    (cond
      ((null? arg) (display "()"))
      ((not (pair? arg)) (display arg))
      (else
        (print-contents arg)
        ;(display ")")
      )
    )
  )
  (display "(")
  (print-list-structure arg)
  (display ")\n")
)
;Последовательности
;Пусть дано дерево чисел в качестве листьев, посчитать сумму квадратов листов которые не четные
(define (sum-odd-sq t)
  (cond
    ((null? t) 0)
    ((not (pair? t))
      (if (odd? t)
        (* t t)
        0
      )
    )
    (else
      (+
        (sum-odd-sq (car t))
        (sum-odd-sq (cdr t))
      )
    )
  )
)

;Дано: натуральное число N
;Результат: список чисел фибоначи {Fib_k: 1 <= k <= N}
(define (fib-list n)
  (define (next k)
    (if (> k n)
      null
      (if (even? (fib k))
        (cons (fib k) (next (+ 1 k)))
        (next (+ 1 k))
      )
    )
  )
  (define (fib k)
    (if (< k 2)
        k
        (+ (fib (- k 1)) (fib (- k 2)))
    )
  )
  (next 0)
)
;Другая реализация
;(define (fib-l2 n)
;  (cond
;    ((= n 0) nil)
;  )
;)

;map
;(define (map f l)
;  (if (null? l)
;    null
;    (cons
;      (f (car l))
;      (map f l)
;      )
;  )
;)
;reduce or accumulate
;flatten
;Дано дерево
;Необходимо вывести его в виде списка
(define (flatten t)
  (cond
    ((null? t) null)
    ((not (pair? t)) (list t))
    (else
      (append (flatten (car t)) (flatten (cdr t)))
    )
  )
)

;a, b числа
;сформировать список всех элементов
(define (enumirate-interval a b)
  (define ei enumirate-interval)
  (cond
    ((= a b) (cons a null))
    ((< a b) (cons a (ei (+ a 1) b)))
    ((> a b) null)
  )
)

;Что-то непонятное про стили программирования
;Существует конвеерная обработка
;Существует последовательная обработка
;(define (sum-odd-sq2 t)
;  (reduce + 0
;    (map square
;      (filter odd? (flatten t))
;    )
;  )
;)

;accumulate-n
;Дано: список из одинакового кол-ва элементов.
;Необходимо применить accumulate к каждому из этих списков
(define (accumulate-n func base ll)
  (map (lambda (x) (foldl func base x)) ll)
)
;Домашнее задание: Матричные операции
; dot-product(V1, V2) -- Два вектора, нужно найти скалярное произведение
; matrix_x_vector(M, V) -- Матрица на вектор
; matrix_x_matrix(M1, M2) -- Матрица умножить на матрицу


;///////////////////////////////////////////////////////////////////////////////
(define tree (list (list 1 2 3) (list 4 (list 5 6) 7) (list 8 9 10)))
;printing
;x
;y
;z
;(puts (cons 1 (cons 2 (cons 3 (cons 4 null)))))
;(puts (list 1 2 3 4))
;(cons 1 (cons 2 3))
;(define (print-tree t)
;  (define (pt t lvl)
;
;  )
;)
;(fib-list 20)
;(flatten tree)
;(cons 1 (cons 2 3))
;print-list-structure z ;((1 . 2) 3. 4)
;(enumirate-interval 5 100)
