#! /usr/bin/env racket
#lang racket
(require racket/trace)
(require racket/stream)
(require math/number-theory)
;(stream-first ones)
;(stream-rest ones)

;===============================================================================
;======================Потенциально бесконечные потоки==========================
;===============================================================================

(define (integer-starting n)
  (stream-cons
    n
    (integer-starting (+ n 1))
  )
)

(define integers (integer-starting 1))
(define zero (stream-cons 0 zero))
(define ones (stream-cons 1 ones))
(define (add-stream s1 s2)
  (stream-cons (+ (stream-first s1) (stream-first s2)) (add-stream (stream-rest s1) (stream-rest s2)))
)
(define ex (add-stream integers ones))
(stream-first ex)
(stream-first (stream-rest ex))
;Определение потоков неявным образом
;Хвостовая точечная нотация
(define (f x y . z)
  (map (lambda (arg) (+ x (* arg y)))
  z)
)
(f 1 2 3 4 5 6)
;
;(apply func some-list) --- выполняет вызов функции с аргументами из листа
(apply f (list 1 2 3 4 5 6))

;===============================================================================
(define (our-stream-map f . arg-stream)
  (stream-cons
    (apply f (map stream-first arg-stream))
    (apply our-stream-map (append (list f) (map stream-rest arg-stream)))
  )
)

(define (add-stream-2 s1 s2)
  (our-stream-map + s1 s2)
)

(set! ex (add-stream-2 integers ones))
(stream-first ex)
(stream-rest ex)
(stream-first (stream-rest ex))
;===============================================================================

(define integers-2
  (stream-cons 1 (add-stream integers ones))
)

(define fib
  (stream-cons
    0
    (stream-cons
      1
      (add-stream fib (stream-rest fib))
    )
  )
)

  (stream-first fib)
  (stream-first (stream-rest fib))
  (stream-first (stream-rest (stream-rest fib)))
  (stream-first (stream-rest (stream-rest (stream-rest fib))))
  ;===============================================================================
;Домашнее задание
;Хемминг
;Перечислить последовательность чисел, которые делятся на 2 3 5
;1)Начинается с 1
;2) (scale-s 2 s) \in s
;3) (scale-s 3 s) \in s
;4) (scale-s 5 s) \in s
;===============================================================================
;===============================================================================
(define (scale-stream c s)
  (stream-map (lambda (x) (* x c)) s)
)

(define (merge s1 s2)
  (cond
    ((< (stream-first s1) (stream-first s2))
      (stream-cons
        (stream-first s1)
        (merge (stream-rest s1) s2)
      )
    )
    ((> (stream-first s1) (stream-first s2))
      (stream-cons
        (stream-first s2)
        (merge (stream-rest s2) s1)
      )
    )
    (else
      (stream-cons
        (stream-first s1)
        (merge (stream-rest s1) (stream-rest s2))
      )
    )
  )
)
(list 'Вот 'так 'воть)
(define hem-l (stream-cons 1 (merge (scale-stream 2 integers) (merge (scale-stream 3 integers) (scale-stream 4 integers)))))
(stream-ref hem-l 1)
(stream-ref hem-l 2)
(stream-ref hem-l 3)
(stream-ref hem-l 4)
(stream-ref hem-l 5)
(stream-ref hem-l 6)
(stream-ref hem-l 7)
(stream-ref hem-l 8)
(stream-ref hem-l 9)
;===============================================================================
;Построить тройки чисел, которые:
;(i, j, i+j - простое)
;Нумерация Кантора
;C: N^2 --> N
;=================================!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;C(x, y) = (x+y)(x+ y + 1)/2 + y =!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;=================================!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(list 'Вот 'так 'воть)
(define (C x y)
  (+ y (/ (* (+ x y) (+ x y 1)) 2))
)
;(define (C-1 n)
;  ()
;)
(C 0 0)
(C 1 0)
(C 0 1)
(C 2 0)
(C 1 1)
(C 0 2)
(C 2 1)
(C 1 2)
;===============================================================================
;===============================================================================
;===============================================================================
(define (interleave s1 s2)
  (stream-cons
    (stream-first s1)
      (interleave s2 (stream-rest s1))
  )
)

;Поток пар
(define (pairs-stream s t)
  (stream-cons
    (cons (stream-first s) (stream-first t))
    (interleave
      (stream-map (lambda (x) (cons (stream-first s) x)) t)
      (pairs-stream (stream-rest s) (stream-rest t))
    )
  )
)
(define pairs-s (stream-filter
  (lambda (x) (prime? (+ (car x) (cdr x))))
  (pairs-stream integers integers))
)
(stream-ref pairs-s 1)
(stream-ref pairs-s 2)
(stream-ref pairs-s 3)
(stream-ref pairs-s 4)
(stream-ref pairs-s 5)
(stream-ref pairs-s 6)
(stream-ref pairs-s 7)
(stream-ref pairs-s 8)
(stream-ref pairs-s 9)
(stream-ref pairs-s 10)
(stream-ref pairs-s 11)
;===============================================================================
;===============================================================================
;===============================================================================
;S(x, y)
;S(1, 100)
;S(100, 99)
;S(99, 100)
