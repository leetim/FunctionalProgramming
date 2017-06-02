#! /usr/bin/env racket
#lang racket
(require racket/trace)
(require racket/stream)
(require math/number-theory)
;(stream-first ones)
;(stream-rest ones)

(define (add-stream s1 s2)
  (stream-cons (+ (stream-first s1) (stream-first s2)) (add-stream (stream-rest s1) (stream-rest s2)))
)

(define (scale-stream c s)
  (stream-map (lambda (x) (* x c)) s)
)

;===============================================================================
;============================= Интегрирование ==================================
;===============================================================================
;Получить значение интегралла g(t) = y_0 + integral(t, 0, y(tau) dtau)
; x - Поток
; результат это поток частичных сумм

(define (integral x y0 dt)
  (define int
    (stream-cons y0 (add-stream (scale-stream dt x) int))
  )
  int
)
(define (f x)
  (sin x)
)
pi
(define dt (/ pi 6))
(define ones (stream-cons 1 ones))
(define numbs (stream-cons 0 (add-stream ones numbs)))
(define sin-stream (stream-map f (scale-stream dt numbs)))
(define res (integral sin-stream 0 dt))
(stream-ref res 0)
(stream-ref res 1)
(stream-ref res 2)
(stream-ref res 3)
(stream-ref res 4)
(stream-ref res 5)
(stream-ref res 6)
(stream-ref res 7)
;===============================================================================
;============================== Задача Коши ====================================
;===============================================================================
;dy/dt = f(y) y(0) = y_0
(define (int-d x-d y0 dt)
  (define int
    (stream-cons y0
      (add-stream (scale-stream dt (force x-d)) int)
    )
  )
  int
)
(define (alkachi f y0 dt)
  (define y (int-d (delay dydt) y0 dt))
  (define dydt (stream-map f y))
  y
)
'===================================================
(set! res (alkachi (lambda (x) (* x x)) 10 dt))
(stream-ref res 0)
(stream-ref res 1)
(stream-ref res 2)
(stream-ref res 3)
(stream-ref res 4)
(stream-ref res 5)
(stream-ref res 6)
(stream-ref res 7)
'===================================================
;===============================================================================
;============================== Задача Коши ====================================
;===============================================================================
;Что-то связанное с банковскими счетамиТ_Т
;Моделирование входных сзначений с помощью потока выходных

(define (make-withdraw b)
  (lambda (a)
    (set! b (- b a))
    b
  )
)
(define bank (make-withdraw 100.0))
(bank 10)
(bank 10)
(bank 10)
(bank 0)
(bank -10)
(bank 10)
'===================================================
;===============================================================================
;===============================================================================
;===============================================================================
(define (stream-withdraw b a-s)
  (stream-cons b (stream-map (make-withdraw b) a-s))
)

(define (stream-withdraw2 b a-s)
  (stream-cons b (stream-withdraw2 (- b (stream-first a-s)) (stream-rest a-s)))
)
;Разница двух способов состоит в том, что в первом мы не явно использовали
;модификатор set!, т.е. как выражается Шевченко "Заболели болезнью императивного
;стиля программирования"

(define bank-stream (stream-withdraw 100 ones))
(stream-ref bank-stream 0)
(stream-ref bank-stream 1)
(stream-ref bank-stream 2)
(stream-ref bank-stream 3)
(stream-ref bank-stream 4)
(stream-ref bank-stream 5)
(stream-ref bank-stream 6)
(stream-ref bank-stream 7)
;===============================================================================
;========================== Общий банковский счет ==============================
;===============================================================================
;Пусть дан общий счет, к которому имеет доступ два субъекта p_1 и p_2.
(define p1 (stream-map (lambda (x) (* x x)) numbs))
(define p2 (stream-map (lambda (x) (* 3 x)) numbs))
;Полезная функция (не рассматривалась в лекциях)
(define (get-list-stream s a b)
  (if (> a b)
    null
    (cons (stream-ref s a) (get-list-stream s (+ a 1) b))
  )
)
(get-list-stream p2 1 10)

;===============================================================================
;============================ Темы на экзамене =================================
;===============================================================================

;1) lambda-исчесление
;2) Объекты
;3) Потоки
