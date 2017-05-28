#! /usr/bin/env racket
#lang racket
(require racket/trace)
(require racket/stream)

;(define (make-table)
;  (let data null)
;  (lambda (oper)
;    (cond
;      ((eq? oper 'lookup))
;    )
;  )
;)
;
;(define (memoize f)
;  (let ((t (make-table)))
;    (lambda (x)
;      (let ((already-computed (lookup x t)))
;        (if (not already-computed)
;          (let ((fx (f x)))
;            (insert! x fx t)
;            )
;          fx
;        )
;      )
;    )
;  )
;)
;
;(define memfib
;  (memoize (lambda (n)
;      (cond
;        ((= n 0) 0)
;        ((= n 1) 1)
;        (else
;          (+ (memfib (- n 1)) (memfib (- n 2)))
;        )
;      )
;    )
;  )
;)

;===============================================================================
;===================================Потоки======================================
;===============================================================================
; РАссмотрим задачу:
; Найти все простые числа от 10^4 до 10^6
;(filter prime? (enumirate-interval 1000 10000))
; car cdr
;Имеется
;#########################
;cons-stream
;stream-car
;stream-cdr
;stream-null?
;the-empty-stream
;#########################
;delay - принимает на вход выражение и возвращает задержаный объект
;force - оценивает объект

(define (cons-stream h t)
  (display "in cons\n")
  (cons h (delay t))
)
(define (stream-car s)
  (car s)
)
(define (stream-cdr s)
  (force (cdr s))
)
(define (stream-null? s)
  (null? s)
)
(define the-empty-stream null)


(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s))
    )
  )
)

(define (display-line x)
  (display x)
  (display "\n")
)

(define (stream-print s)
  (stream-for-each display-line s)
)

(define (stream-filter p s)
  (cond
    ((stream-null? s) the-empty-stream)
    ((p (stream-car s)) (cons-stream (stream-car s) (stream-filter p (stream-cdr s))))
    (else (stream-filter p (stream-cdr s)))
  )
)

(define (stream-enum-interval a b)
  (if (> a b)
    the-empty-stream
    (cons-stream a (stream-enum-interval (+ a 1) b))
  )
)
(define nums (stream-enum-interval 0 10))
(stream-print nums)
(define f (stream-filter (lambda (x) (= 0 (remainder x 2))) nums))
(stream-print f)

;===============================================================================
;========================Реализация delay force=================================
;===============================================================================

(define (delay-1 expr)
  (lambda () expr)
)

(define (force-1 d)
  (d)
)

(define (memo-proc p)
  (let ((flag #f) (result #f))
    (lambda ()
      (if flag
        result
        (begin
          (set! result (p))
          (set! flag #t)
          result
        )
      )
    )
  )
)

(define (delay-2 expr)
  (memo-proc expr)
)

(define (force-2 d-object)
  (d-object)
)

;(define (show x)
;  (display-line x)
;  x
;)

;===============================================================================
;======================Потенциально бесконечные потоки==========================
;===============================================================================

(define (integer-starting n)
  (cons-stream
    n
    (integer-starting (+ n 1))
  )
)
(define integers (integer-starting 1))
(cdr integers)
