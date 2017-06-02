#! /usr/bin/env racket
#lang racket
(require racket/trace)

(define (display-list l)
  (define (display-el el)
    (if (null? el)
      (display ")\n")
      (begin
        (display (car el))
        (display " ")
        (display-el (cdr el))
      )
    )
  )
  (display "( ")
  (display-el l)
)

(define a (cons 0 (list 1 2 3 4 5 6 7 8)))
(display-list a)
;(display a)

;===============================================================================
;================================ Матрицы ======================================
;===============================================================================

(define mat1 (list
    (list 1 4 6)
    (list 2 5 7)
    (list 1 0 1)
    (list 1 2 1)
  )
)

(define mat2 (list
    (list 0 0 1 1)
    (list 0 1 0 0)
    (list 1 0 0 1)
  )
)

(define v1 (list 1 2 3 4 5 6))
(define v2 (list 3 2 7 8 8 8))
(define v3 (list 0 0 0 0 1 0))
(define v4 (list 1 2 3))

(define (dot-product arg1 arg2)
  (foldr + 0 (map * arg1 arg2))
)

(define (mat*vector m v)
  (map (lambda (x)
      (dot-product x v)
    )
    m
  )
)

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

(define (transpose m)
  (foldr-n cons null m)
)

(define (mat*mat m1 m2)
  (transpose
    (map (lambda (x)
        (mat*vector m1 x)
      )
      (transpose m2)
    )
  )
)

(define (print-vector vect)
  (display "=================================\n")
  (define (p-v v)
    (if (null? v)
      (display "=================================\n")
      (begin
        (display (list (car v)))
        (display "\n")
        (p-v   (cdr v))
      )
    )
  )
  (p-v vect)
)

(define (print-mat mat)
(display "=================================\n")
  (define (p-m m)
    (if (null? m)
    (display "=================================\n")
      (begin
        (display (car m))
        (display "\n")
        (p-m (cdr m))
      )
    )
  )
  (p-m mat)
)


;(dot-product v3 v2)
;(mat*vector mat1 v4)
;(transpose mat1)
;(mat*mat mat1 mat2)
;(print-mat mat1)
;(print-vector v3)
;===============================================================================
;================================= Tests =======================================
;===============================================================================

(define M1 (list
    (list 1 2)
    (list 3 4)
  )
)
(define M2 (list
    (list 3 2)
    (list 4 5)
  )
)
(define M3 (list
    (list -2 1 4)
    (list 1 3 5)
    (list -3 2 4)
    (list 5 1 2)
  )
)

(define M4 (list
    (list -2 1 4 0)
    (list 1 3 5 0)
    (list -3 2 4 1)
  )
)

(define M5 (list
    (list -2 1 14)
    (list 0 3 5)
    (list -33 2 42)
  )
)


(define E3 (list
    (list 1 0 0)
    (list 0 1 0)
    (list 0 0 1)
  )
)

(define E4 (list
    (list 1 0 0 0)
    (list 0 1 0 0)
    (list 0 0 1 0)
    (list 0 0 0 1)
  )
)

(define V1 (list 5 4))
(define V2 (list 1 7 6))
(define V3 (list 1 2 3 4))
(define V4 (list 7 6 2 1))
(define V5 (list 3 8 9 2))
(print-mat (transpose M1))
(print-mat (transpose M2))
(dot-product V3 V4)
(dot-product V3 V5)
(print-vector (mat*vector M1 V1))
(print-vector (mat*vector M3 V2))
(print-vector (mat*vector M4 V4))
(print-mat (mat*mat M1 M2))
(print-mat (mat*mat M3 M4))
(print-mat (mat*mat M3 E3))
(print-mat (mat*mat M4 E4))
(print-mat (mat*mat M3 M5))
(print-mat (mat*mat E3 M5))
