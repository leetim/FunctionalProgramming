#! /usr/bin/env racket
#lang racket
(require racket/trace)

(define (enum a b)
  (if (> a b)
    null
    (cons a (enum (+ a 1) b))
  )
)

(define (enum-3 a b)
  (define range (enum a b))
  (foldr append null
    (foldr append null
      (map (lambda (x)
          (map (lambda (y)
              (map (lambda (z)
                  (list x y z)
                )
                range
              )
            )
            range
          )
        )
        range
      )
    )
  )
)

(define (gen-3 s n)
  (filter
    (lambda (thr)
      (let (
          (i (list-ref thr 0))
          (j (list-ref thr 1))
          (k (list-ref thr 2))
        )
        ;(display (list i j k))
        ;(display "\n")
        (and
          ;(display (= (+ i j k) s))
          (= (+ i j k) s)
          (<= 1 k)
          (< k j)
          (< j i)
          (<= i n)
        )
      )
    )
    (enum-3 1 n)
  )
)
(gen-3 6 4)
(gen-3 6 100)
(gen-3 10 4)
(gen-3 10 6)
(gen-3 20 15)
