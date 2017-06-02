#! /usr/bin/env racket
#lang racket
(require racket/trace)

;26
(define (deep-reverse arr)
  (define (rev arr1 buf)
    (if (null? arr1)
      buf
      (rev (cdr arr1) (cons (reverse (car arr1)) buf))
    )
  )
  (rev arr null)
)

(define a (list
  (list 11 12 13 14 15 16)
  (list 21 22 23 24 25 26)
  (list 31 32 33 34 35 36)
  (list 41 42 43 44 45 46)
  )
)
;a
;(deep-reverse a)
;(append a (list 1 2 3))
(define (fringe arr)
  ;(display arr)
  ;(display "\n")
  (cond
    ((null? arr) null)
    ((not (pair? arr)) (cons arr null))
    (else (append (fringe (car arr)) (fringe (cdr arr))))
  )
)
;(fringe a)
;(pair? 2)
(define a1 null)
(fringe a1)

(define a2 (list 1 2 3))
(fringe a2)

(define a3 (list
  (list 11 12)
  (list 21 22)
  )
)
(fringe a3)
(define a4 (list
   (list 11 12 13)
   (list 21 22 23)
   (list 31 32 33)
   )
 )
(fringe a4)


(define a5 (list
   (list 11 12 13)
   (list 21 22 23 (list 31 32 33))
 )
)
(fringe a5)

(define c-push 'push)
(define c-push-back 'push-back)
(define c-push-front 'push-fron)
(define c-pop 'pop)
(define c-pop-back 'pop-back)
(define c-pop-front 'pop-front)
(define c-size 'size)
(define c-empty? 'empty?)






;(define (make-stack)
;  (let ((size 0) (data null))
;    (lambda (comand)
;      (cond
;        ((eq? c-push comand)
;          (lambda (x)
;            (begin
;              (set! size (+ size 1))
;              (set! data (cons x data))
;              (car data)
;            )
;          )
;        )
;        ((eq? c-pop comand)
;          (begin
;            (define temp null)
;            (set! size (- size 1))
;            (set! temp (car data))
;            (set! data (cdr data))
;            temp
;          )
;        )
;        ((eq? c-size comand) size)
;        (else (display "Error!\n"))
;      )
;    )
;  )
;)
;
;(define (get-stack stack size)
;  (if (> size 0)
;
;  )
;)
;
;(define (make-deq)
;  (let front-stack (make-stack))
;  (let back-stack (make-stack))
;  (lambda (comand)
;    ((eq? c-push-front comand) (front-stack c-push))
;    ((eq? c-push-back comand) (back-stack c-push))
;    ((eq? c-pop-back comand)
;      (if (> (back-stack c-size) 0)
;        (back-stack c-pop)
;        ()
;      )
;    )
;  )
;)
;
;
;
;(define stack (make-stack))
;((stack c-push) 10)
;((stack c-push) 20)
;((stack c-push) 30)
;(stack c-pop)
;(stack c-size)
