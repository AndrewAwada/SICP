#lang racket

; Exercise 2.5
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(= (cons 1 2) 18)

(define (car p)
  (define (iter x n)
    (if (even? x)
        (iter (/ x 2) (+ n 1))
        n))
  (iter p 0))

(= (car (cons 1 2)) 1)
(= (car (cons 42 (cons 2 3))) 42)
(= (car (cons (cons 1 2) (cons 2 3))) 18)

(define (cdr p)
  (define (iter x n)
    (if (zero? (remainder x 3))
        (iter (/ x 3) (+ n 1))
        n))
    (iter p 0))

(= (cdr (cons 1 2)) 2)
(= (car (cdr (cons 42 (cons 2 3)))) 2)
(= (cdr (cdr (cons 42 (cons 2 3)))) 3)
(= (cdr (cdr (cons (cons 1 2) (cons 2 3)))) 3)
(= (car (cdr (cons (cons 1 2) (cons 7 3)))) 7)
(= (cdr (car (cons (cons 1 5) (cons 2 3)))) 5)
