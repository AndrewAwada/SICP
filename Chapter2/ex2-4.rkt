#lang racket

; Exercise 2.4
(define (cons x y)
  (λ (m) (m x y)))

(define (car z)
  (z (λ (p q) p)))

(define (cdr z)
  (z (λ (p q) q)))

(define l (cons 1 (cons 2 (cons 3 4))))
(= (car l) 1)
(= (car (cdr l)) 2)
(= (car (cdr (cdr l))) 3)
(= (cdr (cdr (cdr l))) 4)

