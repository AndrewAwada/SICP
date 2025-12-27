#lang racket

(define zero (λ (f) (λ (x) x)))

(define (add-1 n)
  (λ (f) (λ (x) (f ((n f) x)))))

; (lambda (f) (lambda (x) (f ((zero f) x))))
; (lambda (f) (lambda (x) (f x))
(add-1 zero)

(define one (λ (f) (λ (x) (f x))))

; (lambda (f) (lambda (x) (f ((one f) x))))
; (lambda (f) (lambda (x) (f (f x))))
(add-1 one)

(define two (λ (f) (λ (x) (f (f x)))))

(define (add a b)
  (λ (f) (λ (x) ((a f) ((b f) x)))))

(define (inc n)
  (+ n 1))

(define three (add one two))
(define five (add three two))
(define five-c (add two three))

(= ((zero inc) 0) 0)
(= ((one inc) 0) 1)
(= ((two inc) 0) 2)
(= ((three inc) 0) 3)
(= ((five inc) 0) 5)
(= ((five-c inc) 0) 5)
