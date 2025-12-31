#lang racket

(define (make-interval a b)
  (cons a b))

(define (upper-bound interval)
  (cdr interval))

(define (lower-bound interval)
  (car interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (* -1.0 (upper-bound y))
                               (* -1.0 (lower-bound y)))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define x1 (make-interval 1.0 2.0))
(define x2 (make-interval 3.0 4.0))

(= (+ (width x1) (width x2)) (width (add-interval x1 x2)))
(= (+ (width x1) (width x2)) (width (sub-interval x1 x2)))
(not (= (* (width x1) (width x2)) (width (mul-interval x1 x2))))
(not (= (/ (width x1) (width x2)) (width (div-interval x1 x2))))
