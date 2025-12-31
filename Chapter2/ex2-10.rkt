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
  (if (and (<= (lower-bound y) 0.0) (>= (upper-bound y) 0.0))
      (error "dividing by interval that spans 0.0")
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

(define (sub-interval x y)
  (add-interval x
                (make-interval (* -1.0 (upper-bound y))
                               (* -1.0 (lower-bound y)))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))

(define x1 (make-interval 0.0 0.0))
(define x2 (make-interval -1.0 4.0))
(define x3 (make-interval 2.0 3.0))

(div-interval x1 x3) ; no error
(div-interval x2 x3) ; no error
(div-interval x3 x2) ; error
(div-interval x3 x1) ; error
