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

(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (mul-interval x y)
  (define (cmp-to-zero o1 o2 o3 o4)
    (and (o1 (lower-bound x) 0) (o2 (lower-bound y) 0) (o3 (upper-bound x) 0) (o4 (upper-bound y) 0)))
  (cond 
    [(cmp-to-zero >= >= >= >=)
     (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))] ; case 1
    [(cmp-to-zero <= >= >= >=)
     (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y)))] ; case 2
    [(cmp-to-zero >= <= >= >=)
     (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y)))] ; case 3
    [(cmp-to-zero <= >= <= >=)
     (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))] ; case 4
    [(cmp-to-zero >= <= >= <=)
     (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y)))] ; case 5
    [(cmp-to-zero <= <= <= >=)
     (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))] ; case 6
    [(cmp-to-zero <= <= >= <=)
     (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (lower-bound y)))] ; case 7
    [(cmp-to-zero <= <= <= <=)
     (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y)))] ; case 8
    [(cmp-to-zero <= <= >= >=)
     (make-interval (min (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y)))
                    (max (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))] ; case 9
    ))

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

(equal? (mul-interval-old x2 x3) (mul-interval x2 x3))
(equal? (mul-interval-old x1 x3) (mul-interval x1 x3))
