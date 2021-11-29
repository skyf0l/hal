;;; abs
(define (abs x)
  (if (< x 0) (- 0 x) x))

;;; zero?
(define (zero? n)
  (= n 0))

;;;positive?
(define (positive? n)
  (> n 0))

;;;negative?
(define (negative? n)
  (< n 0))

;;;even?
(define (even? n)
  (= 0 (% n 2)))

;;;odd?
(define (odd? n)
  (not (= 0 (% n 2))))

;;; pow
(define (pow x y)
   (if (= y 0) 1
     (if (= y 1) x
       (* x (pow x (- y 1))))))

;;; sum
(define (sum l)
    (if (null? l)
        0
        (+ (car l) (sum (cdr l)))))

;;; product
(define (product l)
    (if (null? l)
        1
        (* (car l) (product (cdr l)))))