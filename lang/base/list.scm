;;; list
(define (list . x) x)

;;; length of list
(define (length list)
    (if (null? list)
        0
        (+ 1 (length (cdr list)))))

;;; map a function over a list
(define (map f list)
    (if (null? list)
        '()
        (cons (f (car list)) (map f (cdr list)))))

;;; filter a list with a predicate
(define (filter p list)
    (if (null? list)
        '()
        (if (p (car list))
            (cons (car list) (filter p (cdr list)))
            (filter p (cdr list)))))

;;; fold-left
(define (fold-left f init list)
    (if (null? list)
        init
        (fold-left f (f init (car list)) (cdr list))))

;;; fold-right
(define (fold-right f init list)
    (if (null? list)
        init
        (f (car list) (fold-right f init (cdr list)))))

;;; member?
(define (member? x list)
    (if (null? list) #f
        (or (eq? x (car list))
            (member? x (cdr list)))))

;;; delete-if
(define (delete-if p list)
    (if (null? list)
        '()
        (if (p (car list))
            (delete-if p (cdr list))
            (cons (car list) (delete-if p (cdr list))))))

;;; delete-duplicates
(define (delete-duplicates list)
    (delete-if (lambda (x) (member? x (cdr list))) list))