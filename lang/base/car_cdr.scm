;;; two layers of car and cdr
(define (caar x)
    (car (car x)))

(define (cadr x)
    (car (cdr x)))

(define (cdar x)
    (cdr (car x)))

(define (cddr x)
    (cdr (cdr x)))

;;; three layers of car and cdr
(define (caaar x)
    (car (caar x)))

(define (caadr x)
    (car (cadr x)))

(define (cadar x)
    (car (cdar x)))

(define (caddr x)
    (car (cddr x)))

(define (cdaar x)
    (cdr (caar x)))

(define (cdadr x)
    (cdr (cadr x)))

(define (cddar x)
    (cdr (cdar x)))

(define (cdddr x)
    (cdr (cdr x)))

;;; four layers of car and cdr
(define (caaaar x)
    (car (caaar x)))

(define (caaadr x)
    (car (caadr x)))

(define (caadar x)
    (car (cadar x)))

(define (caaddr x)
    (car (caddr x)))

(define (cadaar x)
    (car (cdaar x)))

(define (cadadr x)
    (car (cdadr x)))

(define (caddar x)
    (car (cddar x)))

(define (cadddr x)
    (car (cdddr x)))

(define (cdaaar x)
    (cdr (caaar x)))

(define (cdaadr x)
    (cdr (caadr x)))

(define (cdadar x)
    (cdr (cadar x)))

(define (cdaddr x)
    (cdr (caddr x)))

(define (cddaar x)
    (cdr (cdaar x)))

(define (cddadr x)
    (cdr (cdadr x)))

(define (cdddar x)
    (cdr (cddar x)))

(define (cddddr x)
    (cdr (cdddr x)))