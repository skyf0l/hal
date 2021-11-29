;;; counter
(define (make-counter value)
  (lambda ()
    (set! value (+ value 1))
    value
  )
)

;;; (define my-counter (make-counter 10))
;;; (my-counter) -> 11
;;; (my-counter) -> 12
;;; (my-counter) -> 13