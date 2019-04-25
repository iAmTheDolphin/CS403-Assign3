(include "streams.scm")

(define (main)

    (define ones-stream (cons-stream 1 (lambda (x) (++ x))))

    (inspect ones-stream)

)