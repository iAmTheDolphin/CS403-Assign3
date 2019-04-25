
(define (scoping sym def)



)

(define (main)

    (define (square x) (* x x))
    (define body (get 'code square))
    (inspect body)

)