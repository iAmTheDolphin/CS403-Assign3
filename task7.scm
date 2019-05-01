

;;;--------------------------------------UTILITY-------------------------------------------
(define (sdisplay num stream)
    (define (helper n str)
        (cond 
            ((== n 0) (print "..."))
            (else
                (print (stream-car str) ",")
                (helper (- n 1) (stream-cdr str) )
            )
        )
    )
    (print "(")
    (helper num stream)
    (print ")")
) 
(define (stream-multiply s1 s2) (cons-stream (* (stream-car s1) (stream-car s2))  (stream-multiply (stream-cdr s1) (stream-cdr s2)) ))
(define (stream-divide s1 s2) (cons-stream (/ (stream-car s1) (stream-car s2))  (stream-divide (stream-cdr s1) (stream-cdr s2)) ))
(define (stream-add s1 s2)(cons-stream (+ (stream-car s1) (stream-car s2))  (stream-add (stream-cdr s1) (stream-cdr s2)) ))
(define (stream-ref str index) (cond ((== index 0) (stream-car str)) (else (stream-ref (stream-cdr str) (- index 1)))))
(define (square x) (* x x))
(define (stream-map f s) (cons-stream (f (stream-car s)) (stream-map f (stream-cdr s)) ))

(define (euler-transform s)
        (let ((s0 (stream-ref s 0))
                (s1 (stream-ref s 1))
                (s2 (stream-ref s 2))
            )
            (if (eq? 0.0 (+ s0 (* -2.0 s1) s2))

                (cons-stream (- s2 (/  (square (- s2 s1))
                                        0.000000000000000000000000000001)) ; to fix a divide by 0 problem
                            (euler-transform (stream-cdr s)))

                (cons-stream (- s2 (/  (square (- s2 s1))
                                        (+ s0 (* -2.0 s1) s2)))
                            (euler-transform (stream-cdr s)))
                    
                
            )
            
    )
)
;;;----------------------------------------------------------------------------------------


; we need 3 things
; 1) a stream that alternates -,+,-,+,....
; 2) a stream that gives the values of  2!, 4!, 6!, ...
; 3) a stream that gives x^2, x^4, x^6, ...
; then the stream for mystery can just be equvalent to these put together in the right way
(define (mystery x)

    ; the first one
    (define alternator-stream (cons-stream -1.0 (cons-stream 1.0 alternator-stream)))


    (define (factor-accum last-prod x)
        (define new-product (* last-prod x (- x 1.0)))
        (cons-stream new-product (factor-accum new-product (+ x 2.0)))
    )
    ; the second one
    (define factorial-stream (cons-stream 2.0 (factor-accum 2.0 4.0)))


    (define (square-raiser last-x)
        (cons-stream (* last-x x x) (square-raiser (* last-x x x)))
    )
    ; the third one
    ;(define square-stream (cons-stream (* (real x) x) (square-raiser (* (real x) x))))
    (define (next-even last)
        (cons-stream (+ last 2.0) (next-even (+ last 2.0)))
    )
    (define evens-stream (cons-stream 2.0 (next-even 2.0)))
    (define (powers-str evens-str)
        (cons-stream (^ x (stream-car evens-str)) (powers-str (stream-cdr evens-str)))
    )
    (define square-stream (powers-str evens-stream))

    ; put them all together and we get
    (define (mystery-stream-iter fact-str sqr-str alt-str)
        (define math-val (* (/ (stream-car sqr-str) (stream-car fact-str)) (stream-car alt-str)) )
        (cons-stream math-val (mystery-stream-iter (stream-cdr fact-str) (stream-cdr sqr-str) (stream-cdr alt-str)))
    )
    (cons-stream 1.0 (mystery-stream-iter factorial-stream square-stream alternator-stream))
)

;ps-mystery literally just adds mystery up
(define (ps-mystery x)
    (define m-stream (mystery x))
    (define ps-stream (cons-stream (stream-car m-stream) (stream-add (stream-cdr m-stream) ps-stream)) )
)


;mystery, but faster
(define (acc-mystery x)

    ; from SICP page 336
    

    (euler-transform (ps-mystery x))
)

;mystery, but even faster
(define (super-mystery x)

    (define (make-tableau transform s)
        (cons-stream s
                        (make-tableau transform (transform s))
        )
    )

    (define (accelerated-sequence transform s)
        (stream-map stream-car (make-tableau transform s))
    )

    (accelerated-sequence euler-transform (ps-mystery x))
)


(define (symbolic-mystery) (println "(mystery x) is $\\cos x$"))
    


(define (main)
    (define (iterator env)
        (cond 
            ((eof?) 0)
            (else 
                (eval (readExpr) env)
                (iterator env)
            )
        )
    )

    (setPort (open (getElement ScamArgs 1) 'read))
    (iterator this)

    ;(1.0000000000,-2.0000000000,0.6666666667,-0.0888888889,0.0063492063,...)
    ;(1.0000000000,-1.0000000000,-0.3333333333,-0.4222222222,-0.4158730159,...)
)