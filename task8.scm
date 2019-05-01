

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
(define (cube x) (* x x x))
(define (stream-map f s) (cons-stream (f (stream-car s)) (stream-map f (stream-cdr s)) ))
;;;----------------------------------------------------------------------------------------


(define (weight l) 
    (+ (cube (car l)) (cube (cadr l)))
)

(define (make-ints next) (cons-stream next (make-ints (+ next 1))))
(define ints (make-ints 1))
;so pairs of numbers. 
; to get every number pair once, i should be <= j
; so we can get (1, 1), (1, 2), (2, 2), (1, 3), (2, 3) and so on
; lowest weight of a pair would be i = 1, j = n
; highest weight of a pair is i = j = n
; need to then find some way to find when the weights ==  each other
(define (ramanujan)

    ; provides the stream of pairs sorted by weight
    ; inspiration by SICP 341
    (define (combine-weighted s1 s2)
        (cond
            ((stream-null? s1) s2)
            ((stream-null? s2) s1)
            ((<= (weight (stream-car s1)) (weight (stream-car s2)))
                (cons-stream (stream-car s1) (combine-weighted (stream-cdr s1) s2))
            )
            (else 
                (cons-stream (stream-car s2) (combine-weighted s1 (stream-cdr s2)))
            )
        )
    )

    ;SICP 340
    (define (pairs s1 s2)
        (cons-stream 
            (list (stream-car s1) (stream-car s2)) 
            (combine-weighted
                (stream-map (lambda (x) (list (stream-car s1) x))
                    (stream-cdr s2))
                (pairs (stream-cdr s1) (stream-cdr s2))
            )
        ) 
    )

    (define (search-for-ramanujan str)
        (if (== (weight (stream-car str)) (weight (stream-car (stream-cdr str))) )
            (cons-stream (weight (stream-car str)) (search-for-ramanujan (stream-cdr str)))
            (search-for-ramanujan (stream-cdr str))
        )
    )

    (define (get-weights str)
        (cons-stream (weight (stream-car str)) (get-weights (stream-cdr str)))
    )
    (define poiu (get-weights (pairs ints ints)))
    ; (sdisplay 70 poiu)
    ; (sdisplay 5 (search-for-ramanujan (pairs ints ints)))
    (search-for-ramanujan (pairs ints ints))
)


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
)

