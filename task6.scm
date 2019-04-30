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

(define (same-stream? s1 s2 count thr)
    (define num1 (stream-car s1))
    (define num2 (stream-car s2))
    (define dif (abs (- (real num1)  (real num2))))
    (cond
        ((== count 0) #t)
        ((>= dif thr) #f)
        (else
            (same-stream? (stream-cdr s1) (stream-cdr s2) (- count 1) thr)
        )
    )
)

(define (derivate str step cons)
    (define (helper last-area last-y stream) 
        (define tot-area (stream-car stream))
        (define area (- tot-area last-area))

        (define math-val 
            (- (/ (* area 2) step) last-y)
        )

        (cons-stream math-val (helper tot-area math-val (stream-cdr stream)))
    )
    (cons-stream (real cons) (helper 0 (real cons) (stream-cdr str)))
)

(define (integrate str step)
    ; (step / 2) * (str(0) + 2 * str(1) + 2 * str(2) .... + str(n))
    ; or 
    ; ((str(0) + str(1)) * step / 2) + ((str(1) + str(2)) * step / 2) + ....
    (define total 0)
    (define (helper last-y stream)
        (define new-y (stream-car stream))
        (define area (/ (* step (+ last-y new-y 0.0)) 2))
        (+= total area)
        (cons-stream total (helper new-y (stream-cdr stream)))
    )
    (cons-stream (real 0) (helper (stream-car str) (stream-cdr str)))
)

(define (quad x2 x c step)
    (define (helper next-x)
        (define first-num (* x2 (* 1.0 next-x next-x) ))
        (define second-num (* 1.0 x next-x))
        (cons-stream (+ first-num second-num c) (helper (+ next-x step)) )
    )
    (helper 0)
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