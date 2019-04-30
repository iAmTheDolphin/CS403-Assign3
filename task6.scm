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

; you know the drill x - y take the abs and compare to threshhold, blah blah blah
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


; reverse the trapezoid math
(define (derivate str step cons)
    ; (A(n) - A(n-1)) * 2 / s - y(left) = y(right)
    
    (define (helper last-area last-y stream) 
        ;find the area of the one trapezoid we are working with
        (define tot-area (stream-car stream))
        (define area (- tot-area last-area))

        ; use that to calculate the y of the right side
        (define math-val 
            (- (/ (* area 2) step) last-y)
        )

        ; next value will rely on the new total area and the y (now on the left side) that we just calculated
        (cons-stream math-val (helper tot-area math-val (stream-cdr stream)))
    )
    (cons-stream (real cons) (helper 0 (real cons) (stream-cdr str)))
)


(define (integrate str step)
    ; (step / 2) * (str(0) + 2 * str(1) + 2 * str(2) .... + str(n))
    ; or 
    ; ((str(0) + str(1)) * step / 2) + ((str(1) + str(2)) * step / 2) + ....

    (define total 0) ; the total area so far
    (define (helper last-y stream)
        (define new-y (stream-car stream))
        ; trapeziod math!!!
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