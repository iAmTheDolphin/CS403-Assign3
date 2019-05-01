;;
;;  Parker Jones
;;
;;
;;
;; spec : 
    ; Define a series of functions for mathematically manipulating a quadratic equation represented as a stream. The first,
    ; quad, takes the three coefficients of the quadratic plus a step value. For example, (quad 1 3 -4 0.01) would produce
    ; the stream representing the equation f = x
    ; 2 + 3x − 4. The step argument, s, gives the distance between evaluations of
    ; the quadratic, starting at zero. The stream produced would be equivalent to (f(0), f(s), f(2s), f(3s), ...).
    ; The second function, integrate, produces a stream representing the integral of the given stream. You should use the
    ; trapezoid method to compute the area under the curve. The first element of the integrated stream would be zero; the
    ; second element would be computed from a trapezoid having f(0) as the left height and f(s) as the right height. The
    ; integrate function takes two arguments, the stream to integrate and the step size. The i
    ; th element of the resulting
    ; stream should approximate:
    ; ∫ i∗s
    ; 0
    ; f(x)dx
    ; The third function, derivate, produces a stream representing the derivative of the given stream. It should reverse the
    ; integration process. The derivate function takes three arguments, the stream to differentiate, the step size, and a
    ; constant. If f(0) is given as the constant, then an integrated quadratic stream passed to derivate should produce the
    ; original quadratic stream.
    ; A fourth function, same-stream? will help you decide if a differentiated integral stream is the same as the original
    ; stream. Given two streams, s1 and s2, a count n, and a positive threshold t, returns true if all of the first nth elements
    ; (piecewise) differ by less than t and false otherwise.


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

; you know the drill. x - y take the abs and compare to threshhold, blah blah blah
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

; simulate a quadratic equation ax^2 + bx + c
(define (quad a b c step)
    (define (helper next-x)
        (define first-num (* a (* 1.0 next-x next-x) ))
        (define second-num (* 1.0 b next-x))
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