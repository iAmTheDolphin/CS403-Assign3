
;;
;;  Parker Jones
;;
;;
;; spec : 
    ; Define a function named smush that takes a stream of the form (a, b, c, d, ...) and the operator Φ and returns the stream
    ; ((0, a, a),(1, b, aΦb),(2, c, aΦbΦc),(3, d, aΦbΦcΦd), ...), only flattened. You will also need to define a stream displaying
    ; function named sdisplay. See the example below for its behavior.
    ; Example:
    ; $ cat task5.args
    ; (define ones (cons-stream 1 ones))
    ; (sdisplay 12 (smush ones +))
    ; (println)
    ; $ scam -r task5.scm task5.args
    ; (0,1,1,1,1,2,2,1,3,3,1,4,...)
    ; $
    ; For every triplet, the first number, i, is the index of the second number in the original stream. The third number is the
    ; accumulation of the first zero through i elements. Use left associativity when combining.






; 0 1 2 3 4 5 6 7 8 9 ... wholes
; 1 1 1 1 1 1 1 1 1 1 ... ones
; 1 2 3 4 5 6 7 8 9 10 ... ones smushed with +

; should become what it is if you read it column by column
; 0 1 1 1 1 2 2 1 3 3 1 4 4 1 5 ...


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


(define (add-streams s1 s2)
        (cons-stream (+ (stream-car s1) (stream-car s2)) (add-streams (stream-cdr s1) (stream-cdr s2)))
)


(define (smush stream op)

    (define (stream-smusher last str)
        ;(println "\n " last " op " (stream-car str) " is " (op last (stream-car str)))
        (cons-stream 
            ; the car of the smushed stream. the last smushed item op'd to the next thing in the input stream
            (op last (stream-car str)) 
            ; the cdr of the smushed stream. a recursive call passing the value just calculated (the car) and the next item in the input stream
            (stream-smusher (op last (stream-car str))  (stream-cdr str))  
        )
    )
    (define ones (cons-stream 1 ones))
    (define wholes-stream (cons-stream 1 (add-streams ones wholes-stream)))
    (define smushed-stream (cons-stream (stream-car stream) (stream-smusher (stream-car stream) (stream-cdr stream))))

    ; because this whole thing feels like a finite state machine
    (define (fsm wholes passed-in passed-in-w-op state)
        ; the state of the next thing in the stream will switch between
            ; 1) a whole number representing the number of the particular grouping (see spec)
            ; 2) the next value in the stream that was passed in
            ; 3) every previous value of the passed in stream after they have been smushed together with op
        (cond
            ((== state 1)
                (cons-stream (stream-car wholes) (fsm (stream-cdr wholes) passed-in passed-in-w-op 2))
            )
            ((== state 2)
                (cons-stream (stream-car passed-in) (fsm wholes (stream-cdr passed-in) passed-in-w-op 3))
            )
            ((== state 3)
                (cons-stream (stream-car passed-in-w-op) (fsm wholes passed-in (stream-cdr passed-in-w-op) 1))
            )
        )
    )


    (cons-stream 0 (fsm wholes-stream stream smushed-stream 2))

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