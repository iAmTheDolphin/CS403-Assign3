

(define (sdisplay z n)
    (cond 
        ((== n 0) (println "..."))
        (else
            (print (stream-car z) " ")
            (sdisplay (stream-cdr z) (- n 1))
        )
    )
) 


(define (smush stream op)

    (define (add-streams s1 s2)
        (cons-stream (+ (stream-car s1) (stream-car s2)) (add-stream (stream-cdr s1) (stream-cdr s2)))
    )

    (define ones (cons-stream 1 ones))

    (sdisplay ones 5)



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