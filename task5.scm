
(define (sdisplay z n)
    (cond 
        ((= n 0) (println "..."))
        (else
            (print (stream-car z) " ")
            (sdisplay (stream-cdr z) (- n 1))
        )
    )
) 