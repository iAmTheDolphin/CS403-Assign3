; get the value at index n
(define (stream-ref s n) 
    (if (= n 0) 
        (stream-car s) 
        (stream-ref (stream-cdr s) (- n 1))
    )
) 

; sorta like stream-for-each but it returns a new string
(define (stream-map proc s) 
    (if (stream-null? s) 
        the-empty-stream 
        (cons-stream (proc (stream-car s)) (stream-map proc (stream-cdr s)))
    )
) 

(define tot 16)

; for each car in a stream, run proc
(define (stream-for-each proc s) 
    (cond 
        ((== tot 0) 'done)
        ((stream-null? s) 'done )
        (else 
            (-- tot)
            (proc (stream-car s)) 
            (stream-for-each proc (stream-cdr s))
        )
    )
)

; sums a list
(define (sum l) 
    (if (null? l) 
        0 
        (+ (car l) (sum (cdr l)))
    )
)

; finds the average of a list
(define (average @)(/ (sum @) (length @)))

(define (sqrt-improve guess x) (average guess (/ x guess)))


(define (display-stream s) 
    (stream-for-each display-line s)
) 

(define (display-line x) (newline) (display x))


;; making a stream
(define (sqrt-stream x) 

    ; a stream is a function that calls cons stream
    (define guesses 

        ;; calls cons stream to make the stream
        (cons-stream 

            ;; the starting value of the stream
            1.0 

            ;; a way to map the new values?
            (stream-map 

                ;; this is what happens to the last value to give the next one
                (lambda (guess) 
                    (sqrt-improve guess x)
                ) 
                guesses
            )
        )
    ) 
    guesses
) 

(define (nums a)
    (cons-stream a (nums (+ a 1)))
)

(define (new-stream x)

    (cons-stream x (new-stream(* x x)))
)

(define (sdisplay z n)
    (cond 
        ((= n 0) (println "..."))
        (else
            (print (stream-car z) " ")
            (sdisplay (stream-cdr z) (- n 1))
        )
    )
)

    
(define x-stream (nums 1))
(sdisplay (new-stream 2) 4)