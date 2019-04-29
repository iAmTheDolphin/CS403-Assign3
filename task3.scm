(define (deque)

    (define (node)
        (define next nil)
        (define prev nil)
        (define data nil)
        (define endpoint? #f)

        (define (set-next! n)
            (set! next n)
        )
        (define (set-prev! p)
            (set! prev p)
        )
        (define (set-data! d)
            (set! data d)
        )

        (define (dispatcher x)
            (cond
                ((eq? x 'get-data) data)
                ((eq? x 'get-next) next)
                ((eq? x 'get-prev) prev)
                ((eq? x 'set-next!) set-next!)
                ((eq? x 'set-prev!) set-prev!)
                ((eq? x 'set-data!) set-data!)
                ((eq? x 'endpoint!) (set! endpoint? #t))
                ((eq? x 'endpoint?) endpoint?)
                (else (println "ERROR--no such function " x))
            )
        )
        dispatcher
    )

    ; displays everything listed as next in the chain
    (define (display-chain-f head)
        (println (head 'get-data))
        (cond
            ((null? (head 'get-next)) 0)
            (else (display-chain-f (head 'get-next)))
        )
    )

    (define (display-chain-b tail)
        (println (tail 'get-data))
        (cond
            ((null? (tail 'get-prev)) 0)
            (else (display-chain-b (tail 'get-prev)))
        )
    )



    ;;;;;;;;;;;;;;;;;;;;;;;;;;;NODE VS DEQUE;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; setting up the head and tail pointers 
    ; so that we have front and back access
    (define head (node))
    (head 'endpoint!)
    (define tail (node))
    (tail 'endpoint!)
    ((head 'set-next!) tail)
    ((tail 'set-prev!) head)

    (define (testing)
        (enqueueBack 5)
        (enqueueBack 6)
        (enqueueBack 7)
        (enqueueFront 9)
        (enqueueFront 10)
        (enqueueFront 11)
        (enqueueFront 12)
        (enqueueBack 13)   
        (display-chain-f head)
        (println "-----")
        (display-chain-b tail)
        (enqueueIndex 999 6)
        (println "size is " size)
        (display-chain-f head)
    )

    (define size 0)


    (define (enqueueFront v)
        (++ size)
        (define temp (node))
        ((temp 'set-data!) v)
        (define nextone (head 'get-next))
        ((nextone 'set-prev!) temp)
        
        ((temp 'set-next!) nextone)
        ((temp 'set-prev!) head)

        ((head 'set-next!) temp)
    )

    (define (enqueueBack v)
        (++ size)
        (define temp (node))
        ((temp 'set-data!) v)
        (define prevone (tail 'get-prev))
        ((prevone 'set-next!) temp)
        ((temp 'set-next!) tail)
        ((temp 'set-prev!) prevone)
        ((tail 'set-prev!) temp)
    )

    (define (dequeueIndex-helper-head i node)
        (cond
            ((null? node) (println "ERROR--Attempted to dequeue at index out of range"))
            ((== i 0) 
                (define rm-node node)
                (define prev (node 'get-prev))
                (define next (node 'get-next))
                ((prev 'set-next!) next)
                ((next 'set-prev!) prev)
                rm-node
            )
            (else
                (dequeueIndex-helper-head (- i 1) (node 'get-next))
            )
        )
    )

    (define (dequeueIndex-helper-tail i node)
        (cond
            ((null? node) (println "ERROR--Attempted to dequeue at index out of range"))
            ((== i 0)
                (define rm-node node)
                (define prev (node 'get-prev))
                (define next (node 'get-next))
                ((prev 'set-next!) next)
                ((next 'set-prev!) prev)
                rm-node
            )
            (else
                (dequeueIndex-helper-tail (- i 1) (node 'get-prev))
            )
        )
    )

    (define (dequeueIndex i)
        (define midIndex (/ size 2))
        (cond
            ((> i midIndex) (dequeueIndex-helper-tail (- size i) tail))
            (else (dequeueIndex-helper-head i (head 'get-next)))
        )
    )

    (define (enqueueIndex-helper-tail v i n)
        (cond
            ((null? n) (println "ERROR--Attempted to enqueue at index out of range"))
            ((== i 0)
                ; (println "Data at index is " (n 'get-data))

                (++ size)

                (define next-node n)
                (define prev-node (n 'get-prev))

                (define cur-node (node))
                ((cur-node 'set-data!) v)

                ((prev-node 'set-next!) cur-node)

                ((cur-node 'set-prev!) prev-node)
                ((cur-node 'set-next!) next-node)

                ((next-node 'set-prev!) cur-node)
            )
            (else 
                (enqueueIndex-helper-tail v (- i 1) (n 'get-prev))
            )
        )
    )

    (define (enqueueIndex-helper-head v i n)
        (cond
            ((null? n) (println "ERROR--Attempted to enqueue at index out of range"))
            ((== i 0)
                (++ size)

                (define prev-node n)
                (define next-node (n 'get-next))

                (define cur-node (node))
                ((cur-node 'set-data!) v)

                ((prev-node 'set-next!) cur-node)

                ((cur-node 'set-prev!) prev-node)
                ((cur-node 'set-next!) next-node)

                ((next-node 'set-prev!) cur-node)
            )
            (else 
                (enqueueIndex-helper-head v (- i 1) (n 'get-next))
            )
        )
    )

    (define (enqueueIndex i v)
        (define midIndex (/ size 2))
        (cond
            ((> i midIndex) (enqueueIndex-helper-tail v (- size i) tail))
            ((== i size) (enqueueBack v))
            (else (enqueueIndex-helper-head v i head))
        )
    )

    (define (dequeueFront)
        (cond 
            ((== size 0) (println "ERROR--Attempted to remove from an empty list"))
            (else 
                (-- size)
                (define rm-node (head 'get-next))
                (define rm-val (rm-node 'get-data))

                (define new-next (rm-node 'get-next))

                ((head 'set-next!) new-next)
                ((new-next 'set-prev!) head)

                rm-val
            )
        )
    )

    (define (dequeueBack)
        (cond
            ((== size 0) (println "ERROR--Attempted to remove from an empty list") )
            (else 
                (-- size)
                (define rm-node (tail 'get-prev))
                (define rm-val (rm-node 'get-data))

                (define new-prev (rm-node 'get-prev))

                ((tail 'set-prev!) new-prev)
                ((new-prev 'set-next!) tail)

                rm-val
            )
        )
    )

    

    (define (display-helper n)
        (print (n 'get-data))
        (cond
            (((n 'get-next) 'endpoint?) 0)
            ((null? (n 'get-next)) 0)
            (else
                (print ",")
                (display-helper (n 'get-next))
            )
        )
    )

    (define (display)
        (print "[")
        (if (eq? (head 'get-next) tail)
            0
            (display-helper (head 'get-next))
        )
        (print "]")
    )

    (define (peekFront)
        ((head 'get-next) 'get-data)
    )

    (define (peekBack)
        ((tail 'get-prev) 'get-data)
    )   


    (define (dispatcher x)
        (cond
            ((eq? x 'test) (print "this is how we do things"))
            ((eq? x 'enqueueFront) enqueueFront )
            ((eq? x 'enqueueBack) enqueueBack )
            ((eq? x 'enqueueIndex) enqueueIndex )
            ((eq? x 'dequeueFront) dequeueFront )
            ((eq? x 'dequeueBack) dequeueBack )
            ((eq? x 'dequeueIndex) dequeueIndex )
            ((eq? x 'display) display )
            ((eq? x 'peekFront) peekFront)
            ((eq? x 'peekBack) peekBack )
            ((eq? x 'size) size)
            ((eq? x 'testing) testing)
            (else ("--INVALID OPTION"))
        )
    )
    dispatcher
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