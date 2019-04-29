
(define (scoping sym obj)

    (define (scoper new-obj level)
        (cond 
            ((null? new-obj) 'undefined)
            ; ((local? sym new-obj) (println "found on level " (get '__level new-obj) " looking for level " level))
            ((local? sym new-obj)
                (if (eq? level 0)
                    ;(println "bound")
                    'bound
                    ;(println "free")
                    'free
                )
            )
            (else (scoper (get '__context new-obj) (+ level 1)))
        )
    )
    (scoper obj 0)
)


(define (main)

    ; (define x ((define (zurp x) (define y (+ x 1)) this) 5))  
    ; (ppTable x)
    ; (ppTable (get '__context x))
    ; (ppTable this)
    ;(ppTable (get '__context this))
    ; (println (scoping 'cs403sucks ((define (zirp x) this) 5)))

    ; (scoping '* ((define (zarp x) this) 5))

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