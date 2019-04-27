; Parker Jones
; task2.scm
; Define a function named replace that replaces all occurrences of a given symbol in a function definition with a given value. 
; You can obtain the body of a function definition as follows: 
;     (define (square x) (* x x))
;     (define body (get 'code square))
;     (inspect body)
; You can get the formal parameters by substituting 'parameters for 'code in the get expression. The code inspection displays the list (begin (* x x)) whose car is 
; the symbol begin and whose cadr is the list (* x x). 

; You can update the body of the function as follows: 
;     (set 'code '(begin (+ x x)) square)
;     (inspect (square 5))
; The inspection should display the value 10. Likewise, you can update the formal parameters. 

; A call to replace might look like: 
;     (replace square (list '* + '+ *))
; This call would replace the times symbol with the adding built-in and the plus symbol with the multiplying built-in. 

; You can use the replace function to speed up the execution of any function. 

; Your replace function needs to work recursively. That is to say, it needs to replace the symbol in all local function definitions, including lambdas. 
; You may find the predicate functions object? useful this feature as you should not descend into these kinds of objects. 
; Neither should you process any quoted expression. You may assume the given function performs no assignments of any kind. 
; (define (chunk-iterator code-tree)
;     (inspect code-tree)
; )


(define  (replace tree sym rep)
    (cond
        ((atom? tree) 
            ; (println "atom--" tree)
        )
        ((pair? tree) 
            (cond
                ((object? tree))
                ((quote? tree))
                (else 
                    ; (println "tree--" tree)
                    (cond
                        ((eq? (car tree) sym) 
                            ; (println "replacing " sym " with " rep)
                            (set-car! tree rep) 
                        )

                    )
                    (replace (car tree) sym rep)
                    (replace (cdr tree) sym rep)
                )
            )
        )
        (else
            ; (println tree)
            ;(stepper (car tree))
            ;(stepper (cdr tree))
        )
    )
)

(define (displaypair t)

    (println "car " (car t))
    (println "cdr " (cdr t))
)


; make sure you dont skip over the word quoted so we know when to not replace quoted strings
(define (main)

    (define (square x) (* x x) )
    (define body (get 'code square))
    (replace body '* '+)
    (println (square 5))


)

