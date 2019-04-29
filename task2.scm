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

(define (debug @)
    (define db? #t)
    (if db? (println @))
)


(define  (replace fun sym-list)

    (define (in-sym-list? sym)
        (println "checking sym-list for " sym)

        (define (iter n)
            (cond
                ((null? n) #f) ;end of sym list
                ((null? (cdr n)) #f)
                ((eq? (car n) sym)
                    #t
                )
                (else 
                    (iter (cddr n))
                )
            )
        )
        (iter sym-list)
    )

    (define (find-replacement sym)
        (println "replacing " sym)
        ; n is the cons cell of the replacement list. its car will be the symbol to be replaced, its cadr will be what to replace it with
        (define (iter n)
            (cond
                ((null? n) nil) ; end of sym list
                ((eq? (car n) sym) ; 
                    (cadr n)
                )
                (else 
                    (iter (cddr n)) ;to skip the replacement value we do cddr
                )
            )
        )
        (iter sym-list)
    
    )

    (define (replace-iter node)
        (debug "calling replace-iter")
        (cond
            ((null? node))
            ((object? node))
            ((eq? (car node) 'quote) 
                (debug "quote") 
                (cond
                    ((in-sym-list? (car node)) set-car! (find-replacement (car node)))
                )
            )
            ((pair? node)
                (debug "pair")
                (debug (eq? (type  node) 'CONS))
                ; (println "tree--" tree)
                (replace-iter (car node))
                (replace-iter (cdr node))
            )
            (else
                (debug "otherwise " node)
                ((in-sym-list? (car node))
                    (set-car! (find-replacement (car node))) 
                )
                (replace-iter (cdr node))
            )
        )
    )
    (replace-iter (cadr (get 'code fun)))
)

(define (displaypair t)

    (println "car " (car t))
    (println "cdr " (cdr t))
)

















(define (replace f items)

    (define (in-list? n l)
        (cond 
            ((null? (cddr sym-list))
                (if (eq? (car sym-list) temp)
                    (cadr sym-list)
                    temp
                )
                (if (eq? (car sym-list) temp)
                    (cadr sym-list)
                    (check temp (cddr sym-list))
                )
            )
        )
    )

    (define (check temp sym-list)
        (if (null? (cddr sym-list))
            (if (eq? (car sym-list) temp)
                (cadr sym-list)
                temp
            )
            (if (eq? (car sym-list) temp)
                (cadr sym-list)
                (check temp (cddr sym-list))
            )
        )
    )

    (define (iter node)
        (cond
            ((null? node) node)
            ((object? (car node)) node)
            ((eq? (car node) 'quote) 
                (set-car! node (check (car node) items))
            )
            ((eq? (type (car node)) 'CONS)
                (iter (car node))
                (iter (cdr node))
            )
            (else
                (set-car! node (check (car node) items))
                (iter (cdr node))
                )
            )
        )

    (iter (get 'parameters f))
    (iter (cadr (get 'code f)))
    )

; make sure you dont skip over the word quoted so we know when to not replace quoted strings
(define (main)

    (define (abs n) (if (< n 0) (- n) n))
    (replace abs (list '< > '- +))
    (inspect (get 'code abs))

)

