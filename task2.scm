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
    (if db? (apply println @))
)



(define (replace f replace-list)

    (define (in-list? n sym-list)
        (cond
            ((null? sym-list) #f)
            ((eq? (car sym-list) n)  #t)
            ((null? (cddr sym-list)) #f)
            (else 
                (in-list? n (cddr sym-list))
            )
        )
    )

    (define (get-from-list n sym-list)
        (cond
            ((null? sym-list) (println "ERROR--reached end of list without finding replacement"))
            ((eq? (car sym-list) n)  (cadr sym-list)) ;return what needs to be replaced with
            (else 
                (get-from-list n (cddr sym-list))
            )
        )
    )

    (define (replace-iter node)
        (cond
            ((null? node) node) ; null? stop doing stuff
            ((object? (car node)) node) ; dont mess with objects
            ((eq? (car node) 'quote)  ; dont recur cdr
                (if (in-list? (car node) replace-list)   
                    (set-car! node (get-from-list (car node) replace-list))
                )
            )
            ((pair? (car node))
                (replace-iter (car node))
                (replace-iter (cdr node))
            )
            (else
                (if (in-list? (car node) replace-list)   
                    (set-car! node (get-from-list (car node) replace-list))
                )
                (replace-iter (cdr node))
            )
        )
    )

    (replace-iter (get 'parameters f))
    (replace-iter (cadr (get 'code f)))
)

; make sure you dont skip over the word quoted so we know when to not replace quoted strings
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

