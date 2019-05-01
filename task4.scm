; Parker Jones
; Task 4
; NAND GATE LOGIC

; NAND is considered a universal set as any logical expression can be built solely with NAND gates. 
; Define the constructors inverter, and-gate, or-gate, nor-gate, xor-gate, and xnor-gate, built entirely from NAND gates. 
; Your gates need to be minimal. Name your NAND gate constructor nand-gate. Follow the style of the book in implemting your gate. 
; Assume a nand-gate delay of 6 time units and hard-wire this number into your nand-gate function. 
; Use the logic gate system of the text to implement NAND and the gates built with NANDs.
;
; Use your gates to build a half-adder and a full-adder, as in the text. 
; Place your nand-gate function by itself (with no other functions) in a file named nand.scm and include that file in your task file: 
; You will also need to make the agenda in your task file: 

(include "gates.scm")
(include "queue.scm")
(include "nand.scm")
(define the-agenda (make-agenda))


(define (inverter a b)
    (let ()
        (nand-gate a a b)
        'ok
    )
)

(define (or-gate a b c)
    (let (
            (in-a (make-wire))
            (in-b (make-wire))
        )
        (inverter a in-a)
        (inverter b in-b)
        (nand-gate in-a in-b c)
        'ok
    )
)

(define (and-gate a b c)
    (let (
            (in-a (make-wire))
        )
        (nand-gate a b in-a)
        (inverter in-a c)

        'ok
    )
)

(define (xor-gate a b c)
    (let (
            (ab-not (make-wire))
            (a-int (make-wire))
            (b-int (make-wire))
        )
        (nand-gate a b ab-not)
        (nand-gate a ab-not a-int)
        (nand-gate b ab-not b-int)
        (nand-gate a-int b-int c)
        
        'ok
    ) 
)

(define (nor-gate a b c)
    (let (
            (a-l1 (make-wire))
            (b-l1 (make-wire))
            (ab-l2 (make-wire))
        )
        (inverter a a-l1)
        (inverter b b-l1)
        (nand-gate a-l1 b-l1 ab-l2)
        (inverter ab-l2 c)
        'ok
    )
)

; cant be a composite. too slow
(define (xnor-gate a b c) 
    (let (
            (a-not (make-wire))
            (b-not (make-wire))
            (ab-nand (make-wire))
            (ab-not-nand (make-wire))
        )
        (inverter a a-not)
        (inverter b b-not)
        (nand-gate a b ab-nand)
        (nand-gate a-not b-not ab-not-nand)
        (nand-gate ab-not-nand ab-nand c)
        'ok
    )
)

(define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
        'ok
    )
)

(define (full-adder a b c-in sum c-out)
    (let (
            (s (make-wire))
            (c1 (make-wire))
            (c2 (make-wire))
        )
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
        'ok
    )
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



