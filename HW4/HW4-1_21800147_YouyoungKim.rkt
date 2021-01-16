#lang plai
; Problem1: 
; Solved by myself: Y
; Time taken: about 20 mins

;<BFAE> ::= <num>
;         | {+ <BFAE> <BFAE>}
;         | {- <BFAE> <BFAE>}
;         | <id>
;         | {fun {<id} <BFAE>}
;         | {<BFAE> <BFAE>}
;         | {newbox <BFAE>}
;         | {setbox <BFAE> <BFAE>}
;         | {openbox <BFAE>}
;         | {seqn <BFAE> <BFAE>}

(define-type BFAE
  [num (n number?)]
  [add (lhs BFAE?)(rhs BFAE?)]
  [sub (lhs BFAE?)(rhs BFAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body BFAE?)]
  [newbox (v BFAE?)]
  [setbox (bn BFAE?)(v BFAE?)]
  [openbox (v BFAE?)]
  [seqn (ex1 BFAE?)(ex2 BFAE?)]
  [app (ftn BFAE?)(arg BFAE?)])

; num-op: operators for artithmatic computation -> function for artithmatic computation
; purpose: to get a function for arithmatic computation.
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(address integer?)(ds DefrdSub?)])

(define-type Store
  [mtSto]
  [aSto (address integer?)(value BFAE-Value?)(rest Store?)])

(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)(body BFAE?)(ds DefrdSub?)]
  [boxV (address integer?)])

(define-type Value*Store
  [v*s (value BFAE-Value?)(store Store?)])

; lookup: symbol DefrdSub -> address
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub() (error 'lookup "free identifier")]
    [aSub (i adr saved) (if (symbol=? i name)
                            adr
                            (lookup name saved))]))

; store-lookup
; [contract] address Store -> BFAE-Value
; [purpose] get the BFAE-value corresponding to the address.
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto() (error 'store-lookup "No value at address")]
    [aSto (location value rest-store)
                   (if (= location address)
                      value
                      (store-lookup address rest-store))]))

; max-address
; [contract] Store -> Integer
; [purpose] add 1 to the max address value.
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

; malloc
; [contract] Store -> Integer
; [purpose] allocate the address value.
(define (malloc st)
  (+ 1 (max-address st)))

; interp-two
; [contract] BFAE BFAE DefrdSub Store
;             (Value Value Store -> Value*Store)
;             -> Value*Store
; [purpose] it can be calculated simply without repeating the same patterns.
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (interp expr1 ds st)
    [v*s (val1 st2)
         [type-case Value*Store (interp expr2 ds st2)
           [v*s (val2 st3)
                (handle val1 val2 st3)]]]))

; parse
; [contract] sexp -> BFAE
; [purpose] to convert sexp to BFAE
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)     (fun p (parse b))]
        [(list 'newbox v)           (newbox (parse v))]
        [(list 'setbox bn v)        (setbox (parse bn) (parse v))]
        [(list 'openbox v)          (openbox (parse v))]
        [(list 'seqn ex1 ex2)       (seqn (parse ex1) (parse ex2))]
        [(list f a)                 (app (parse f) (parse a))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))

; interp
; [contract] BFAE DefrdSub Store -> Value*Store
; [purpose] to get BFAE-Value and Store from BFAE
(define (interp bfae ds st)
  (type-case BFAE bfae
    [num (n) (v*s (numV n) st)]
    [add (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
    [id (s) (v*s (store-lookup (lookup s ds) st) st)]
    [fun (p b) (v*s (closureV p b ds) st)]
    [app (f a) (type-case Value*Store (interp f ds st)
                 [v*s (f-value f-store)
                      (type-case Value*Store (interp a ds f-store)
                        [v*s (a-value a-store)
                             (local [(define new-address (malloc a-store))]
                               (interp (closureV-body f-value)
                                       (aSub (closureV-param f-value)
                                             new-address
                                             (closureV-ds f-value))
                                       (aSto new-address
                                             a-value
                                             a-store)))])])]
    [newbox (val)
                    (type-case Value*Store (interp val ds st)
                      [v*s (v1 st1)
                                     (local [(define a (malloc st1))]
                                            (v*s (boxV a)
                                            (aSto a v1 st1)))])]
    [setbox (bx-expr val-expr)
                   (interp-two bx-expr val-expr ds st
                               (lambda (bx-val val st1)
                                       (v*s val
                                       (aSto (boxV-address bx-val)
                                             val
                                             st1))))]
    [openbox (bx-expr)
                   (type-case Value*Store (interp bx-expr ds st)
                     [v*s (bx-val st1)
                                       (v*s (store-lookup (boxV-address bx-val)
                                                            st1)
                                             st1)])]
    [seqn (a b) (interp-two a b ds st (lambda (v1 v2 st1) (v*s v2 st1)))]))

(define (run sexp ds st)
  (interp (parse sexp) ds st))
