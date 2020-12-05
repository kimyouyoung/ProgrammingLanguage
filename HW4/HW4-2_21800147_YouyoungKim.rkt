#lang plai
; Problem1: 
; Solved by myself: 
; Time taken: about mins
; [contract] parse: string -> SDFAE
; [purpose] to convert string(sub-expression) to SDFAE

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
  [boxV (address integer?)]
  [exprV (expr BFAE?) (ds DefrdSub?) (st Store?) (value (box/c (or/c false BFAE-Value?)))])

; strict: Value*Store -> Value*Store
; purpose: to interpret exprV expression to get a value in strictness points.
(define (strict vs)
    (type-case Value*Store vs
        [v*s (v s)
             (type-case BFAE-Value v
               [exprV (expr ds st v-box)
                      (if (not (unbox v-box))
                          (local [(define v (strict (interp expr ds s)))
                                  (define val (v*s-value v))]
                            (begin (set-box! v-box val)
                                   v))
                          (v*s (unbox v-box) s))] 
               [else vs])]))
                         

(define-type Value*Store
  [v*s (value BFAE-Value?)(store Store?)])

; lookup: symbol DefrdSub -> address
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub() (error 'lookup "free identifier")]
    [aSub (i adr saved) (if (symbol=? i name)
                            adr
                            (lookup name saved))]))

; store-lookup address Store -> BFAE-Value
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto() (error 'store-lookup "No value at address")]
    [aSto (location value rest-store)
                   (if (= location address)
                       (v*s-value (strict (v*s value sto)))
                       (store-lookup address rest-store))]))

; max-address: Store -> Integer
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

; malloc: Store -> Integer
(define (malloc st)
  (+ 1 (max-address st)))

; parse: sexp -> BFAE
; purpose: to convert sexp to BFAE
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

; interp-two: BFAE BFAE DefrdSub Store
;             (Value Value Store -> Value*Store)
;             -> Value*Store
(define (interp-two expr1 expr2 ds st handle)
  (type-case Value*Store (strict (interp expr1 ds st))
    [v*s (val1 st2)
         [type-case Value*Store (strict (interp expr2 ds st2))
           [v*s (val2 st3)
                (handle val1 val2 st3)]]]))

; interp: BFAE DefrdSub Store -> Value*Store
(define (interp bfae ds st)
  (type-case BFAE bfae
    [num (n) (v*s (numV n) st)]
    [add (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num+ v1 v2) st1)))]
    [sub (l r) (interp-two l r ds st (lambda (v1 v2 st1) (v*s (num- v1 v2) st1)))]
    [id  (s) (v*s (store-lookup (lookup s ds) st) st)]
    [fun (p b) (v*s (closureV p b ds) st)]
    [app (f a) (type-case BFAE a
                 [newbox (val)
                         (type-case Value*Store (interp f ds st)
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
                 [else (type-case Value*Store (strict (interp f ds st))
                         [v*s (f-value f-store)
                              (local [(define new-address (malloc f-store))
                                      (define a-value (exprV a ds f-store (box #f)))]
                                (interp (closureV-body f-value)
                                        (aSub (closureV-param f-value)
                                              new-address
                                              (closureV-ds f-value))
                                        (aSto new-address
                                              a-value
                                              f-store)))])])]  
    [newbox (val)
                    (type-case Value*Store (interp val ds st)
                      [v*s (v1 st1)
                                     (local [(define a (malloc st1))
                                             (define a-val (exprV val ds st1 (box #f)))]
                                            (v*s (boxV a)
                                            (aSto a a-val st1)))])]
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

(test (run '{{fun {x} {+ 1 1}} {with {b {newbox 7}} {seqn {setbox b 10} {openbox b}}}} (mtSub) (mtSto)) (v*s (numV 2) (aSto 1 (exprV (app (fun 'b (seqn (setbox (id 'b) (num 10)) (openbox (id 'b)))) (newbox (num 7)))(mtSub) (mtSto) '#&#f) (mtSto))))

(test (run '{{fun {x} {+ 1 x}} 10} (mtSub) (mtSto)) (v*s (numV 11) (aSto 1 (exprV (num 10) (mtSub) (mtSto) (box (numV 10))) (mtSto))))

(test (run '7 (mtSub) (mtSto)) (v*s (numV 7) (mtSto)))

(test (run '{+ 7 6} (mtSub) (mtSto)) (v*s (numV 13) (mtSto)))

(test (run '{newbox 1} (mtSub) (mtSto)) (v*s (boxV 1) (aSto 1 (exprV (num 1) (mtSub) (mtSto) '#&#f) (mtSto))))

(test (run '{with {b {newbox {+ 2 3}}} {openbox b}} (mtSub) (mtSto)) (v*s (numV 5) (aSto 2 (boxV 1) (aSto 1
(exprV (add (num 2) (num 3)) (mtSub) (mtSto) (box (numV 5))) (mtSto)))))

(test (run '{with {b {newbox 7}} {openbox b}} (mtSub) (mtSto)) (v*s (numV 7) (aSto 2 (boxV 1) (aSto 1 (exprV (num 7) (mtSub)
(mtSto) (box (numV 7))) (mtSto)))))

(test (run '{with {b {newbox 7}} {seqn {setbox b 10}
{openbox b}}} (mtSub) (mtSto)) (v*s (numV 10) (aSto 1 (numV 10) (aSto 2 (boxV 1)
(aSto 1 (exprV (num 7) (mtSub) (mtSto) '#&#f) (mtSto))))))

(test (run '{with {b {newbox 7}} {seqn {openbox b} {openbox b}}} (mtSub) (mtSto)) (v*s (numV 7) (aSto 2 (boxV 1)
(aSto 1 (exprV (num 7) (mtSub) (mtSto) (box (numV 7)))(mtSto)))))

(test (run '{+ {with {b {newbox 10}} {seqn {setbox b 7} {openbox b}}} {with {b {newbox 10}} {seqn {setbox b 5} {openbox b}}}} (mtSub) 
(mtSto)) (v*s (numV 12) (aSto 3 (numV 5) (aSto 4 (boxV 3) (aSto 3 (exprV (num 10) (mtSub) (aSto 1 (numV 7) (aSto 2 (boxV 1)
(aSto 1 (exprV (num 10) (mtSub) (mtSto) '#&#f) (mtSto)))) '#&#f) (aSto 1 (numV 7) (aSto 2 (boxV 1) (aSto 1 (exprV (num
10) (mtSub) (mtSto) '#&#f) (mtSto)))))))))