#lang plai
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

(define-type RBMFAE
  [num (n number?)]
  [add (lhs RBMFAE?)(rhs RBMFAE?)]
  [sub (lhs RBMFAE?)(rhs RBMFAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body RBMFAE?)]
  [refun (param symbol?)(body RBMFAE?)]
  [newbox (v RBMFAE?)]
  [setbox (bn RBMFAE?)(v RBMFAE?)]
  [openbox (v RBMFAE?)]
  [seqn (ex1 RBMFAE?)(ex2 RBMFAE?)]
  [app (ftn RBMFAE?)(arg RBMFAE?)]
  [setvar (id symbol?)(body RBMFAE?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)(address integer?)(ds DefrdSub?)])

(define-type Store
  [mtSto]
  [aSto (address integer?)(value RBMFAE-Value?)(rest Store?)])

(define-type RBMFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)(body RBMFAE?)(ds DefrdSub?)]
  [refclosV (param symbol?)(body RBMFAE?)(ds DefrdSub?)]
  [boxV (address integer?)])

(define-type Value*Store
  [v*s (value RBMFAE-Value?)(store Store?)])

; lookup: symbol DefrdSub -> address
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub() (error 'lookup "free identifier")]
    [aSub (i adr saved) (if (symbol=? i name)
                            adr
                            (lookup name saved))]))

; store-lookup address Store -> RBMFAE-Value
(define (store-lookup address sto)
  (type-case Store sto
    [mtSto() (error 'store-lookup "No value at address")]
    [aSto (location value rest-store)
                   (if (= location address)
                      value
                      (store-lookup address rest-store))]))

(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

; max-address: Store -> Integer
(define (max-address st)
  (type-case Store st
    [mtSto () 0]
    [aSto (n v st)
          (max n (max-address st))]))

; malloc: Store -> Integer
(define (malloc st)
  (+ 1 (max-address st)))

(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)     (fun p (parse b))]
        [(list 'refun (list p) b)   (refun p (parse b))]
        [(list 'newbox v)           (newbox (parse v))]
        [(list 'setbox bn v)        (setbox (parse bn) (parse v))]
        [(list 'openbox v)          (openbox (parse v))]
        [(list 'seqn ex1 ex2)       (seqn (parse ex1) (parse ex2))]
        [(list 'setvar id b)        (setvar id (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))

; interp: RBMFAE DefrdSub Store -> Value*Store
(define (interp rbmfae ds st)
  (type-case RBMFAE rbmfae
    [num (n) (v*s (numV n) st)]
    [add (l r) (type-case Value*Store (interp l ds st)
                 [v*s (l-value l-store)
                      (type-case Value*Store (interp r ds l-store)
                        [v*s (r-value r-store)
                             (v*s (num+ l-value r-value)
                                  r-store)])])]
    [sub (l r) (type-case Value*Store (interp l ds st)
                 [v*s (l-value l-store)
                      (type-case Value*Store (interp r ds l-store)
                        [v*s (r-value r-store)
                             (v*s (num- l-value r-value)
                                  r-store)])])]
    [id (s) (v*s (store-lookup (lookup s ds) st) st)]
    [fun (p b) (v*s (closureV p b ds) st)]
    [refun (p b) (v*s (refclosV p b ds) st)]
    [app (f a) (type-case Value*Store (interp f ds st)
                 [v*s (f-value f-store)
                      (type-case RBMFAE-Value f-value
                        [closureV (c-param c-body c-ds)
                                  (type-case Value*Store (interp a ds f-store)
                                    [v*s (a-value a-store)
                                         (local ([define new-address (malloc a-store)])
                                           (interp (closureV-body f-value)
                                                   (aSub (closureV-param f-value)
                                                         new-address
                                                         (closureV-ds f-value))
                                                   (aSto new-address
                                                         a-value
                                                         a-store)))])]
                        [refclosV (rc-param rc-body rc-ds)
                                  (local ([define address (lookup (id-name a) ds)])
                                    (interp rc-body
                                            (aSub rc-param
                                                  address
                                                  rc-ds)
                                            f-store))]
                        [else (error interp "trying to apply a number")])])]
    [newbox (val)
            (type-case Value*Store (interp val ds st)
              [v*s (v1 st1)
                   (local [(define a (malloc st1))]
                     (v*s (boxV a)
                          (aSto a v1 st1)))])]
    [setbox (bx-expr val-expr)
            (type-case Value*Store (interp bx-expr ds st)
              [v*s (bx-val st2)
                   (type-case Value*Store (interp val-expr ds st2)
                     [v*s (val st3)
                          (v*s val
                               (aSto (boxV-address bx-val)
                                     val
                                     st3))])])]
    [openbox (bx-expr)
             (type-case Value*Store (interp bx-expr ds st)
               [v*s (bx-val st1)
                    (v*s (store-lookup (boxV-address bx-val)
                                       st1)
                         st1)])]
    [seqn (a b)
          (type-case Value*Store (interp a ds st)
              [v*s (a-val st2)
                   (type-case Value*Store (interp b ds st2)
                     [v*s (b-val st3)
                          (v*s b-val st3)])])]
    
    [setvar (id val-expr) (local [(define a (lookup id ds))]
                           (type-case Value*Store (interp val-expr ds st)
                             [v*s (val st)
                                  (v*s val
                                       (aSto a val st))]))]))
    

(define (run sexp ds st)
  (interp (parse sexp) ds st))

(test (run '{with {swap {refun {x} {refun {y} {with {z x} {seqn {setvar x y} {setvar y z}}}}}} {with {a 10} {with {b 20} {seqn {{swap a} b} b}}}} (mtSub) (mtSto)) (v*s (numV 10) (aSto 3 (numV 10) (aSto 2 (numV 20) (aSto 4 (numV 10) (aSto 3 (numV 20) (aSto 2 (numV 10) (aSto 1 (refclosV 'x (refun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))) (mtSub)) (mtSto)))))))))

(test (run '{with {swap {refun {x} {refun {y} {with {z x} {seqn {setvar x y} {setvar y z}}}}}} {with {a 10} {with {b 20} {seqn {{swap a} b} a}}}} (mtSub) (mtSto)) (v*s (numV 20) (aSto 3 (numV 10) (aSto 2 (numV 20) (aSto 4 (numV 10) (aSto 3 (numV 20) (aSto 2 (numV 10) (aSto 1 (refclosV 'x (refun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))) (mtSub)) (mtSto)))))))))

(parse '{with {swap {refun {x}
{refun {y} {with {z x}
{seqn {setvar x y} {setvar y z}}}}}}
{with {a 10} {with {b 20}
{seqn {{swap a} b} a}}}})

(test (app (fun 'swap (app (fun 'a (app (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a))) (num 20))) (num 10))) (refun 'x (refun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x)))))
(app (fun 'swap (app (fun 'a (app (fun 'b (seqn (app (app (id 'swap) (id 'a)) (id 'b)) (id 'a))) (num 20))) (num 10))) (refun 'x (refun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))))))

(test (v*s (numV 20) (aSto 3 (numV 10) (aSto 2 (numV 20) (aSto 4 (numV 10) (aSto 3 (numV 20) (aSto 2 (numV 10) (aSto 1 (refclosV 'x (refun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))) (mtSub)) (mtSto))))))))
(v*s (numV 20) (aSto 3 (numV 10) (aSto 2 (numV 20) (aSto 4 (numV 10) (aSto 3 (numV 20) (aSto 2 (numV 10) (aSto 1 (refclosV 'x (refun 'y (app (fun 'z (seqn (setvar 'x (id 'y)) (setvar 'y (id 'z)))) (id 'x))) (mtSub)) (mtSto)))))))))