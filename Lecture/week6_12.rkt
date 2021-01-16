#lang plai

; concrete syntax
;<FAE> ::= <num>                            
;	| {+ <FAE><FAE>}
;	| {- <FAE><FAE>}
;	| {with{<id><FAE>} <FAE>}
;	| <id>
;    	| {fun {<id><FAE>}
;    	| {<FAE><FAE>}

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?)(rhs FAE?)]
  [sub (lhs FAE?)(rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body FAE?)]
  [app (ftn FAE?)(arg FAE?)])

(define (num-op op)
  (lambda (x y)
    (numV(op (numV-n x)(numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

; parse
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l)(parse r))]
    [(list '- l r) (sub (parse l)(parse r))]
    [(list 'with (list i v) e) (app(fun i (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]  
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax:~a"sexp)]))
(parse '{f {fun {y} {+ x y}}})
(parse  '{with {x 3} {+ x x}})
(parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})
(parse '{with {z {fun {x}{+ x y}}}{with {y 10} z}})

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DefrdSub?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

; lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if(symbol=? i name)
                              v
                              (lookup name saved))]))

; interp: FAE DefrdSub -> FAE-value
(define (interp fae ds)
  (type-case FAE fae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds)(interp r ds))]
    [sub (l r) (num- (interp l ds)(interp r ds))]
    [id (s) (lookup s ds)]
    [fun (p b) (closureV p b ds)]
    [app (f a) (local [(define f-val (interp f ds))
                       (define a-val (interp a ds))]
                 (interp (closureV-body f-val)
                         (aSub(closureV-param f-val)
                              a-val
                              (closureV-ds f-val))))]))

;(interp (parse '{with {y 3}{{fun {x}{+ x y}}{+ 23 1}}}) (mtSub))
(parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}})
(test (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (app (fun 'x (app (fun 'f (app (fun 'x (app (id 'f) (num 4))) (num 5))) (fun 'y (add (id 'x) (id 'y))))) (num 3)))
;(interp (parse '{with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}) (mtSub))
(interp (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub))