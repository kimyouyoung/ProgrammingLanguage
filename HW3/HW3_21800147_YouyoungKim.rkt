#lang plai
;<SDFAE> ::= <num>
;	|  <id>
;	|  {+ <SDFAE> <SDFAE>}
;       |  {- <SDFAE> <SDFAE>}
;       |  {with {<id> <SDFAE>} <SDFAE>}
;       |  {<SDFAE> <SDFAE>}
;       |  {s fun {<id>} <SDFAE>}
;       |  {d fun {<id>} <SDFAE>}

(define-type SDFAE
    [num     (n number?)]
    [add     (lhs SDFAE?) (rhs SDFAE?)]
    [sub     (lhs SDFAE?) (rhs SDFAE?)]
    [id      (name symbol?)]
    [fun     (sd symbol?) (param symbol?) (body SDFAE?)]
    [app     (ftn SDFAE?) (arg SDFAE?)])

; Problem1: 
; Solved by myself: Y
; Time taken: about 20mins
; [contract] parse: string -> SDFAE
; [purpose] to convert string(sub-expression) to SDFAE

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l)(parse r))]
    [(list '- l r) (sub (parse l)(parse r))]
    [(list 'with (list i v) e) (app(fun 's i (parse e)) (parse v))]
    [(? symbol?) (id sexp)]
    [(list 'd 'fun (list p) b) (fun 'd p (parse b))]
    [(list 's 'fun (list p) b) (fun 's p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax:~a"sexp)]))


; Problem2: 
; Solved by myself: Y
; Time taken: about 40mins
; [contract] interp : SDFAE ds → result
; [purpose] interpret SDFAE to produce results

(define (num-op op)
  (lambda (x y)
    (numV (op (numV-n x)(numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

(define-type SDFAE-Value
  [numV (n number?)]
  [closureV (sd symbol?) (param symbol?) (body SDFAE?) (ds DefrdSub?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value SDFAE-Value?) (ds DefrdSub?)])

; lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if(symbol=? i name)
                              v
                              (lookup name saved))]))

(define (interp sdfae ds)
  (type-case SDFAE sdfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds)(interp r ds))]
    [sub (l r) (num- (interp l ds)(interp r ds))]
    [id (s) (lookup s ds)]
    [fun (sd p b) (closureV sd p b ds)]
    [app (f a) (if (symbol=? (closureV-sd (interp f ds)) 'd)
                             (local ([define ftn (interp f ds)])
                                    (interp (closureV-body ftn)
                                            (aSub (closureV-param ftn)
                                                  (interp a ds)
                                                  ds)))
                             (local [(define f-val (interp f ds))
                                     (define a-val (interp a ds))]
                               (interp (closureV-body f-val)
                                       (aSub(closureV-param f-val)
                                            a-val
                                            (closureV-ds f-val)))))]))