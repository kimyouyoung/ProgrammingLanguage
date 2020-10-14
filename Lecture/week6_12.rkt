#lang plai

; concrete syntax
;<FAE> ::= <num>                            
;	| {+ <FAE><FAE>}
;	| {- <FAE><FAE>}
;	| {with {<id><FAE>}<FAE>}
;	| <id>
;    	| {fun {<id><FAE>}
;    	| {<FAE><FAE>}

(define (num-op op)
  (lambda (x y)
    (num(op (num-n x)(num-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

; interp: FAE -> FAE
(define (interp fae)
  (type-case FAE fae
    [num (n) fae]
    [add (l r) (num+ (interp l)(interp r))]
    [sub (l r) (num- (interp l)(interp r))]
    [id (s) (error 'interp "free identifier")]
    [fun (p b) fae]
    [app (f a) (local [(define ftn (interp f))]
                 (interp (subst(fun-body ftn)
                               (fun-param ftn)
                               (interp a))))]))