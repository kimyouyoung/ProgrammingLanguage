#lang plai

;{+{-{+ 5 5}3}{-{+ 5 5}3}}
;{with{x {+ 5 5}}{with{y {- x 3}}{+ y y}}}

;Define WAE type
;<WAE> ::= <num>                            
;	| {+ <WAE><WAE>}
;	| {- <WAE><WAE>}
;	| {with {<id><WAE>} <WAE>}
;	| <id>

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)(rhs WAE?)]
  [sub (lhs WAE?)(rhs WAE?)]
  [with (name symbol?)(named-expr WAE?)(body WAE?)]
  [id (name symbol?)])

;[contract] parse: sexp -> WAE
;[purpose] to convert s-expression into WAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l)(parse r))]
    [(list '- l r) (sub (parse l)(parse r))]
    [(list 'with (list i v) e) (with i (parse v)(parse e))]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax:~a"sexp)]))

(test(parse '{+{- 3 4} 7})(add(sub (num 3)(num 4))(num 7)))
(test(parse '{with{x 5}{+ 8 2}})(with 'x(num 5)(add(num 8)(num 2))))
(test(parse '{with{x 5}{+ x x}})(with 'x(num 5)(add(id 'x)(id 'x))))

(parse '{with {x {+ 5 5}}{+ x x}})

;(parse '{with {x 5}{+ x {with {y 3} x}}})