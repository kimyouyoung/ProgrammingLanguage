#lang plai

; abstract syntax
(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?)(rhs FWAE?)]
  [sub (lhs FWAE?)(rhs FWAE?)]
  [with (name symbol?)(named-expr FWAE?)(body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body FWAE?)]
  [app (ftn FWAE?)(arg FWAE?)])

(fun 'x(add (id 'x)(id 'x)))
; (app (id 'twice)(num 10))
(app(fun 'x(add (id 'x)(id 'x)))(num 10))

; parse: sexp -> FWAE
; purpose: to convert sexp to FWAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l)(parse r))]
    [(list '- l r) (sub (parse l)(parse r))]
    [(list 'with (list i v) e) (with i (parse v)(parse e))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]  ; e.g., {fun {x}{+ x 1}}
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax:~a"sexp)]))

(parse '{with{f {fun {x}{+ x x}}}{- 20{f 10}}})

; interp: FWAE -> FWAE
(define (interp fwae)
  (type-case FWAE fwae
    [num (n) fwae]
    [add (l r) (num+ (interp l)(interp r))]
    [sub (l r) (num- (interp l)(interp r))]
    [with (i v e) (interp (subst e i (interp v)))]
    [id (s) (error 'interp "free identifier")]
    [fun (p b) fwae]  ; return a function itself as it is a valid value in FWAE
    [app (f a) (local [(define ftn (interp f))]
                  (interp (subst (fun-body ftn)
                                 (fun-param ftn)
                                 (interp a))))]))
;(interp (num 9)) ; 9 ==> F1WAE
; in FWAE number or function definition => FWAE
(interp (num 9)) ; ==> (num 9)

; interp
; (add (num 6)(num 5)) == f1wae interp -> 11
; (add (num 6)(num 5)) == fwae interp -> (num 11)

; (num-n (num 6))

; num+: FWAE FWAE -> FWAE
;(define (num+ x y)
;  (num (+ (num-n x)(num-n y))))
; num-: FWAE FWAE -> FWAE
;(define (num- x y)
;  (num (- (num-n x)(num-n y))))

; num-op: (number number -> number) -> (FWAE FWAE -> FWAE)  (num 3)(num5)
(define (num-op op)
  (lambda (x y)
    (num(op (num-n x)(num-n y)))))

((num-op +) (num 3)(num 5))
((num-op -) (num 3)(num 5))

(define num+ (num-op +))
(define num- (num-op -))

(num+ (num 3)(num 11))
(num- (num 3)(num 11))

; subst: FWAE symbol FWAE -> FWAE
(define (subst exp idtf val)
  (type-case FWAE exp
    [num (n) exp]
    [add (l r) (add (subst l idtf val)(subst r idtf val))]
    [sub (l r) (sub (subst l idtf val)(subst r idtf val))]
    [with (i v e) (with i (subst v idtf val)(if (symbol=? i idtf) e
                                 (subst e idtf val)))]
    [id (name) (cond[(equal? name idtf) val]
                    [else exp])]
    [app (f arg) (app (subst f idtf val)
                      (subst arg idtf val))]
    [fun (id body) (if (equal? idtf id)
                       exp
                       (fun id (subst body idtf val)))]))

; tests
(test (interp (with 'x (num 5) (add (id 'x)(id 'x)))) (num 10))
(test (interp (with 'x (num 5) (add (num 1)(with 'y (id 'x)(id 'y))))) (num 6))
(test (interp (parse '{fun {a} {+ a a}})) (fun 'a (add (id 'a)(id 'a))))
;{with {x 1} {fn {with {y 10} {+ y x}}}}
; F1WAE test case
;(test (interp (parse '{with {x 1} {fn {with {y 10} {+ y x}}}}) (list (parse-fd '{deffun {fn a} {+ a a}}))) 22)
; {fun {a} {+ a a}} = value part
(test (interp (parse '{with {fn {fun {a} {+ a a}}} {with {x 1} {fn {with {y 10} {+ y x}}}}})) (num 22))

;dynamic scope issue
(interp (parse '{with {y 3} {with {z {fun {x} {+ x y}}} {with {y 10} z}}}))
(interp (parse '{with {z {fun {x} {+ x y}}} {with {y 10} z}}))  ;; work as dynamic scope but it must be an error, free identifier as we adopt static scope.
(interp (parse '{with {y 3} {with {z {fun {x} {+ x y}}} {with {y 10} {z 5}}}}))
(interp (parse '{with {z {fun {x} {+ x y}}} {with {y 10} {z 5}}}))

(parse '{with {x 3} {fun {x}{+ x y}}})
(parse '{with {x 3} {fun {y}{+ x y}}})
(interp (parse '{with {x 3} {fun {y}{+ x y}}}))
(interp (parse '{fun {x}{+ x y}}))
(interp (with 'x (num 3) (fun 'x (add (id 'x) (id 'y)))))