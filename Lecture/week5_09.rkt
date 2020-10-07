#lang plai
; Change
; interp: WAE -> number
; to
; interp: WAE DefrdSub -> number

(define-type DefrdSub
  [mtSub]   ; empty cache
  [aSub (name symbol?)
        (value number?)
        (saved DefrdSub?)])  ; cache

(aSub 'x 1 (aSub 'y 4(aSub 'x 2(mtSub))))

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?)(rhs WAE?)]
  [sub (lhs WAE?)(rhs WAE?)]
  [with (name symbol?)(named-expr WAE?)(body WAE?)]
  [id (name symbol?)])

; interp: WAE DefrdSub -> number
(define (interp wae ds)
  (type-case WAE wae
    [num (n) n]
    [add (l r) (+ (interp l)(interp r))]
    [sub (l r) (- (interp l)(interp r))]
    [with (i v e) (interp e (aSub i (interp v ds) ds))]
    [id (s) (lookup s ds)]))

; lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if(symbol=? i name)
                              v
                              (lookup name saved))]))

(test(lookup 'x(aSub 'x 1 (mtSub))) 1)
(test(lookup 'y(aSub 'x 1 (aSub 'y 4 (mtSub)))) 4)

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])
; abstract syntax
(define-type F1WAE
  [f1-num (n number?)]
  [f1-add (lhs F1WAE?)(rhs F1WAE?)]
  [f1-sub (lhs F1WAE?)(rhs F1WAE?)]
  [f1-with (name symbol?)(named-expr F1WAE?)(body F1WAE?)]
  [f1-id (name symbol?)]
  [f1-app (ftn symbol?)(arg F1WAE?)])

; lookup-fundef: synbol list-of-FunDef -> FunDef
(define (lookup-fundef name fundefs)
  (cond
    [(empty? fundefs)
     (error 'lookup-fundef "unknown function")]
    [else
     (if (symbol=? name(fundef-fun-name (first fundefs)))
            (first fundefs)
            (lookup-fundef name(rest fundefs)))]))

; interp: F1WAE list-of-FuncDef DefrdSub -> number
(define (f1-interp f1wae fundefs ds)
  (type-case F1WAE f1wae
    [f1-num (n) n]
    [f1-add (l r) (+ (f1-interp l fundefs ds)(f1-interp r fundefs ds))]
    [f1-sub (l r) (- (f1-interp l fundefs ds)(f1-interp r fundefs ds))]
    [f1-with (i v e) (f1-interp e fundefs(aSub i (f1-interp v fundefs ds) ds))]
    [f1-id (s) (lookup s ds)]
    [f1-app (f a)
              (local
                [(define a-fundef(lookup-fundef f fundefs))]
                (f1-interp (fundef-body a-fundef)
                         fundefs
                        (aSub (fundef-arg-name a-fundef)
                              (f1-interp a fundefs ds)
                              (mtSub))
                 ))]))

; parse: sexp -> F1WAE
(define (parse sexp)
  (match sexp
    [(? number?) (f1-num sexp)]
    [(list '+ l r) (f1-add (parse l)(parse r))]
    [(list '- l r) (f1-sub (parse l)(parse r))]
    [(list 'with (list i v) e) (f1-with i (parse v)(parse e))]
    [(? symbol?) (f1-id sexp)]
    [(list f a) (f1-app f (parse a))]
    [else (error 'parse "bad syntax:~a"sexp)]))

; F1WAE parser
; parse-fd: sexp -> FunDef
(define (parse-fd sexp)
  (match sexp
    [(list 'deffun (list f x) b)(fundef f x (parse b))]))

(test(f1-interp(parse '{f 1})(list(parse-fd '{deffun (f x){+ x 3}}))(mtSub)) 4)