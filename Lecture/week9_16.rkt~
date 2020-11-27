#lang plai

; <RCFAE> ::= <num>
;            |{+ <RCFAE> <RCFAE>}
;            |{- <RCFAE> <RCFAE>}
;            |{* <RCFAE> <RCFAE>}
;            |<id>
;            |{fun {<id>} <RCFAE>}
;            |{<RCFAE> <RCFAE>}
;            |{if0 <RCFAE> <RCFAE> <RCFAE>}
;            |{rec {<id> <RCFAE>} <RCFAE>}

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?)(rhs RCFAE?)]
  [sub (lhs RCFAE?)(rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?)(body RCFAE?)]
  [app (fun-expr RCFAE?)(arg-expr RCFAE?)]
  [if0 (test-expr RCFAE?)
       (then-expr RCFAE?)(else-expr RCFAE?)]
  [rec (name symbol?)(named-expr RCFAE?)(fst-call RCFAE?)])

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)(body RCFAE?)(ds DefrdSub?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value RCFAE-Value?)
        (ds DefrdSub?)]
  [aRecSub (name symbol?)
           (value-box (box/c RCFAE-Value?))
           (ds DefrdSub?)])

; lookup: symbol DefrdSub -> RCFAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free variable")]
    [aSub (sub-name val rest-ds)
                (if(symbol=? sub-name name)
                    val
                    (lookup name rest-ds))]
    [aRecSub (sub-name val-box rest-ds)
             (if(symbol=? sub-name name)
                (unbox val-box)
                (lookup name rest-ds))]))

; num-op: operators for artithmatic computation -> function for artithmatic computation
; purpose: to get a function for arithmatic computation.
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

; interp: RCFAE DefrdSub -> RCFAE-Value
(define (interp rcfae ds)
  (type-case RCFAE rcfae
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds)(interp r ds))]
    [sub (l r) (num- (interp l ds)(interp r ds))]
    [id (name) (lookup name ds)]
    [fun (param body-expr)(closureV param body-expr ds)]
    [app (f a) (local [(define ftn (interp f ds))]
                 (interp (closureV-body ftn)
                         (aSub (closureV-param ftn)
                               (interp a ds)
                               (closureV-ds ftn))))]
    [if0 (test-expr then-expr else-expr)
         (if (numzero? (interp test-expr ds))
             (interp then-expr ds)
             (interp else-expr ds))]
    [rec (bound-id named-expr fst-call)
         (local [(define value-holder (box(numV 198)))
                 (define new-ds (aRceSub bound-id
                                         value-holder
                                         ds))]
                 (begin
                   (set-box! value-holder (interp named-expr new-ds))
                   (interp fst-call new-ds)))]))

(interp '{rec {count {fun {n} {if0 n 0 {+ 1 {count {- n 1}}}}}} {count 8}} (mtSub))