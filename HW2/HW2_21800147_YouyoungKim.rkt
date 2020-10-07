#lang plai

; Problem1: 
; Solved by myself: Y
; Time taken: about 25mins

;Define PWAE type
;<PWAE> ::= <num>                            
;	| <op>
;	| <id>
;	| <keyword>
;	| {<PWAE> <PWAE> <op>}
;    	| {{<id> <PWAE>} <PWAE> <keyword>}

(define-type PWAE
  [num (n number?)]
  [op (op symbol?)]
  [id (name symbol?)]
  [keyword (word symbol?)]
  [postfix (lhs PWAE?)(rhs PWAE?)(opr PWAE?)]
  [substitute (name symbol?)(named-expr PWAE?)(body PWAE?)(key PWAE?)])

  

; [contract] parse: sexp -> PWAE
; [purpose] to convert s-expression into PWAE
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list l r '+) (postfix (parse l)(parse r)(op 'add))]
    [(list l r '-) (postfix (parse l)(parse r)(op 'sub))]
    [(list (list i v) e 'with) (substitute i (parse v)(parse e)(keyword 'with))] 
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax:~a"sexp)]))

(parse '{{3 4 -} 7 +})
(parse '{{x 5} {x x +} with})
(parse '{{x {5 4 +}} {x x +} with})
(parse '{{x {5 5 +}} {x x +} with})
(parse '{{x 5} {8 2 +} with})

; Problem2: 
; Solved by myself: 
; Time taken: about
; [contracet] free-ids: PWAE -> list-of-sym
; [purpose]


; Problem3: 
; Solved by myself: 
; Time taken: about
; [contracet] binding-ids PWAE -> list-of-sym
; [purpose] find binding-ids
(binding-ids (postfix (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (op 'add)))
(binding-ids (substitute 'y (num 3) (substitute 'y (id 'x) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (keyword 'with)))

(define (binding-ids pwae)
  type-case PWAE pwae
  [postfix (l r o) ((binding-ids l)(binding-ids r)(binding-ids o))]
  [substitute (i v e) (

; Problem4: 
; Solved by myself: 
; Time taken: about
; [contracet] bound-ids PWAE -> list-of-sym
; [purpose]
