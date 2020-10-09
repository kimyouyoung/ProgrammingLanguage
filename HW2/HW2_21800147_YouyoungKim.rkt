#lang plai

; Problem1-(a): 
; Solved by myself: Y
; Time taken: about 10mins
; [contract] define-type <PWAE>
; [purpose] define type of postfix WAE, PWAE

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

  
; Problem1-(b): 
; Solved by myself: Y
; Time taken: about 20mins
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

(test (parse '{{3 4 -} 7 +}) (postfix (postfix (num 3) (num 4) (op 'sub)) (num 7) (op 'add)))
(test (parse '{{x 5} {x x +} with}) (substitute 'x (num 5) (postfix (id 'x) (id 'x) (op 'add)) (keyword 'with)))
(test (parse '{{x {5 4 +}} {x x +} with}) (substitute 'x (postfix (num 5) (num 4) (op 'add)) (postfix (id 'x) (id 'x) (op 'add)) (keyword 'with)))
(test (parse '{{y {2 3 +}} {{x y} {x y +} with} with}) (substitute 'y (postfix (num 2) (num 3) (op 'add)) (substitute 'x (id 'y) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (keyword 'with)))
;(parse '{{x {5 5 +}} {x x +} with})
;(parse '{{x 5} {8 2 +} with})

; Problem2: 
; Solved by myself: Y
; Time taken: about 1hour
; [contracet] free-ids: PWAE -> list-of-sym
; [purpose] find free-ids

(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(define (make-list id lst)
  (remove-duplicates (sort (append id lst) symbol<?)))

(define (check-free-ids name lst-idtf)
  [cond
    [(empty? lst-idtf) (list name)]
    [(equal? (first lst-idtf) name) '()]
    [else (check-free-ids name (rest lst-idtf))]])

(define (sub-free pwae lst-idtf)
  (type-case PWAE pwae
    [num (n) '()]
    [op (o) '()]
    [id (name) (check-free-ids name lst-idtf)]
    [keyword (word) '()]
    [postfix (l r o) (append (sub-free l lst-idtf) (sub-free r lst-idtf))]
    [substitute (i v e k) (append (sub-free v lst-idtf) (sub-free e (append (list i) lst-idtf)))]))

(define (free-ids pwae)
  (type-case PWAE pwae
    [num (n) '()]
    [op (o) '()]
    [id (name) '()]
    [keyword (word) '()]
    [postfix (l r o) (make-list (free-ids l) (free-ids r))]
    [substitute (i v e k) (make-list (sub-free v '()) (sub-free e (list i)))]))

(test (free-ids (substitute 'x (num 3) (postfix (id 'x) (postfix (num 3) (id 'x) (op 'sub)) (op 'add)) (keyword 'with))) '())
(test (free-ids (substitute 'x (num 3) (postfix (id 'a) (postfix (num 4) (id 'x) (op 'add)) (op 'sub)) (keyword 'with))) '(a))
(test (free-ids (substitute 'x (num 3) (postfix (id 'b) (postfix (id 'a) (id 'x) (op 'sub)) (op 'sub)) (keyword 'with))) '(a b))
(test (free-ids (substitute 'x (num 3) (postfix (id 'a) (postfix (id 'b) (postfix (id 'x) (id 'b) (op 'add)) (op 'sub)) (op 'sub)) (keyword 'with))) '(a b))
(test (free-ids (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with))) '(a b y))
(test (free-ids (substitute 'x (id 't) (postfix (id 'x) (substitute 'y (id 'y) (postfix (id 'x) (postfix (id 'b) (id 'a) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with))) '(a b t y))
(test (free-ids (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with))) '(x y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'a) (id 'a) (keyword 'with)) (op 'add))) '(a b c y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'a) (keyword 'with)) (op 'add))) '(b c d y))
(test (free-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add))) '(b c d y z))

; Problem3: 
; Solved by myself: Y
; Time taken: about 30mins
; [contracet] binding-ids PWAE -> list-of-sym
; [purpose] find binding-ids

(define (binding-ids pwae)
  (type-case PWAE pwae
    [num (n) '()]
    [op (o) '()]
    [id (name) '()]
    [keyword (word) '()]
    [postfix (l r o) (binding-ids l)(binding-ids r)(binding-ids o)]
    [substitute (i v e k) (remove-duplicates (sort (append (append (list i)(binding-ids e))(binding-ids v)) symbol<?))]))

(test (binding-ids (postfix (num 3) (postfix (id 'x) (id 'y) (op 'sub)) (op 'add))) '())
(test (binding-ids (substitute 'y (num 3) (substitute 'x (id 'x) (id 'y) (keyword 'with)) (keyword 'with))) '(x y))
(test (binding-ids (substitute 'y (num 3) (substitute 'y (id 'x) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (keyword 'with))) '(y))
(test (binding-ids (substitute 'y (num 3) (substitute 'y (substitute 'x (postfix (num 3) (id 'y) (op 'sub)) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (keyword 'with))) '(x y))
(test (binding-ids (substitute 'z (num 3) (substitute 'w (substitute 'z (postfix (num 3) (id 'y) (op 'add)) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (substitute 'w (id 'y) (postfix (num 7) (id 'w) (op 'add)) (keyword 'with)) (keyword 'with)) (keyword 'with))) '(w z))

; Problem4: 
; Solved by myself: Y
; Time taken: about 4hours
; [contracet] bound-ids PWAE -> list-of-sym
; [purpose] find bound-ids

(define (check-ids name lst-idtf)
  [cond
    [(empty? lst-idtf) '()]
    [(equal? (first lst-idtf) name) (list name)]
    [else (check-ids name (rest lst-idtf))]])

(define (sub-bound pwae lst-idtf)
  (type-case PWAE pwae
    [num (n) '()]
    [op (o) '()]
    [id (name) (check-ids name lst-idtf)]
    [keyword (word) '()]
    [postfix (l r o) (append (sub-bound l lst-idtf) (sub-bound r lst-idtf))]
    [substitute (i v e k) (append (sub-bound v lst-idtf) (sub-bound e (append (list i) lst-idtf)))]))

(define (bound-ids pwae)
  (type-case PWAE pwae
    [num (n) '()]
    [op (o) '()]
    [id (name) '()]
    [keyword (word) '()]
    [postfix (l r o) (make-list (bound-ids l) (bound-ids r))]
    [substitute (i v e k) (make-list (bound-ids v) (sub-bound e (list i)))]))

(test (bound-ids (substitute 'x (num 3) (postfix (id 'y) (num 3) (op 'add)) (keyword 'with))) '())
(test (bound-ids (substitute 'x (num 3) (postfix (id 'x) (postfix (id 'x) (id 'y) (op 'sub)) (op 'add)) (keyword 'with))) '(x))
(test (bound-ids (substitute 'x (num 3) (postfix (id 'x) (substitute 'y (num 7) (postfix (id 'x) (id 'y) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x y))
(test (bound-ids (substitute 'x (num 3) (substitute 'y (id 'x) (postfix (num 3) (id 'y) (op 'sub)) (keyword 'with)) (keyword 'with))) '(x y))
(test (bound-ids (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (id 'x) (postfix (num 3) (num 7) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x))
(test (bound-ids (substitute 'x (id 'x) (postfix (id 'y) (substitute 'y (id 'y) (postfix (num 3) (substitute 'z (num 7) (postfix (id 'z) (id 'x) (op 'sub)) (keyword 'with)) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(x z))
(test (bound-ids (substitute 'x (substitute 'y (num 3) (postfix (id 'x) (id 'y) (op 'add)) (keyword 'with)) (postfix (id 'y) (substitute 'y (id 'y) (postfix (num 3) (num 7) (op 'sub)) (keyword 'with)) (op 'add)) (keyword 'with))) '(y))
(test (bound-ids (substitute 'x (id 'a) (substitute 'y (id 'b) (substitute 'z (id 'c) (postfix (id 'd) (postfix (id 'x) (postfix (id 'y) (id 'z) (op 'add)) (op 'sub)) (op 'sub)) (keyword 'with)) (keyword 'with)) (keyword 'with))) '(x y z))
(test (bound-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'sub)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'a) (keyword 'with)) (op 'add))) '(a x))
(test (bound-ids (postfix (substitute 'x (num 10) (substitute 'x (num 3) (postfix (id 'y) (substitute 'y (num 7) (postfix (id 'x) (postfix (id 'c) (id 'b) (op 'sub)) (op 'add)) (keyword 'with)) (op 'sub)) (keyword 'with)) (keyword 'with)) (substitute 'a (id 'd) (id 'z) (keyword 'with)) (op 'add))) '(x))
