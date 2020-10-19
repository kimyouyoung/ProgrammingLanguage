#lang plai
; interpreter => AE -> number
(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

; '{+ {- 2 1} 3} ==> concrete syntax
; ae1's type is 'add'
; add-rhs -> '-' = in java '.'
(define ae1 (add (sub (num 3)(num 4)) (num 7))) ; ==> abstract syntax
(sub? ae1)    ; #f
(add-lhs ae1) ; (sub (num 3)(num 4))
(add-rhs ae1) ; (num 7)
(sub-rhs (add-lhs ae1))  ;(num 4)

; sexp: sub-expression which is just source code
; parse: sexp -> AE
; to convert sub-expressions into AEs in abstract syntax
(define (parse sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(and (= 3 (length sexp))
          (eq? (first sexp) '+))
     (add(parse(second sexp))
         (parse(third sexp)))]
    [(and (= 3 (length sexp))
          (eq? (first sexp) '-))
     (sub(parse(second sexp))
         (parse(third sexp)))]
    [else (error 'parse "bad syntax: ~a" sexp)]
    ))

; tests
(test(parse '3)(num 3))
(test(parse '{+ 3 4})(add (num 3)(num 4)))
(test(parse '{- 4 3})(sub (num 4)(num 3)))
(test(parse '{+ {+ 4 3}{- 4 3}})(add(add (num 4)(num 3))(sub (num 4)(num 3))))
(test(parse '{+ {- 5 4} 5})(add(sub (num 5)(num 4))(num 5)))
(test/exn(parse '{-5 1 2})"parse: bad syntax: (-5 1 2)")
       
;(parse '{+ 3 4})
;(parse '{+ 3 4 5})

; [contract] interp: AE -> number
; [purpose] consumes an AE and compute the corresponding number.
(define (interp ae)
  (type-case AE ae
    ; n is recognized as actual number for computers.
    [num (n) n]
    ; add is recognized as a real behavior to sum two AEs. 
    [add (l r) (+(interp l)(interp r))]
    ; sub is recognized as a real behavior to substract two AEs.
    [sub (l r) (-(interp l)(interp r))]))

(interp (parse '3))
(interp (parse '{+ 1 2}))
(interp (parse '{+{- 1 2} 5}))

;infix parse
(define (parse-infix sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(and (= 3 (length sexp))
          (eq? (second sexp) '+))
     (add(parse-infix(first sexp))
         (parse-infix(third sexp)))]
    [(and (= 3 (length sexp))
          (eq? (second sexp) '-))
     (sub(parse-infix(first sexp))
         (parse-infix(third sexp)))]
    [else (error parse-infix "bad syntax: ~a" sexp)]
    ))

(interp (parse-infix '{2 + {9 - 2}}))

;postfix parse
(define (parse-post sexp)
  (cond
    [(number? sexp)(num sexp)]
    [(and (= 3 (length sexp))
          (eq? (third sexp) '+))
     (add(parse-post(first sexp))
         (parse-post(second sexp)))]
    [(and (= 3 (length sexp))
          (eq? (third sexp) '-))
     (sub(parse-post(first sexp))
         (parse-post(second sexp)))]
    [else (error parse-post "bad syntax: ~a" sexp)]
    ))

(interp (parse-post '{2{9 2 -}+}))