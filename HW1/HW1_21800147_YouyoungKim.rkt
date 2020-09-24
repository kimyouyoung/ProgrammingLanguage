#lang plai

; Problem1:
; Solved by myself: Y
; Time taken: about 3mins
; [contract] get-average: number number -> number
; [purpose] compute the average of two numbers.
; [test] (test (get-average 2 8) 5)
;        (test (get-average 9 10) 9.5)

(define (get-average n1 n2)
  (/ (+ n1 n2) 2))

(test (get-average 2 8) 5)
(test (get-average 9 10) 9.5)

; Problem2:
; Solved by myself: Y
; Time taken: about 3mins
; [contract] inchworm-travel: number -> number
; [purpose] indicates the distance traveled by the inchworm in centimeters.
; [test] (test (inchworm-travel 9) 22.86)
;        (test (inchworm-travel 33) 83.82)

(define (inchworm-travel hour)
  (* hour 2.54))

(test (inchworm-travel 9) 22.86)
(test (inchworm-travel 33) 83.82)

; Problem3:
; Solved by myself: Y
; Time taken: about 3mins
; [contract] volume-cube: number -> number
; [purpose] compute the volume of the cube.
; [test] (test (volume-cube 6) 216)
;        (test (volume-cube 27) 19683)

(define (volume-cube len)
  (* len len len))

(test (volume-cube 6) 216)
(test (volume-cube 27) 19683)

; Problem4:
; Solved by myself: Y
; Time taken: about 3mins
; [contract] my-BMI: number number -> number
; [purpose] compute BMI by receiving height and weight.
; [test] (test (my-BMI 50 1.63) 19)
;        (test (my-BMI 75 1.82) 23)

(define (my-BMI weight height)
  (round (/ weight (* height height))))

(test (my-BMI 50 1.63) 19)
(test (my-BMI 75 1.82) 23)

; Problem5:
; Solved by myself: Y
; Time taken: about 3hours
; [contract] fib: number -> list of number
; [purpose] the fibonacci number is computed and the list is returned.
; [test] (test (fib 9) '(1 1 2 3 5 8 13 21 34))
;        (test (fib 17) '(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597))
;        (test (fib 5) '(1 1 2 3 5))

(define (recursion start num lst)
  (cond
    [(= start (+ 1 num)) lst]
    [(or (= start 1) (= start 2)) (recursion (+ 1 start) num (append lst (list 1)))]
    [else (recursion (+ 1 start) num (append lst (list (+ (list-ref lst (- start 2)) (list-ref lst (- start 3))))))]))

(define (fib num)
  (recursion 1 num '()))

(test (fib 9) '(1 1 2 3 5 8 13 21 34))
(test (fib 17) '(1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597))
(test (fib 5) '(1 1 2 3 5))

; Problem6-a:
; Solved by myself: Y
; Time taken: about 7mins
; [contract] Vehicle : Bicycle(wheels), Car(wheels, windows), Airplane(wheels, windows, engines)
; [purpose] define the type Vehicle, which has three variants, Bicycle, Car, Airplane. 

(define-type Vehicle
  (Bicycle (num_wheels number?))
  (Car (num_wheels number?)
       (num_windows number?))
  (Airplane (num_wheels number?)
            (num_windows number?)
            (num_engines number?)))

; Problem6-b:
; Solved by myself: Y
; Time taken: about 5mins
; [contract] Vehicle -> number
; [purpose] taxes on vehicles are computed according to the number of wheels, windows and engines.
; [test] (test (vehicle-tax (Car 4 8)) 12)
;        (test (vehicle-tax (Airplane 8 22 8)) 38)

(define myBicycle(Bicycle 7))
(define myCar(Car 4 8))
(define myAirplane(Airplane 5 30 24))

(define (vehicle-tax v tax_w tax_win tax_e)
  (type-case Vehicle v
    [Bicycle (w) (* w tax_w)]
    [Car (w win) (+ (* w tax_w)(* win tax_win))]
    [Airplane (w win e) (+(+ (* w tax_w)(* win tax_win))(* e tax_e))]))

(test (vehicle-tax myCar 4 8 9) 80)
(test (vehicle-tax myAirplane 8 22 8) 892)

; Problem6-c:
; Solved by myself: Y
; Time taken: about 5mins
; [contract] Vehicle -> string
; [purpose] determine whether it is safe or unsafe in accordance with safety standards.
; [test] (test (is-vehicle-safe (Bicycle 3)) "safe")
;        (test (is-vehicle-safe (Airplane 2 8 2)) "unsafe")
;        (test (is-vehicle-safe (Car 6 1)) "unsafe")
;        (test (is-vehicle-safe (Airplane 10 33 3)) "safe")

(define (is-vehicle-safe v)
  (cond
    [(type-case Vehicle v
      [Bicycle (w) (< w 4)]
      [Car (w win) (and (>= w 3) (>= win 2))]
      [Airplane (w win e) (and (and (>= w 2) (>= win 10)) (>= e 1))]) "safe"]
    [else "unsafe"]))

(test (is-vehicle-safe (Bicycle 3)) "safe")
(test (is-vehicle-safe (Airplane 2 8 2)) "unsafe")
(test (is-vehicle-safe (Car 6 1)) "unsafe")
(test (is-vehicle-safe (Airplane 10 33 3)) "safe")

; Problem7:
; Solved by myself: Y (I see the example code from https://docs.racket-lang.org/reference/strings.html - string-append, string=?)
; Time taken: about 7mins
; [contract] update-name: string string list of string -> list of string
; [purpose] list of strings by appending source to all occurrences of target.
; [test] (test (update-name "john" " ate a meal" '("ken" "john" "esther" "henny")) ("ken" "john ate a meal" "esther" "henny"))
;        (test (update-name "julia" " studies english" '("julia" "marvin" "daniel" "claire" "julia" "mike")) ("julia studies english" "marvin" "daniel" "claire" "julia studies english" "mike"))
;        (test (update-name "claire" " is nice" '("jc" "claire" "kate")) '("jc" "claire is nice" "kate"))
;        (test (update-name "kyy" " is a student" '("kyy" "kyj" "ky" "kyy")) '("kyy is a student" "kyj" "ky" "kyy is a student"))

(define (find-name target source lst len start final_lst)
  (cond
    [(= start len) final_lst]
    [else
     (cond
       [(string=? target (list-ref lst start)) (find-name target source lst len (+ 1 start) (append final_lst (list (string-append target source))))]
       [else (find-name target source lst len (+ 1 start) (append final_lst (list (list-ref lst start))))])]))

(define (update-name target source lst)
  (find-name target source lst (length lst) 0 '()))

(test (update-name "john" " ate a meal" '("ken" "john" "esther" "henny")) '("ken" "john ate a meal" "esther" "henny"))
(test (update-name "julia" " studies english" '("julia" "marvin" "daniel" "claire" "julia" "mike")) '("julia studies english" "marvin" "daniel" "claire" "julia studies english" "mike"))
(test (update-name "claire" " is nice" '("jc" "claire" "kate")) '("jc" "claire is nice" "kate"))
(test (update-name "kyy" " is a student" '("kyy" "kyj" "ky" "kyy")) '("kyy is a student" "kyj" "ky" "kyy is a student"))

; Problem8:
; Solved by myself: Y(I see the example code from https://docs.racket-lang.org/reference/generic-numbers.html - quotient)
; Time taken: about 20mins
; [contract] binary-search: list of number number -> list of number
; [purpose] returns a list of numbers that are binary search pass records.
; [test] (test (binary-search '(3 5 9 11 15 26 33) 26) '(11 26))
;        (test (binary-search '(1 2 5 7 9 13 29 34 38 49) 13) '(9 34 13))

(define (binary-search-traversal-history lst target low high final_lst)
  (cond
   [(> low high) final_lst]
   [(= (list-ref lst (quotient (+ low high) 2)) target) (append final_lst (list target))]
   [(< (list-ref lst (quotient (+ low high) 2)) target) (binary-search-traversal-history lst target (+ 1 (quotient (+ low high) 2)) high (append final_lst (list (list-ref lst (quotient (+ low high) 2)))))]
   [(> (list-ref lst (quotient (+ low high) 2)) target) (binary-search-traversal-history lst target low (- (quotient (+ low high) 2) 1) (append final_lst (list (list-ref lst (quotient (+ low high) 2)))))]))

(define (binary-search lst target)
  (binary-search-traversal-history lst target 0 (- (length lst) 1) '()))

(test (binary-search '(3 5 9 11 15 26 33) 26) '(11 26))
(test (binary-search '(1 2 5 7 9 13 29 34 38 49) 13) '(9 34 13))
(test (binary-search '(1 2 3) 3) '(2 3))
(test (binary-search '(1 2 3 4 5 6 7 8) 3) '(4 2 3))
(test (binary-search '(1 2 3 4 5 6 7 8 9 10) 9) '(5 8 9))
(test (binary-search '(1 2 3 4 5) 5) '(3 4 5))

