#lang racket

(require "derivative-parsers.rkt") 
(require srfi/41) ; for streams


; Empty string

;(parse (lang (eps)) (stream))
;(parse (lang (eps '())) (stream))


; Tokens
(define NUM  (lang (? number? 'number))) 
(define PLUS (lang (? (λ (token) (eq? token '+)) 'plus)))
(define SPLUS (lang "+"))

;(parse NUM (stream 3))
;(parse NUM (stream #f))
;(parse PLUS (stream '+))
;(parse SPLUS (stream "+"))


; Alternation
;(parse (lang (or "a" "b")) (stream "a"))
;(parse (lang (or "a" "b")) (stream "b"))



; Sequence
;(parse (lang (seq "a" "b" "c")) (stream "a" "b" "c"))
;(parse (lang (seq* "a" "b" "c")) (stream "a" "b" "c"))

; Selective sequence:
#;(parse (lang (seq! `"a" `"b" "c" `"d")) 
       (stream "a" "b" "c" "d"))


; Ambiguous term grammar:
(define term (lang (or (seq! `term `"+" `term)
                       NUM)))

;(parse term (stream 3 "+" 3))
;(parse term (stream 3 "+" 3 "+" 3))



; Repetition

;(parse (lang (rep (or "a" "b"))) (stream "a" "b" "a" "b"))



; Option

(define bin-int
  (lang (seq* (opt "-" "+")
              (rep+ (or "0" "1")))))

;(parse bin-int
;       (stream "-" "0" "1"))

;(parse bin-int
;       (stream "0" "1"))



; Reduction

; '("-" ("0" "1"))

#;(parse (lang (--> bin-int (λ (num) 
                            (car (cdr num)))))
       (stream "-" "0" "1"))
                            
#;(parse (lang (@--> bin-int (λ (sign num) 
                             num)))
       (stream "-" "0" "1"))

#;(parse (lang ($--> bin-int
                   (car (cdr $$))))
       (stream "-" "0" "1"))

#;(parse (lang ($--> bin-int
                   ($ 1)))
       (stream "-" "0" "1"))

#;(parse (lang (>--> bin-int
                   [(cons sign (cons num '())) num]
                   #;[(list sign num) num]
                   #;[`(,sign ,num) #;=> num]))
       (stream "-" "0" "1"))


; Grammars

(define exp-grammar
  (grammar
   [exp    (or ($--> (seq* exp "+" term)
                     (list '+ ($ 0) ($ 2)))
               term)]
   
   [term   (or (>--> (seq* term "*" factor)
                     [`(,t1 "*" ,t2) 
                      `(* ,t1 ,t2)])
               factor)]
   
   [factor (or #;(car (seq! "(" `exp ")"))
               (@--> (seq* "(" exp ")")
                     (λ (lpar exp rpar) exp))
               (? number? 'num))]
   exp))


(parse exp-grammar (stream "(" 3 "+" 4 ")" "*" 10))



(define exp-red-grammar
  (grammar
   [exp    (or ($--> (seq* exp "+" term)
                     `(+ ,($ 0) ,($ 2)))
               term)]
   
   [term   (or (>--> (seq* term "*" factor)
                     [`(,t1 "*" ,t2)  #;=> `(* ,t1 ,t2)])
               factor)]
   
   [factor (or (@--> (seq* "(" exp ")")
                     (λ (lpar exp rpar) exp))
               (? number? 'num))]
   exp))

#;(parse exp-red-grammar 
         (stream "(" 3 "+" 4 ")" "*" 10))

