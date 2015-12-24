#lang racket

; <exp> ::= number
;        |  (+ <exp> <exp>)
;        |  (* <exp> <exp>)
;        |  (if <exp> <exp> <exp>)
;        |  (let <var> <exp> <exp>)
;        |  (λ v . b)
;        |  (<exp> <exp>)


(define (atom? exp)
  (match exp
    [(? number?)  #t]
    [`(λ ,(? symbol?) . ,_)  #t]
    [else #f]))
      

(define (sub exp v a)
  
  (define (is-v? x)
    (eq? v x))
  
  (match exp
    [(? number?)     exp]
    [(? is-v?)       a]
    [(? symbol?)     exp]
    [`(+ ,x ,y)     `(+ ,(sub x v a) ,(sub y v a))]
    [`(* ,x ,y)     `(* ,(sub x v a) ,(sub y v a))]
    [`(if ,c ,t ,f) `(if ,(sub c v a) ,(sub t v a) ,(sub f v a))]
    [`(let ,(? is-v?) ,arg ,body)
     ; =>
     `(let ,v ,(sub arg v a) ,body)]
    
    [`(let ,v* ,arg ,body)
     ; =>
     `(let ,v* ,(sub arg v a) ,(sub body v a))]
    
    [`(λ ,(? is-v?) . ,body)
     ; =>
     exp]
    
    [`(λ ,var . ,body)
     ; =>
     `(λ ,var . ,(sub body v a))]
    
    [`(,f ,arg)
     ; =>
     `(,(sub f v a) ,(sub arg v a))]))
      

(define (reduce exp)
  (match exp
    [(? number?)   #;=>   exp]
    
    [`(if ,(and cond (? atom?)) ,t ,f)
     ; =>
     (if (= 0 cond)
         f
         t)]
    
    [`(let ,(and var (? symbol?)) ,(and arg (? atom?)) ,body)
     ; =>
     (sub body var arg)]
    
    [`(let ,(and var (? symbol?)) ,arg ,body)
     ; =>
     `(let ,var ,(reduce arg) ,body)]
    
    [`(if ,cond ,t ,f)
     ; =>
     `(if ,(reduce cond) ,t ,f)]
    
    [`(+ ,(and a (? number?)) ,(and b (? number?)))
     ; =>
     (+ a b)]
    
    [`(+ ,a ,(and b (? number?)))
     ; =>
     `(+ ,(reduce a) ,b)]
    
    [`(+ ,(and a (? number?)) ,b)
     ; =>
     `(+ ,a ,(reduce b))]
    
    
    [`(* ,(and a (? number?)) ,(and b (? number?)))
     ; =>
     (* a b)]
    
    [`(* ,a ,(and b (? number?)))
     ; =>
     `(* ,(reduce a) ,b)]
    
    [`(* ,(and a (? number?)) ,b)
     ; =>
     `(* ,a ,(reduce b))]
    
    [`(λ ,_ . ,_)
     exp]
    
    [`((λ ,var . ,body) ,(and arg (? atom?)))
     ; =>
     (sub body var arg)]
    
    [`((λ ,var . ,body) ,arg)
     ; =>
     `((λ ,var . ,body) ,(reduce arg))]
    
    [`(,f ,arg)
     ; =>
     `(,(reduce f) ,arg)]
    
    ))


(define (reduce* exp)
  (let ((exp* (reduce exp)))
    (if (equal? exp exp*)
        exp
        (reduce* exp*))))

(reduce* '(+ 7 (+ 3 4)))

(reduce* '(if (+ -1 1) 1 2))


(reduce* '(let x 3 (let x x x)))

(reduce* '(let id (λ x . x) (id 3)))

(reduce* '(((λ x . x) (λ x . x)) 4))

(reduce* '(let u (λ f . (f f)) (u u)))


#;(define (atom? exp)
  (match exp
    [(? number?)    #t]
    [`(λ ,_ . ,_)   #t]
    
    [else #f]))

#;(define (sub v a e)
  
  (define (is-v? v*)
    (eq? v v*))
  
  (match e
    [(? atom?)    e]
    [(? is-v?)    a]
    [(? symbol?)  e]
    
    [`(+ ,e1 ,e2)  `(+ ,(sub v a e1)
                       ,(sub v a e2))]
    
    [`(* ,e1 ,e2)  `(* ,(sub v a e1)
                       ,(sub v a e2))]
    
    [`(if ,c ,t ,e) `(if ,(sub v a c)
                         ,(sub v a t)
                         ,(sub v a e))]
    
    [`(let ,(? is-v?) ,x ,b) 
     ; =>
     `(let ,v ,(sub v a x) ,b)]
    
    [`(let ,v* ,x ,b)
     ; =>
     `(let ,(sub v a x) ,(sub v a b))]
    
    [`(λ ,(? is-v?) ,b)
     `(λ ,v ,b)]
    
    [`(λ ,v* ,b)
     `(λ ,v* ,(sub v a b))]
    
    [`(,f ,x)
     `(,(sub v a f) ,(sub v a x))]))
        

#;(define (reduce exp)
  (match exp
    [(? number?)
     exp]
    
    [`(let ,v ,(and a (? atom?)) ,b)
     ;=>
     (sub v a b)]
    
    [`(let ,v ,a ,b)
     ;=>
     `(let ,v ,(reduce a) ,b)]
    
    [`(if ,(and cond (? atom?)) ,then ,else)
     ;=>
     (if (eq? cond 0)
         else
         then)]
    
    [`(if ,cond ,then ,else)
     ;=>
     `(if ,(reduce cond) ,then ,else)]
     
    [`(+ ,(and a (? number?)) ,(and b (? number?)))
     ;=>
     (+ a b)]
    
    [`(+ ,a ,(and b (? number?)))
     ;=>
     `(+ ,(reduce a) ,b)]
    
    [`(+ ,a ,b)
     ;=>
     `(+ ,a ,(reduce b))]
    
    [`(* ,(and a (? number?)) ,(and b (? number?)))
     ;=>
     (* a b)]
    
    [`(* ,a ,(and b (? number?)))
     ;=>
     `(* ,(reduce a) ,b)]
    
    [`(* ,a ,b)
     ;=>
     `(* ,a ,(reduce b))]
     
    [`((λ ,v . ,b) ,(and a (? atom?)))
     ;=>
     (sub v a b)]
    
    
    [`((λ ,v . ,b) ,a)
     ;=>
     `((λ ,v . ,b) ,(reduce a))]
    
    [`(,f ,x)
     ;=>
     `(,(reduce f) ,x)]))
    
    
    
    


#;(define (reduce* exp)
  
  (define exp* (reduce exp))
  
  (if (equal? exp exp*)
      exp*
      (reduce* exp*)))
      

;(reduce* '(if (+ -18 (+ (* 3 4) (* 2 3))) 1 0))

;(reduce* '(let v 3 (+ v 4)))

;(reduce* '(let f (λ x . (+ x 1)) (f 10)))

;(reduce* '(let U (λ f . (f f)) (U U)))