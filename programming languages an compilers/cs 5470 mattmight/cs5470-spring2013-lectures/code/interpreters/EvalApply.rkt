#lang racket

; <exp> ::= number
;        |  <var>
;        |  (+ <exp> <exp>)
;        |  (* <exp> <exp>)
;        |  (if <exp> <exp> <exp>)
;        |  (let <var> <exp> <exp>)
;        |  (λ <var> . <exp>)
;        |  (<exp> <exp>)

;        |  (set! <var> <exp> <exp>)


(struct clo {lam env})

(define (eval exp env)
  (match exp
    [(? number?)    exp]
    [(? symbol?)    (unbox (hash-ref env exp))]
    
    [`(+ ,a ,b)
     (+ (eval a env) (eval b env))]
    
    [`(* ,a ,b)
     (* (eval a env) (eval b env))]
    
    [`(if ,cond ,t ,f)
     ; =>
     (if (= (eval cond env) 0)
         (eval f env)
         (eval t env))]
    
    [`(set! ,var ,arg ,body)
     (define value (eval arg env))
     (set-box! (hash-ref env var) value)
     (eval body env)]
    
    [`(let ,var ,arg ,body)
     (let* ([argval (eval arg env)]
            [env*   (hash-set env var (box argval))])
       (eval body env*))]
    
    [`(λ ,v . ,b)
     ; =>
     (clo exp env)]
    
    [`(,f ,arg)
     ; =>
     (apply (eval f env)
            (eval arg env))]))

       

    
    
; value * value => value
(define (apply proc value)
  (match proc
    [(clo `(λ ,v . ,b) env)
     ; =>
     (define env* (hash-set env v (box value)))
     (eval b env*)]))


;(eval '((λ x . x) 3) #hasheq())


;(eval '(let u (λ f . (f f)) (u u)) #hasheq())

(eval '(let x 10 (set! x 3 (+ (let x 4 (set! x 17 x)) x))) #hasheq())








#;(struct clo {lam env})

; Assume: env is a hasheq-map from variables to values
#;(define (eval exp env)
  (match exp
    [(? number?)      exp]
    [(? symbol?)      (unbox (hash-ref env exp))]
    
    [`(+ ,e1 ,e2)     (+ (eval e1 env) (eval e2 env))]
    [`(* ,e1 ,e2)     (* (eval e1 env) (eval e2 env))]
    
    [`(if ,c ,t ,e)   (if (= (eval c env) 0) e t)]
    
    [`(let ,v ,a ,b)  (let* ([value  (eval a env)]
                             [env    (hash-set env v (box value))])
                        (eval b env))]
    
    [`(set! ,v ,a ,b) (begin (set-box! (hash-ref env v) 
                                       (eval a env))
                             (eval b env))]
                                
    
    [`(λ ,v . ,b)     (clo exp env)]
    
    [`(,f ,e)         (let ([proc  (eval f env)]
                            [value (eval e env)])
                        (apply proc value))]
    
    [else 
     (error "unrecognized expression form")]))
                       


#;(define (apply proc value)
  (match proc
    [(clo `(λ ,v . ,b) env)
     ; =>
     (let* ([env (hash-set env v (box value))])
       (eval b env))]))
    

#;(eval '((λ x . x) 3) #hasheq())

#;(eval '(let x 10
         (set! x 42
               x))
      #hasheq())

#;(eval '((λ f . (f f)) (λ g . (g g))) #hasheq())