#lang racket

(require parser-tools/lex)

(require (prefix-in : parser-tools/lex-sre))


(define-lex-abbrev SYMBOL (:+ (:or alphabetic numeric #\-)))

(define-lex-abbrev NATURAL (:+ numeric))

(define s-lex
  (lexer
   [NATURAL    #;`(ATOM ,(string->number lexeme))
               (cons 'ATOM (cons (string->number lexeme) '()))]
   [SYMBOL     `(ATOM ,(string->symbol lexeme))]
   [#\(        '(LPAR)]
   [#\)        '(RPAR)]
   [whitespace  (s-lex input-port)]
   ["#t"       '(ATOM #t)]
   ["#f"       '(ATOM #f)]
   [(eof)      '(EOF)]))




(define next-token #f)

(define (peek)
  (if next-token
      next-token
      (begin
        (set! next-token (s-lex input))
        next-token)))

(define (next)
  (peek)
  (define tmp next-token)
  (set! next-token #f)
  tmp)

(define (eat expected-type)
  (match (next)
    [`(,token-type . ,_)
     (when (not (eq? expected-type token-type))
       (error "expected differs from received"))]))
  

(define (parse-S)
  (match (peek)
    ['(LPAR)
     ; =>
     (eat 'LPAR)
     (define tmp (parse-SL))
     (eat 'RPAR)
     tmp]
    
    [else
     ; =>
     (parse-A)]))

(define (parse-SL)
  (match (peek)
    [(or '(RPAR) '(EOF))
     ; =>
     '()]
    
    [else
     (define first (parse-S))
     (define rest (parse-SL))
     (cons first rest)]))
     

(define (parse-A)
  (match (next)
    [`(ATOM ,atom)
     ; =>
     atom]))


(define input (open-input-string "(123 (foo) 400)"))
