#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

(require parser-tools/yacc)


(define-tokens sexp-tokens (ID NUM STRING))

(define-empty-tokens sexp-delimiters (LPAR RPAR EOF))

(define-empty-tokens sexp-literals (TRUE FALSE))


(define-lex-abbrev symbol-char
  (:or alphabetic numeric #\+ #\* #\- #\% #\^ #\$))

(define-lex-abbrev symbol
  (:+ symbol-char))

(define-lex-abbrev decimal-int
  (:: (:? #\-) (:+ (char-range #\0 #\9))))

(define-lex-abbrev comment 
  (:: "#" (:* (char-complement #\newline))))

(define sexp-lexer 
  (lexer 
   [decimal-int  (token-NUM (string->number lexeme))]
   [symbol       (token-ID (string->symbol lexeme))]
   ["#t"         (token-TRUE)]
   ["#f"         (token-FALSE)]
   [#\(          (token-LPAR)]
   [#\)          (token-RPAR)]
   [#\"          ((string-lexer '()) input-port)]
   [whitespace   (sexp-lexer input-port)]
   [comment      (sexp-lexer input-port)]
   [(eof)        (token-EOF)]))

(define (string-lexer chars)
  (lexer
   [#\" 
    ; =>
    (token-STRING (list->string (reverse chars)))]
   
   [(:: #\\ any-char)
    ; =>
    ((string-lexer (cons (string-ref lexeme 1) chars)
                   input-port))]
   
   [any-char ((string-lexer (cons (string-ref lexeme 0) chars))
              input-port)]))
   

(define sexp-parser
  (parser
   (tokens sexp-tokens sexp-literals sexp-delimiters)
   
   (error (λ problem 
            (display "problem: ")
            (display problem)
            (newline)
            (error "problem")))
   
   (start s-exp)
   
   (end EOF)
   
   (grammar
    
    {s-exp   [(atom)             $1]
             [(LPAR s-exp* RPAR) $2]}
    
    {s-exp*  [(s-exp s-exp*)   (cons $1 $2)]
             [()              '()]}
    
    {atom    [(ID)      $1]
             [(NUM)     $1]
             [(STRING)  $1]
             [(TRUE)    #t]
             [(FALSE)   #f]})))


(define i (open-input-string "(foo (123 \"baz\" bar) bar)"))

(define tok-gen 
  (λ ()
    (sexp-lexer i)))

(define t (sexp-parser tok-gen))