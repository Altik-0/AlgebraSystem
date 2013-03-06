#lang racket
(require parser-tools/lex)
(require parser-tools/yacc)

;-----------
; Oper defs
;-----------

(define-lex-abbrev <pls_mns>
  (union #\+ #\-))

(define-lex-abbrev <plus>  #\+)
(define-lex-abbrev <minus> #\-)
(define-lex-abbrev <mult>  #\*)
(define-lex-abbrev <div>   #\/)

;-------------
; Number defs
;-------------
(define-lex-abbrev <int>
  (union
   (concatenation #\0)
   (concatenation (char-range #\1 #\9)
                  (repetition 0 +inf.0
                              (char-range #\0 #\9)))))
(define-lex-abbrev <pt_float>
  (concatenation (repetition 0 1 <int>)
                 #\.
                 (repetition 1 +inf.0
                             (char-range #\0 #\9))))
(define-lex-abbrev <float_exp>
  (concatenation (union #\e #\E)
                 (repetition 0 1 <pls_mns>)
                 (repetition 1 +inf.0
                             (char-range #\0 #\9))))
(define-lex-abbrev <float>
  (union <pt_float>
         (concatenation (union <int> <pt_float>)
                        <float_exp>)))

(define-lex-abbrev <num>
  (union <int> <float>))

;-----------
; Var defs
;-----------

(define-lex-abbrev <exp>
  (concatenation #\^ <num>))
(define-lex-abbrev <var>
  (union (char-range #\a #\z)
         (char-range #\A #\Z)))

;===========
; Lexer Def
;===========

; Token definitions - required by parser
(define-tokens group-a (VAR EXP NUM))
(define-empty-tokens group-opers (PLUS MINUS MULT DIV)) ; Seperates operators into precedence and associativity groupings
(define-empty-tokens group-b (LPAR RPAR EOF))

(define algebra_lexer
  (lexer
   [<var> (token-VAR (string->symbol lexeme))]
   [<exp> (token-EXP (string->number (substring lexeme 1)))] ; we trim the carat for convenience later
   [<num> (token-NUM (string->number lexeme))]
   [<plus> (token-PLUS)]
   [<minus> (token-MINUS)]
   [<mult> (token-MULT)]
   [<div> (token-DIV)]
   [#\( (token-LPAR)]
   [#\) (token-RPAR)]
   [whitespace (algebra_lexer input-port)]   ; ignore whitespace
   [(eof) (token-EOF)]))

(define algebra_parser
  (parser
   (tokens group-a group-b group-opers)
   (start poly)
   (end EOF)
   (error "dafuq")
   (grammar
    ; <atom> ::= NUM
    ;         |  VAR
    ;         |  VAR EXP
    ;         |  (<poly>)
    ;         |  (<poly>) EXP
    (atom ((NUM) $1)
          ((VAR) `(exp ,$1 1))
          ((VAR EXP) `(exp ,$1 ,$2))
          ((LPAR poly RPAR) $2)
          ((LPAR poly RPAR EXP) `(exp ,$2 ,$4)))
    
    ; <poly-term> ::= <atom> <poly-term>
    ;              |  <atom>
    (poly-term ((atom poly-term) `(* ,$1 ,$2))
               ((atom) $1))
    
    ; Handle operator precedence
    (div-term ((div-term DIV poly-term) `(/ ,$1 ,$3))
              ((poly-term) $1))
    (mult-term ((div-term MULT mult-term) `(* ,$1 ,$3))
               ((div-term) $1))
    (minus-term ((minus-term MINUS mult-term) `(- ,$1 ,$3))
                ((mult-term) $1))
    (plus-term ((minus-term PLUS plus-term) `(+ ,$1 ,$3))
               ((minus-term) $1))
    
    (poly ((plus-term) $1)))))

;-------
; Input
;-------
(define (lex-it! lexer input) (lambda () (lexer input)))
(define lexed (lex-it! algebra_lexer (current-input-port)))
(pretty-write (algebra_parser lexed))