#lang racket
(require parser-tools/lex)

;-----------
; Oper defs
;-----------

(define-lex-abbrev <pls_mns>
  (union #\+ #\-))
(define-lex-abbrev <oper>
  (union <pls_mns> #\* #\/))

;-------------
; Number defs
;-------------
(define-lex-abbrev <int>
  (union
   (concatenation (repetition 0 1 #\-) #\0)
   (concatenation (repetition 0 1 #\-)
                  (char-range #\1 #\9)
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

(define algebra_lexer
  (lexer
   [<var> (cons `(VAR ,(string->symbol lexeme))
                (algebra_lexer input-port))]
   [<exp> (cons `(EXP ,(string->number 
                       (substring lexeme 1)))  ; We trim the ^ off the front for convenience later
                (algebra_lexer input-port))]
   [<num> (cons (string->number lexeme)
                (algebra_lexer input-port))]
   [<oper> (cons `(OPER ,(string->symbol lexeme))
                 (algebra_lexer input-port))]
   [#\( (cons '(LPAR)
              (algebra_lexer input-port))]
   [#\) (cons '(RPAR)
              (algebra_lexer input-port))]
   [whitespace (algebra_lexer input-port)]   ; ignore whitespace
   [(eof) '()]))

;------------
; Quick test
;------------
(algebra_lexer (current-input-port))