#lang racket

(require parser-tools/yacc)
(require "algebra_lexer.rkt")
(algebra_lexer (current-input-port))