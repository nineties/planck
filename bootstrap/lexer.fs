\ planckvm -
\ Copyright (C) 2021 nineties

\ Lexer of PlanckIR tokens.
\ See spec/syntax.rst.

struct
    cell% field lexer>input  ( input string )
    int%  field lexer>pos    ( current position )
    int%  field lexer>lineno ( current source line no. )
end-struct lexer%

\ Token types
0
    enum Tid
    enum Tchar
    enum Tint
    enum Tfloat
    enum Tstr
drop

: lex
    not-implemented
;
