\ planckvm -
\ Copyright (C) 2021 nineties

\ Lexer of PlanckIR tokens.
\ See spec/syntax.rst.

include lib/string.fs

\ Token types
0
    enum Tid
    enum Tchar
    enum Tint
    enum Tfloat
    enum Tstr
drop

private{

struct
    cell% field lexer>input  ( input string )
    int%  field lexer>pos    ( current position )
    int%  field lexer>lineno ( current source line no. )
end-struct lexer%

1024 1024 * constant BUFFER_SIZE0

: make-lexer ( input -- lexer )
    lexer% %allocate throw
    tuck lexer>input !
    0 over lexer>pos !
    1 over lexer>lineno !
; export

: lex
    not-implemented
; export

}private

\ XXX: Temporary test code
s" aaaaaaa" make-string constant source
source make-lexer constant lexer
