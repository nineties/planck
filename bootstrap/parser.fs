\ planck -
\ Copyright (C) 2021 nineties

\ Parser of "subset of" PlanckIR
\ Since this parser is used only for bootstrapping phase,
\ some syntax element types, such as floating point literals,
\ are not necessary and not implemented.
\ See spec/syntax.rst.

include lib/string.fs
include lexer.fs
include graph.fs

private{

: expect-sym ( lexer c -- bool )
    over lexer>token_tag @ Tsymbol = unless 2drop false exit then
    swap lexer>token_val @ =
;

: parse-label ( lexer -- node )
    dup lexer>token_tag @ Tid = if
        dup lexer>token_buf make-id
        swap lex
    else
        drop 0
    then
;

: parse-register ( lexer -- node )
    dup '%' expect-sym unless drop 0 exit then
    dup lex_nospace
    dup lexer>token_tag @ Tint = if
        dup lexer>token_val @ make-register
        swap lex
    else
        drop 0
    then
;

: parse-place ( lexer -- node )
    dup parse-label ?dup if nip exit then
    dup parse-register ?dup if nip exit then
    dup '*' expect-sym unless drop 0 exit then
    dup lex
    recurse ?dup if make-deref else 0 then
;


( Parse `input` string and returns abstract syntax tree )
: parse ( input -- graph )
    make-lexer
    dup lex
    dup parse-place ?dup if nip exit then
    not-implemented
; export

}private

s"
*x123
" make-string parse pretty-print
