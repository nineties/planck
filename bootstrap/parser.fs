\ planck -
\ Copyright (C) 2021 nineties

\ Parser of "subset of" PlanckIR
\ Since this parser is used only for bootstrapping phase,
\ some syntax element types, such as floating point literals,
\ are not necessary and not implemented.
\ See spec/syntax.rst.

include lib/string.fs
include lib/array.fs
include lexer.fs
include graph.fs

s" Sytax Error" exception constant SYNTAX-ERROR

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

: parse-never-type ( lexer -- node )
    dup '!' expect-sym if lex never-type else drop 0 then
;

: parse-prim-type ( lexer -- node )
    dup lexer>token_tag @ case
    Ttrue of lex true-type endof
    Tfalse of lex false-type endof
    Tchar of lex char-type endof
    Ti8 of lex i8-type endof
    Tu8 of lex u8-type endof
    Ti16 of lex i16-type endof
    Tu16 of lex u16-type endof
    Ti32 of lex i32-type endof
    Tu32 of lex u32-type endof
    Ti64 of lex i64-type endof
    Tu64 of lex u64-type endof
    Tf32 of lex f32-type endof
    Tf64 of lex f64-type endof
    2drop 0
    endcase
;

: parse-type ( lexer -- node )
    dup parse-never-type ?dup if nip exit then
    dup parse-prim-type ?dup if nip exit then
    not-implemented
;

: parse-expression ( lexer -- node )
    parse-place
;

: parse-instruction ( lexer -- node )
    dup parse-place ?dup unless drop 0 exit then
    over '=' expect-sym unless SYNTAX-ERROR throw then
    over lex
    over parse-expression ?dup unless SYNTAX-ERROR throw then
    swap make-assign
    swap drop
;

: parse-jump-instruction ( lexer -- node )
    dup lexer>token_tag @ Treturn = if
        make-return
        swap lex
    else
        drop 0
    then
;

: parse-basic-block ( lexer -- node )
    dup parse-label ?dup unless drop 0 exit then
    over ':' expect-sym unless SYNTAX-ERROR throw then
    over lex
    
    0 make-array
    begin 2 pick parse-instruction ?dup while over array-push repeat
    2 pick parse-jump-instruction ?dup unless SYNTAX-ERROR throw then
    ( lexer label array jump )
    basicblock% %allocate throw
    tuck basicblock>jump !
    tuck basicblock>insns !
    tuck basicblock>name !
    swap drop
;

: parse-function-params ( lexer -- node )
    0 make-array swap
    begin
        dup parse-register ?dup unless drop exit then
        over ':' expect-sym if over lex else SYNTAX-ERROR throw then
        over parse-type ?dup unless SYNTAX-ERROR throw then
        ( arr lexer reg type )
        swap make-paramdecl 2 pick array-push
        dup ',' expect-sym unless drop exit then
        dup lex
    again
;

: parse-function-definition ( lexer -- node )
    dup lexer>token_tag @ Texport = if dup lex true else false then swap
    dup lexer>token_tag @ Tfunction = if dup lex else  SYNTAX-ERROR throw then
    dup parse-label ?dup if swap else SYNTAX-ERROR throw then
    dup '(' expect-sym if dup lex else SYNTAX-ERROR throw then
    dup parse-function-params ?dup if swap else SYNTAX-ERROR throw then
    dup ')' expect-sym if dup lex else SYNTAX-ERROR throw then
    dup ':' expect-sym if dup lex else SYNTAX-ERROR throw then
    dup parse-type ?dup if swap else SYNTAX-ERROR throw then
    dup '{' expect-sym if dup lex else SYNTAX-ERROR throw then

    0 make-array swap
    dup parse-basic-block ?dup if 2 pick array-push else SYNTAX-ERROR throw then

    begin
        dup parse-basic-block ?dup if 2 pick array-push false else true then
    until

    drop
    ( export label params rettype body )
    fundef% %allocate throw
    tuck fundef>graph !
    tuck fundef>rettype !
    tuck fundef>params !
    tuck fundef>name !
    tuck fundef>export !
;

( Parse `input` string and returns abstract syntax tree )
: parse ( input -- graph )
    make-lexer
    dup lex
    dup parse-function-definition ?dup if nip exit then
    ." hoge" cr
    not-implemented
; export

}private

s"
export function main(%0:i8, %1:char): i32 {
block:
    %0 = %1
    return
block2:
    %2 = %3
    return
}

" make-string parse pp-fundef
