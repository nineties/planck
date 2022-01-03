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

s" Syntax Error" exception constant SYNTAX-ERROR

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

: parse-argument ( lexer -- node )
    dup '$' expect-sym unless drop 0 exit then
    dup lex_nospace
    dup lexer>token_tag @ Tint = if
        dup lexer>token_val @ make-argument
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

: parse-operand ( lexer -- node )
    dup lexer>token_tag @ Tint = if
        dup lexer>token_val @ Nuint make-node1
        swap lex exit
    then
    dup lexer>token_tag @ Ttrue = if lex 1 Nbool make-node1 exit then
    dup lexer>token_tag @ Tfalse = if lex 0 Nbool make-node1 exit then
    dup parse-label ?dup if nip exit then
    dup parse-register ?dup if nip exit then
    dup parse-argument ?dup if nip exit then
    dup '*' expect-sym unless drop 0 exit then
    dup lex
    recurse ?dup if make-deref else 0 then
;

: parse-never-type ( lexer -- node )
    dup '!' expect-sym if lex never-type else drop 0 then
;

: parse-prim-type ( lexer -- node )
    dup lexer>token_tag @ case
    Tbool of lex bool-type endof
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
    2drop 0 exit
    endcase
;

: parse-type ( lexer -- node )
    dup parse-never-type ?dup if nip exit then
    dup parse-prim-type ?dup if nip exit then
    drop 0
;

: parse-phi-arg ( lexer -- node )
    dup parse-label ?dup unless drop 0 exit then
    over ':' expect-sym unless SYNTAX-ERROR throw then
    over lex
    over parse-place ?dup unless SYNTAX-ERROR throw then
    ( label place )
    2 cells allocate throw
    tuck tuple1 !
    tuck tuple0 !
    nip
;

: parse-phi-expression ( lexer -- node )
    dup lex
    dup '(' expect-sym unless SYNTAX-ERROR throw then
    dup lex
    0 make-array swap
    dup parse-phi-arg ?dup unless SYNTAX-ERROR throw then
    2 pick array-push
    begin dup ',' expect-sym while
        dup lex
        dup parse-phi-arg ?dup unless SYNTAX-ERROR throw then
        2 pick array-push
    repeat
    dup ')' expect-sym unless SYNTAX-ERROR throw then
    lex
    0 swap make-phi
;

: parse-expression ( lexer -- node )
    dup lexer>token_tag @ Tphi = if parse-phi-expression exit then
    dup parse-operand swap
    dup '+' expect-sym if dup lex parse-operand 0 -rot Nadd make-node3 exit then
    dup '-' expect-sym if dup lex parse-operand 0 -rot Nsub make-node3 exit then
    dup '*' expect-sym if dup lex parse-operand 0 -rot Nmul make-node3 exit then
    dup '/' expect-sym if dup lex parse-operand 0 -rot Ndiv make-node3 exit then
    dup lexer>token_tag @ Tmod = if
        dup lex parse-operand 0 -rot Nmod make-node3 exit
    then
    dup '&' expect-sym if dup lex parse-operand 0 -rot Nand make-node3 exit then
    dup '|' expect-sym if dup lex parse-operand 0 -rot Nor  make-node3 exit then
    dup '^' expect-sym if dup lex parse-operand 0 -rot Nxor make-node3 exit then
    dup '(' expect-sym if
        dup lex

        \ function call
        over node>tag @ Nid = unless SYNTAX-ERROR throw then
        dup ')' expect-sym if
            lex
            \ function call with no argument
            0 make-array 0 -rot Ncall make-node3 exit
        then
        0 make-array swap
        dup parse-operand ?dup unless SYNTAX-ERROR throw then
        2 pick array-push
        begin dup ',' expect-sym while
            dup lex
            dup parse-operand ?dup unless SYNTAX-ERROR throw then
            2 pick array-push
        repeat
        dup ')' expect-sym unless SYNTAX-ERROR throw then
        lex
        0 -rot Ncall make-node3 exit
    then
    drop
;

: parse-instruction ( lexer -- node )
    dup lexer>token_tag @ Tnop = if
        lex make-nop exit
    then
    dup parse-place ?dup unless drop 0 exit then
    over '=' expect-sym unless SYNTAX-ERROR throw then
    over lex
    over parse-expression ?dup unless SYNTAX-ERROR throw then
    ( lexer lhs rhs )

    dup node>tag @ case
    Nphi of tuck node>arg0 ! endof
    Nadd of tuck node>arg0 ! endof
    Nsub of tuck node>arg0 ! endof
    Nmul of tuck node>arg0 ! endof
    Ndiv of tuck node>arg0 ! endof
    Nmod of tuck node>arg0 ! endof
    Nand of tuck node>arg0 ! endof
    Nor  of tuck node>arg0 ! endof
    Nxor of tuck node>arg0 ! endof
    Ncall of tuck node>arg0 ! endof
    drop make-move 0
    endcase
    nip
;

: parse-branch-instruction ( lexer -- node )
    dup lexer>token_tag @ Tgoto = if
        dup lex
        parse-label ?dup unless SYNTAX-ERROR throw then
        make-goto
    else dup lexer>token_tag @ Treturn = if
        dup lex
        parse-operand ?dup unless SYNTAX-ERROR throw then
        make-return
    else dup lexer>token_tag @ Tif = if
        dup lex
        dup parse-operand ?dup unless SYNTAX-ERROR throw then
        swap dup parse-label ?dup unless SYNTAX-ERROR throw then
        swap parse-label ?dup unless SYNTAX-ERROR throw then
        Niftrue
        make-node3
    else
        drop 0
    then then then
;

: parse-basic-block ( lexer -- node )
    dup parse-label ?dup unless drop 0 exit then
    over ':' expect-sym unless SYNTAX-ERROR throw then
    over lex
    
    0 make-array ( phi insns )
    0 make-array ( non-branch insns )
    begin 3 pick parse-instruction ?dup while
        dup node>tag @ Nphi = if
            \ phi instructions must be placed at the beginning of basic block
            over array-size 0 <> if SYNTAX-ERROR throw then
            2 pick array-push
        else
            over array-push
        then
    repeat
    3 pick parse-branch-instruction ?dup unless SYNTAX-ERROR throw then
    ( lexer label phi-insns insns jump )
    make-bblock
    nip
;

: parse-function-params ( lexer -- node )
    0 make-array swap
    dup parse-type ?dup unless drop exit then
    2 pick array-push
    begin
        dup ',' expect-sym unless drop exit then
        dup lex
        dup parse-type ?dup unless SYNTAX-ERROR throw then
        ( arr lexer type )
        2 pick array-push
    again
;

: parse-function-definition ( lexer -- node )
    dup lexer>token_tag @ Texport = if dup lex true else false then swap
    dup lexer>token_tag @ Tfunction = if dup lex else 2drop 0 exit then
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
    dup '}' expect-sym if dup lex else SYNTAX-ERROR throw then

    drop
    ( export label params rettype body )
    make-fundef
;

: parse-toplevel-definition ( lexer -- node )
    parse-function-definition
;

( Parse `input` string and returns abstract syntax tree )
: parse ( input -- graph )
    make-lexer
    0 make-array swap
    dup lex
    begin
        dup parse-toplevel-definition ?dup if
            2 pick array-push false
        else
            dup lexer>token_tag @ Tnull <> if SYNTAX-ERROR throw then
            true
        then
    until
    free
    make-program
; export

}private
