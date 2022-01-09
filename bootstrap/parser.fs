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

\ for mutual recursion
create parse-type-p 0 ,

: parse-tuple-type ( lexer -- node is-tuple )
    dup '(' expect-sym if
        dup lex
        dup ')' expect-sym if lex unit-type true exit then
        dup parse-type-p @ execute ?dup if swap else SYNTAX-ERROR throw then
        dup ')' expect-sym if lex false exit then \ "(" type ")" == type
        dup ',' expect-sym if dup lex else SYNTAX-ERROR throw then
        dup ')' expect-sym if \ "(" type "," ")"
            lex
            0 make-array tuck array-push
            TyTuple make-node1
            true
            exit
        then
        ( ty lexer )
        swap 0 make-array tuck array-push swap
        dup parse-type-p @ execute ?dup unless SYNTAX-ERROR throw then
        2 pick array-push
        begin dup ',' expect-sym while
            dup lex
            dup parse-type-p @ execute ?dup unless SYNTAX-ERROR throw then
            2 pick array-push
        repeat
        dup ')' expect-sym unless SYNTAX-ERROR throw then
        lex
        TyTuple make-node1
        true
    else
        drop 0 0
    then
;

: parse-type ( lexer -- node )
    dup parse-never-type ?dup if nip exit then
    dup parse-prim-type ?dup if nip exit then
    dup parse-tuple-type swap ?dup if
        ( lexer is-tuple ty )
        rot dup '-' expect-sym if
            dup lex_nospace
            dup '>' expect-sym unless SYNTAX-ERROR throw then
            dup lex
            recurse ?dup unless SYNTAX-ERROR throw then
            ( tup ret )
            rot if
                swap node>arg0 @
            else
                swap 0 make-array tuck array-push
            then
            TyFunc make-node2
        else
            drop nip
        then
        exit
    else
        2drop 0
    then
;

' parse-type parse-type-p !

: parse-place ( lexer -- node )
    dup parse-label ?dup if nip exit then
    dup parse-register ?dup if nip exit then
    dup '*' expect-sym unless drop 0 exit then
    dup lex
    recurse ?dup if make-deref else 0 then
;

: parse-operand ( lexer -- node )
    dup lexer>token_tag @ Tint = if
        dup lexer>token_val @ swap
        dup lex
        dup ':' expect-sym unless SYNTAX-ERROR throw then
        dup lex
        parse-type ?dup unless SYNTAX-ERROR throw then
        Nint make-node2
        exit
    then
    dup lexer>token_tag @ Ttrue = if lex true-value exit then
    dup lexer>token_tag @ Tfalse = if lex false-value exit then
    dup parse-label ?dup if nip exit then
    dup parse-register ?dup if nip exit then
    dup parse-argument ?dup if nip exit then
    dup '*' expect-sym unless drop 0 exit then
    dup lex
    recurse ?dup if make-deref else 0 then
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
    dup '(' expect-sym if \ tuple
        dup lex
        dup ')' expect-sym if
            lex 0 0 make-array Nmaketuple make-node2 exit
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
        0 swap Nmaketuple make-node2 exit
    then
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
    dup '.' expect-sym if
        dup lex
        dup lexer>token_tag @ Tint = unless SYNTAX-ERROR throw then
        dup lexer>token_val @ swap lex
        0 -rot Ntupleat make-node3 exit
    then
    dup '=' expect-sym if
        dup lex_nospace
        dup '=' expect-sym unless SYNTAX-ERROR throw then
        dup lex parse-operand 0 -rot Neq make-node3 exit
    then
    dup '!' expect-sym if
        dup lex_nospace
        dup '=' expect-sym unless SYNTAX-ERROR throw then
        dup lex parse-operand 0 -rot Nne make-node3 exit
    then
    dup '<' expect-sym if
        dup lex_nospace
        dup '=' expect-sym if
            dup lex parse-operand 0 -rot Nle make-node3 exit
        else
            dup lex parse-operand 0 -rot Nlt make-node3 exit
        then
    then
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
    drop 0 swap make-move
;

: parse-instruction ( lexer -- node )
    dup lexer>token_tag @ Tnop = if
        lex make-nop exit
    then
    dup parse-place ?dup unless drop 0 exit then
    over '=' expect-sym unless SYNTAX-ERROR throw then
    over lex
    over parse-expression ?dup unless SYNTAX-ERROR throw then
    tuck node>arg0 !
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
        dup parse-expression ?dup unless SYNTAX-ERROR throw then
        swap dup parse-label ?dup unless SYNTAX-ERROR throw then
        swap parse-label ?dup unless SYNTAX-ERROR throw then
        >r >r
        dup node>tag @ case
        Neq of dup node>arg1 @ swap node>arg2 @ r> r> Nifeq make-node4 endof
        Nne of dup node>arg1 @ swap node>arg2 @ r> r> Nifne make-node4 endof
        Nlt of dup node>arg1 @ swap node>arg2 @ r> r> Niflt make-node4 endof
        Nle of dup node>arg1 @ swap node>arg2 @ r> r> Nifle make-node4 endof
        Nmove of node>arg1 @ r> r> Niftrue make-node3 endof
        not-reachable
        endcase
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

: parse-function-definition ( lexer -- node )
    false swap \ export
    dup lexer>token_tag @ Tfun = if dup lex else 2drop 0 exit then
    dup parse-label ?dup if swap else SYNTAX-ERROR throw then
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
    ( export label ty body )
    0 \ place for comment
    make-fundef
;

: parse-variable-definition ( lexer -- node )
    drop 0
;

: parse-toplevel-definition ( lexer -- node )
    \ parse document
    s" " make-string swap
    begin dup lexer>token_tag @ Tdocument = while
        swap over lexer>token_buf concat-string swap
        dup lex
    repeat swap >r

    dup lexer>token_tag @ Texport = if dup lex true else false then >r

    dup parse-function-definition ?dup unless
    dup parse-variable-definition ?dup unless
        r> r> drop if SYNTAX-ERROR throw then
        drop 0 exit
    then then

    \ set export and document
    dup node>tag @ case
    Nfundef of
        r> over fundef>export !
        r> over fundef>comment !
    endof
    Nvardef of
        r> over vardef>export !
        r> over vardef>comment !
    endof
    not-reachable
    endcase
    nip
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
