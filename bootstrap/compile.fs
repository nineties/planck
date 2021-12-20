\ planck -
\ Copyright (C) 2021 nineties

\ This is an implementation of PlanckVM compiler
\ in PlanckForth.
\ It compiles PlanckIR programs to object files
\ for the vm.

include parser.fs
include lib/table.fs

struct
    cell% field compiler>IdT    ( identifier table )
end-struct compiler%

0
    enum SecIdT
drop

struct
    char% 4 * field section>type
    int% field section>bytes
end-struct section%

private{

\ align n1 to u-byte boundary
: aligned-by ( n1 u -- n2 )
    1- dup invert   \ ( n1 u-1 ~(u-1) )
    -rot + and
;

\ Construct Control-Flow Graph from an array of basic blocks
: construct-CFG ( arr -- graph )
;

: make-compiler ( -- compiler )
    compiler% %allocate throw
    make-string-table over compiler>IdT !
;

\ Add an id to IdT section if not exist. Returns index of the id.
: get-id ( id compiler -- n )
    swap node>arg0 @ swap compiler>IdT @
    2dup ?table-in if table@ exit then
    dup table-size dup >r -rot table! r>
;

: compile-fundecl ( node compiler -- )
    ." compiling function: " over fundecl>name @ pp-node cr
    over fundecl>name @ over get-id drop
    2drop
    \ over fundecl>body construct-CFG
    \ not-implemented
;

: compile-definition ( def compiler -- )
    over node>tag @ case
    Nfundecl of compile-fundecl endof
        not-reachable
    endcase
;

: compile-program ( program -- compiler )
    make-compiler swap
    program>defs @ dup array-size 0 ?do
        i over array@ 2 pick compile-definition
    loop
    drop
; export

: write-u32 ( n file -- )
    sp@ cell + 4 2 pick write-file throw 2drop
;

: build-IdT ( compiler -- section )
    ." build-IdT" cr
    dup compiler>IdT @ table-keys
    4 swap  ( 4 bytes for id count )
    0 >r    ( R: id count )
    begin ?dup while
        swap over car strlen 1+ + swap
        r> 1+ >r
        cdr
    repeat
    dup 4 aligned-by
    section% rot + %allocate throw
    s"  IdT" over section>type 4 memcpy
    tuck section>bytes !
    ( compiler section )
    r>
    ( compiler section num-id )
    sp@ 2 pick 8 + 4 memcpy drop
    dup 12 + >r
    over compiler>IdT @ table-keys
    begin ?dup while
        dup car dup strlen 1+ r> 2dup + >r swap memcpy
        cdr
    repeat
    r> drop
    nip
;

: codegen ( compiler file -- )
    s" PLNK" 4 2 pick write-file throw
    \ compile sections
    0 2 pick build-IdT swap cons dup >r
    \ compute data size
    0 >r
    begin ?dup while
        dup car section>bytes @ 4 aligned-by 8 + r> + >r
        cdr
    repeat
    \ write data size field
    r> over write-u32
    \ Write sections
    r>
    begin ?dup while
        ." writing: \"" dup car section>type 4 typen ." \" section" cr
        dup car dup section>bytes @ 4 aligned-by 8 + 3 pick write-file throw
        cdr
    repeat
    2drop
; export

}private

s" test.pk" W/O open-file throw constant testfile

s" function main(): i32 {
block:
    return
}

function hoge(%0: i8): i32 {
root:
    return
}
"
make-string parse compile-program testfile codegen

testfile flush-file throw
