\ planck -
\ Copyright (C) 2021 nineties

\ This is an implementation of PlanckVM compiler
\ in PlanckForth.
\ It compiles PlanckIR programs to object files
\ for the vm.

\ See spec/bytecode.rst

include parser.fs
include lib/table.fs

struct
    cell% field compiler>Name    ( name table: string -> ID )
    cell% field compiler>ExpT    ( list of (type, ID idx, def idx) )
    cell% field compiler>fundefs ( array of function definitions )
end-struct compiler%

struct
    char% 4 * field section>type
    u32%      field section>bytes
    byte% 0 * field section>data
end-struct section%

struct
    cell% field tuple0
    cell% field tuple1
    cell% field tuple2
    cell% field tuple3
    cell% field tuple4
end-struct tuple%

private{

\ align n1 to u-byte boundary
: aligned-by ( n1 u -- n2 )
    1- dup invert   \ ( n1 u-1 ~(u-1) )
    -rot + and
;

: section-size ( section -- n )
    section>bytes @ 4 aligned-by 8 +
;

\ Construct Control-Flow Graph from an array of basic blocks
: construct-CFG ( arr -- graph )
;

: make-compiler ( -- compiler )
    compiler% %allocate throw
    make-string-table over compiler>Name !
    0 over compiler>ExpT !
    0 make-array over compiler>fundefs !
;

\ Add an id to Name section if not exist. Returns index of the id.
: get-id ( id compiler -- n )
    swap node>arg0 @ swap compiler>Name @
    2dup ?table-in if table@ exit then
    dup table-size dup >r -rot table! r>
;

: add-export ( type id-idx def-idx compiler -- )
    >r
    3 cells allocate throw
    tuck tuple2 !
    tuck tuple1 !
    tuck tuple0 !
    r>
    tuck compiler>ExpT cons
    swap compiler>ExpT !
;

: compile-fundecl ( node compiler -- )
    ." compiling function: " over fundecl>name @ pp-node cr
    over fundecl>export @ if
        dup compiler>fundefs array-size dup >r
        ." > fundef idx: " . cr
        over fundecl>name @ over get-id dup >r
        ." > name idx: " . cr
        'F' r> r> 3 pick add-export
    then
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

: compile-Name ( compiler -- section )
    ." compiling \"Name\" section" cr
    dup compiler>Name @ table-keys
    4 swap  ( 4 bytes for id count )
    0 >r    ( R: id count )
    begin ?dup while
        swap over car strlen 1+ + swap
        r> 1+ >r
        cdr
    repeat
    dup 4 aligned-by
    section% rot + %allocate throw
    s" Name" over section>type 4 memcpy
    tuck section>bytes !
    ( compiler section )
    r>
    ( compiler section num-id )
    sp@ 2 pick section>data 4 memcpy drop
    dup 12 + >r
    over compiler>Name @ table-keys
    begin ?dup while
        dup car
            ." > add: " dup type cr
            dup strlen 1+ r> 2dup + >r swap memcpy
        cdr
    repeat
    r> drop
    nip
;

: codegen ( compiler file -- )
    s" PLNK" 4 2 pick write-file throw
    \ compile sections
    0 2 pick compile-Name swap cons dup >r
    \ compute data size
    0 >r
    begin ?dup while
        dup car section-size r> + >r
        cdr
    repeat
    \ write data size field
    r> over write-u32
    \ Write sections
    r>
    begin ?dup while
        ." writing: \"" dup car section>type 4 typen ." \" section" cr
        dup car dup section-size 3 pick write-file throw
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

export function hoge(%0: i8): i32 {
root:
    return
}
"
make-string parse compile-program testfile codegen

testfile flush-file throw
