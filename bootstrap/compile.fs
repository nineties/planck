\ planck -
\ Copyright (C) 2021 nineties

\ This is an implementation of PlanckIR compiler
\ in PlanckForth.
\ It compiles a PlanckIR program to an object file.

\ See spec/encoding.rst

include parser.fs
include encoding.fs
include lib/table.fs

struct
    cell% field compiler>Name    ( name table: string -> ID )
    cell% field compiler>ExpT    ( array of (type, ID idx, def idx) )
    cell% field compiler>fundefs ( array of function definitions )

    \ NB: Since this script is only for bootstrapping, we allocate a buffer
    \ with enough size and don't care about reallocation of it if the size
    \ is not enough.
    byte% $100000 * field compiler>buf ( bytecode buffer )
    ptr% field compiler>pos
end-struct compiler%

struct
    cell% field tuple0
    cell% field tuple1
    cell% field tuple2
    cell% field tuple3
    cell% field tuple4
end-struct tuple%

private{

: list-length ( list -- n )
    0 >r
    begin ?dup while
        r> 1+ >r
        cdr
    repeat
    r>
;

: u32! ( n addr -- )
    sp@ cell + swap 4 memcpy drop
;

: u32@ ( addr -- n )
    @ $ffffffff and
;

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
    make-string-table over compiler>Name !
    0 make-array over compiler>ExpT !
    0 make-array over compiler>fundefs !
    dup compiler>buf over compiler>pos !
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
    tuck compiler>ExpT @ array-push
    drop
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

\ : compile-Name ( compiler -- section )
\     ." compiling \"Name\" section" cr
\     dup compiler>Name @ table-keys
\     4 swap  ( 4 bytes for id count )
\     0 >r    ( R: id count )
\     begin ?dup while
\         swap over car strlen 1+ + swap
\         r> 1+ >r
\         cdr
\     repeat
\     s" Name" swap make-section
\     r>
\     ( compiler section num-id )
\     sp@ 2 pick section>data 4 memcpy drop
\     dup 12 + >r
\     over compiler>Name @ table-keys
\     begin ?dup while
\         dup car
\             ." > add: " dup type cr
\             dup strlen 1+ r> 2dup + >r swap memcpy
\         cdr
\     repeat
\     r> drop
\     nip
\ ;
\ 
\ : compile-ExpT ( compiler -- section )
\     ." compiling \"ExpT\" section" cr
\     compiler>ExpT @
\     dup array-size
\     dup 8 * 4 + s" ExpT" swap make-section dup >r
\ 
\     \ write data field
\     section>data
\     tuck u32! 4 +  \ entry count
\     over array-size 0 ?do
\         i 2 pick array@
\         tuck
\             dup tuple1 @ swap tuple0 @ 24 lshift or
\         over u32! 4 +
\         swap tuple2 @ over u32! 4 +
\     loop
\     2drop
\     r>
\ ;

: codegen ( compiler file -- )
    >r
    %11011111 over compiler>pos @ u8! 1 over compiler>pos +!

    \ write buf to file
    dup compiler>buf over compiler>pos @ over - r> write-file throw drop
    drop exit
    not-implemented

\    s" PLNK" 4 2 pick write-file throw
\    \ compile sections
\    0 make-array
\    2 pick compile-Name over array-push
\    2 pick compile-ExpT over array-push
\    \ compute data size
\    0
\    over array-size 0 ?do
\        i 2 pick array@ section-size +
\    loop
\    \ write data size field
\    2 pick write-u32
\    \ Write sections
\    dup array-size 0 ?do
\        i over array@
\        ." writing: \"" dup section>type 4 typen ." \" section" cr
\        dup section-size 3 pick write-file throw
\    loop
\    drop 2drop
; export

}private

s" test.pk" W/O open-file throw constant testfile

s"
export function main(): i32 {
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
