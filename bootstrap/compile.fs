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
    cell% field compiler>IdTable ( name table: string -> ID )
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

\ Construct Control-Flow Graph from an array of basic blocks
: construct-CFG ( arr -- graph )
;

: make-compiler ( -- compiler )
    compiler% %allocate throw
    make-string-table over compiler>IdTable !
    0 make-array over compiler>ExpT !
    0 make-array over compiler>fundefs !
    dup compiler>buf over compiler>pos !
;

\ Add an id to IdTable section if not exist. Returns index of the id.
: get-id ( id compiler -- n )
    swap node>arg0 @ swap compiler>IdTable @
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

: emit ( compiler w 'encoder -- compiler )
    >r over compiler>pos @ r> execute over compiler>pos +!
;

: codegen ( compiler file -- )
    >r
    %11011111 ['] encode-u8 emit
    %11111111 ['] encode-u8 emit
    1 ['] encode-uint emit

    \ write ID section
    0 ['] encode-uint emit  \ section type
    dup compiler>IdTable @ table-keys dup >r
    list-length ['] encode-uint emit
    r> begin ?dup while
        dup >r car ['] encode-str emit r> cdr
    repeat

    \ write buf to file
    dup compiler>buf over compiler>pos @ over - r> write-file throw drop
    drop exit
    not-implemented

\    \ compile sections
\    2 pick compile-IdTable over array-push
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

export function hoge(%0: i8): i32 {
root:
    return
}
"
make-string parse .s compile-program .s testfile .s codegen

testfile flush-file throw
