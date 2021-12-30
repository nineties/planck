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
    cell% field compiler>idtable ( name table: string -> ID )
    cell% field compiler>export  ( array of (type, ID idx, def idx) )
    cell% field compiler>fundefs ( array of function definitions )

    \ NB: Since this script is only for bootstrapping, we allocate a buffer
    \ with enough size and don't care about reallocation of it if the size
    \ is not enough.
    byte% $100000 * field compiler>buf ( bytecode buffer )
    ptr% field compiler>pos
end-struct compiler%

private{

: list-length ( list -- n )
    0 >r
    begin ?dup while
        r> 1+ >r
        cdr
    repeat
    r>
;

: make-compiler ( -- compiler )
    compiler% %allocate throw
    make-string-table over compiler>idtable !
    0 make-array over compiler>export !
    0 make-array over compiler>fundefs !
    dup compiler>buf over compiler>pos !
;

\ Add an id to idtable section if not exist. Returns index of the id.
: get-id ( id compiler -- n )
    swap node>arg0 @ swap compiler>idtable @
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
    tuck compiler>export @ array-push
    drop
;

: compile-function-type ( node -- type )
    0 make-array swap
    dup fundef>params @ array-size 0 ?do
        i over fundef>params @ array@
        2 pick array-push
    loop
    fundef>retty @ swap TyFunc make-node2
;

: replace-label ( table node -- table )
    dup node>tag @ case
    Nphi of
        dup node>arg1 @ ( args )
        array-size 0 ?do
            i over node>arg1 @ array@ dup tuple0 @ node>arg0 @
            3 pick table@ swap tuple0 !
        loop
        drop
    endof
    Ngoto of
        dup node>arg0 @ node>arg0 @
        ( table node idx )
        2 pick table@
        over node>arg0 !
        drop
    endof
    Nreturn of drop endof
    not-reachable
    endcase
;

: compile-function-body ( node -- basicblocks )
    make-string-table
    over fundef>blocks @ array-size 0 ?do
        i 2 pick fundef>blocks @ array@
        node>arg0 @ node>arg0 @ ( label-name )
        i swap 2 pick table!
    loop
    \ replace label of phi and branch instruction with block index
    ( node tbl )
    over fundef>blocks @ array-size 0 ?do
        i 2 pick fundef>blocks @ array@ dup >r
        node>arg1 @ tuck array-size 0 ?do
            i 2 pick array@ replace-label
        loop
        nip
        r> node>arg3 @ replace-label   ( branch insn )
    loop
    drop

    fundef>blocks @
;

: compile-fundef ( node compiler -- )
    over fundef>tag @ Nfundef <> if not-reachable then
    ." compiling function: " over fundef>name @ pp-node cr
    over fundef>export @ if
        dup compiler>fundefs @ array-size dup >r
        ." > fundef idx: " . cr
        over fundef>name @ over get-id dup >r
        ." > name idx: " . cr
        'F' r> r> 3 pick add-export
    then
    over compile-function-type >r
    over compile-function-body >r
    r> r>
    2 cells allocate throw
    tuck tuple0 !
    tuck tuple1 !
    over compiler>fundefs @ array-push
    2drop
;

: compile-definition ( def compiler -- )
    over node>tag @ case
    Nfundef of compile-fundef endof
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

: emit ( compiler w 'encoder -- compiler )
    >r over compiler>pos @ r> execute over compiler>pos +!
;

: codegen ( compiler file -- )
    >r
    %11011111 ['] encode-u8 emit
    %11111111 ['] encode-u8 emit
    3 ['] encode-uint emit  \ number of sections

    \ write ID section
    $00 ['] encode-u8 emit  \ section type
    dup compiler>idtable @ table-keys dup >r
    list-length ['] encode-uint emit
    r> begin ?dup while
        dup >r car ['] encode-str emit r> cdr
    repeat

    \ write function section
    $01 ['] encode-u8 emit \ section type
    dup compiler>fundefs @ array-size ['] encode-uint emit \ num of funcs
    dup compiler>fundefs @ array-size 0 ?do
        i over compiler>fundefs @ array@ dup >r
        tuple0 @ ['] encode-type emit           \ emit function type
        r> tuple1 @ ['] encode-basicblocks emit
    loop

    \ write export section
    $02 ['] encode-u8 emit  \ section type
    dup compiler>export @ array-size ['] encode-uint emit
    dup compiler>export @ array-size 0 ?do
        i over compiler>export @ array@ dup dup >r >r
        tuple0 @ ['] encode-uint emit    \ type of the ID
        r> tuple1 @ ['] encode-uint emit \ index of the ID
        r> tuple2 @ ['] encode-uint emit \ index of corresponding def
    loop

    \ write buf to file
    dup compiler>buf over compiler>pos @ over - r> write-file throw drop
    drop exit
; export

}private

s" test.pk" W/O open-file throw constant testfile

s"
function hoge(): u8 {
root:
    return
}

export function main(i8): i32 {
root:
    nop
    %1 = %0
    nop
    goto exit
exit:
    %2 = phi(root:%1)
    return
}

"
make-string parse compile-program testfile codegen

testfile flush-file throw
