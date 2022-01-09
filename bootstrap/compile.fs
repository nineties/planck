\ planck -
\ Copyright (C) 2021 nineties

\ This is an implementation of PlanckIR compiler
\ in PlanckForth.
\ It compiles a PlanckIR program to an object file.

\ See spec/encoding.rst

include parser.fs
include encoding.fs
include lib/table.fs

private{

: list-length ( list -- n )
    0 >r
    begin ?dup while
        r> 1+ >r
        cdr
    repeat
    r>
;

struct
    \ NB: Since this script is only for bootstrapping, we allocate a buffer
    \ with enough size and don't care about reallocation of it if the size
    \ is not enough.
    byte% $100000 * field compiler>buf ( bytecode buffer )
    ptr% field compiler>pos
end-struct compiler%

: make-compiler ( -- compiler )
    compiler% %allocate throw
    dup compiler>buf over compiler>pos !
;

variable PROGRAM
make-string-table constant IDTABLE  \ string -> ID
0 make-array constant EXPORTS       \ array of (type, ID idx, def idx)
0 make-array constant FUNDEFS       \ array of function definitions
0 make-array constant VARDEFS       \ array of variable definitions
$100000 allocate throw constant CODEBUF
create CODEPOS CODEBUF ,

make-compiler constant COMPILER export

\ Add an id to idtable section if not exist. Returns index of the id.
: get-id ( id -- n )
    node>arg0 @ IDTABLE
    2dup ?table-in if table@ exit then
    dup table-size dup >r -rot table! r>
;

: add-export ( type id-idx def-idx comment -- )
    4 cells allocate throw
    tuck tuple3 !
    tuck tuple2 !
    tuck tuple1 !
    tuck tuple0 !
    EXPORTS array-push
;

: replace-label-impl ( table node idx -- table node )
    cells over node>arg0 + dup >r @ node>arg0 @ 2 pick table@ r> !
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
    Ngoto of 0 replace-label-impl drop endof
    Nreturn of drop endof
    Niftrue of 1 replace-label-impl 2 replace-label-impl drop endof
    Nifeq of 2 replace-label-impl 3 replace-label-impl drop endof
    Nifne of 2 replace-label-impl 3 replace-label-impl drop endof
    Niflt of 2 replace-label-impl 3 replace-label-impl drop endof
    Nifle of 2 replace-label-impl 3 replace-label-impl drop endof
    not-reachable
    endcase
;

: lookup-fundef ( compiler name -- idx )
    0 PROGRAM @ program>defs @ array-size 0 ?do
        i PROGRAM @ program>defs @ array@
        dup node>tag @ Nfundef = if
            fundef>name @ node>arg0 @ 2 pick streq if
                unloop
                nip nip exit
            else
                1+
            then
        else
            drop
        then
    loop
;

: compile-insn ( compiler insn -- insn )
    dup node>tag @ case
    Ncall of
        over over node>arg1 @ node>arg0 @ lookup-fundef
        over node>arg0 @ swap
        2 pick node>arg2 @
        Nlcall make-node3
        nip nip
    endof
        drop nip 0
    endcase
;

: compile-function-body ( compiler node -- compiler basicblocks )
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
    drop \ drop the basicblock table

    dup fundef>blocks @ array-size 0 ?do
        i over fundef>blocks @ array@ ( compiler node block )
        dup node>arg2 @ array-size 0 ?do
            i over node>arg2 @ array@
            3 pick swap compile-insn
            i 2 pick node>arg2 @ array!
        loop
        drop
    loop

    fundef>blocks @
;

: compile-fundef ( node compiler -- )
    ." compiling function: " over fundef>name @ pp-node cr
    over fundef>export @ if
        over fundef>comment @ >r
        FUNDEFS array-size >r
        over fundef>name @ get-id >r
        'F' r> r> r> add-export
    then
    over compile-function-body
    3 cells allocate throw
    3 pick fundef>name @ node>arg0 @ over tuple0 !
    3 pick fundef>type @ over tuple1 !
    tuck tuple2 !
    FUNDEFS array-push
    2drop
;

: compile-vardef ( node compiler -- )
    ." compiling variable definition: " over vardef>name @ pp-node cr
    over vardef>export @ if
        over vardef>comment @ >r
        VARDEFS array-size >r
        over vardef>name @ get-id >r
        'D' r> r> r> 4 pick add-export
    then
    ." done" cr
    2drop
;

: compile-definition ( def compiler -- )
    over node>tag @ case
    Nfundef of compile-fundef endof
    Nvardef of compile-vardef endof
        not-reachable
    endcase
;

: compile-program ( program -- )
    COMPILER swap
    dup PROGRAM !
    program>defs @ dup array-size 0 ?do
        i over array@ 2 pick compile-definition
    loop
    2drop
; export

: emit ( w 'encoder -- )
    >r CODEPOS @ r> execute CODEPOS +!
;

: codegen ( file -- )
    >r
    %11011111 ['] encode-u8 emit
    %11111111 ['] encode-u8 emit
    3 ['] encode-uint emit  \ number of sections

    \ write ID section
    $00 ['] encode-u8 emit  \ section type
    IDTABLE table-keys dup >r
    list-length ['] encode-uint emit
    r> begin ?dup while
        dup >r car ['] encode-str emit r> cdr
    repeat

    \ write function section
    $01 ['] encode-u8 emit \ section type
    FUNDEFS array-size ['] encode-uint emit \ num of funcs
    FUNDEFS array-size 0 ?do
        i FUNDEFS array@ dup >r
        tuple1 @ ['] encode-type emit           \ emit function type
        r> tuple2 @ ['] encode-basicblocks emit
    loop

    \ write export section
    $02 ['] encode-u8 emit  \ section type
    EXPORTS array-size ['] encode-uint emit
    EXPORTS array-size 0 ?do
        i EXPORTS array@ dup dup dup >r >r >r
        tuple0 @ ['] encode-uint emit    \ type of the ID
        r> tuple1 @ ['] encode-uint emit \ index of the ID
        r> tuple2 @ ['] encode-uint emit \ index of corresponding def
        r> tuple3 @ ['] encode-str emit  \ documentation
    loop

    \ write buf to file
    CODEBUF CODEPOS @ over - r> write-file throw drop
    drop exit
; export

}private

$2000000 constant SOURCE_BUFFER_SIZE

:noname
    argc @ 3 <> if
        ." Usage: ./planck < bootstrap.fs " argv @ @ type ."  <input> <output>" cr
        1 quit
    then
    \ read input
    argv @ 1 cells + @ ( input )
    R/O open-file throw
    SOURCE_BUFFER_SIZE allocate throw dup >r
    SOURCE_BUFFER_SIZE
    2 pick read-file throw
    dup SOURCE_BUFFER_SIZE >= if
        ." The size of source buffer is not enough" cr
        1 quit
    then
    drop
    close-file throw

    \ compile program
    r> parse compile-program

    \ wrtie to output
    argv @ 2 cells + @ ( output )
    W/O open-file throw dup >r
    codegen
    r> flush-file throw
; execute
