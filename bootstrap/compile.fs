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

variable PROGRAM
make-string-table constant IDTABLE  \ string -> ID
0 make-array constant EXPORTS       \ array of (type, ID idx, def idx)
0 make-array constant IMPORTS       \ array of ID idx
0 make-array constant FUNDEFS       \ array of function definitions
0 make-array constant VARDEFS       \ array of variable definitions
$100000 allocate throw constant CODEBUF
create CODEPOS CODEBUF ,

: list-length ( list -- n )
    0 >r
    begin ?dup while
        r> 1+ >r
        cdr
    repeat
    r>
;

\ Add an id to idtable section if not exist. Returns index of the id.
: get-id ( str -- n )
    IDTABLE
    2dup ?table-in if table@ exit then
    dup table-size dup >r -rot table! r>
;

\ list of ids to foo::bar::baz style string
: join-longid ( list -- s )
    dup cdr if
        dup cdr recurse
        s" ::" swap concat-string
        swap car swap concat-string
    else
        car
    then
;

\ convert a list like (foo bar baz f) to two strings foo::bar:baz and f
: split-longid ( list -- m f )
    dup cdr cdr if
        dup cdr recurse >r ( list m f )
        s" ::" swap concat-string
        swap car swap concat-string r>
    else dup cdr if
        dup car swap cdr car
    else
        not-reachable
    then then
;

: lookup-fundef ( name -- idx )
    0 PROGRAM @ program>defs @ array-size 0 ?do
        i PROGRAM @ program>defs @ array@
        dup node>tag @ Nfundef = if
            fundef>name @ node>arg0 @ 2 pick streq if
                unloop
                nip exit
            else
                1+
            then
        else
            drop
        then
    loop
    2drop -1
;

: lookup-vardef ( name -- idx )
    0 PROGRAM @ program>defs @ array-size 0 ?do
        i PROGRAM @ program>defs @ array@
        dup node>tag @ Nvardef = if
            vardef>name @ node>arg0 @ 2 pick streq if
                unloop
                nip exit
            else
                1+
            then
        else
            drop
        then
    loop
    not-reachable
;

: lookup-import-index ( name -- idx )
    get-id
    IMPORTS array-size 0 ?do
        i IMPORTS array@ over = if drop i unloop exit then
    loop
    not-reachable
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
    Nlload of
        dup node>arg1 @ node>arg0 @ lookup-vardef
        swap node>arg1 !
    endof
    Nlstore of
        dup node>arg0 @ node>arg0 @ lookup-vardef
        swap node>arg0 !
    endof
    drop
    endcase
;

: compile-insn ( insn -- insn )
    dup node>tag @ case
    Ncall of
        dup node>arg1 @ node>tag @ Nid = if
            dup node>arg1 @ node>arg0 @ lookup-fundef dup 0< if not-reachable then
            over node>arg0 @ swap
            2 pick node>arg2 @
            Nlcall make-node3
            nip
        else
            dup node>arg1 @ node>tag @ Nlongid = unless not-reachable then

            dup node>arg2 >r
            dup node>arg1 @ node>arg0 @ split-longid
            get-id swap lookup-import-index swap .s >r >r
            node>arg0 @ r> r> r> .s Necall make-node4
        then
    endof
        drop 0
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
        i 2 pick fundef>blocks @ array@ dup dup >r >r
        node>arg1 @ tuck array-size 0 ?do
            i 2 pick array@ replace-label
        loop nip
        r>
        node>arg2 @ tuck array-size 0 ?do
            i 2 pick array@ replace-label
        loop nip
        r> node>arg3 @ replace-label   ( branch insn )
    loop
    drop \ drop the basicblock table

    dup fundef>blocks @ array-size 0 ?do
        i over fundef>blocks @ array@ ( node block )
        dup node>arg2 @ array-size 0 ?do
            i over node>arg2 @ array@ compile-insn
            i 2 pick node>arg2 @ array!
        loop
        drop
    loop

    fundef>blocks @
;

: compile-fundef ( node -- )
    ." compiling function: " dup fundef>name @ pp-node cr
    dup fundef>export @ if
        dup fundef>comment @ >r
        FUNDEFS array-size >r
        dup fundef>name @ node>arg0 @ get-id >r
        'F' r> r> r> add-export
    then
    dup compile-function-body
    3 cells allocate throw
    2 pick fundef>name @ node>arg0 @ over tuple0 !
    2 pick fundef>type @ over tuple1 !
    tuck tuple2 !
    FUNDEFS array-push
    drop
;

: compile-vardef ( node -- )
    ." compiling variable definition: " dup vardef>name @ pp-node cr
    dup vardef>export @ if
        dup vardef>comment @ >r
        VARDEFS array-size >r
        dup vardef>name @ node>arg0 @ get-id >r
        'D' r> r> r> add-export
    then
    VARDEFS array-push
;

: compile-import ( node -- )
    node>arg0 @ node>arg0 @ join-longid get-id IMPORTS array-push
;


: compile-definition ( def -- )
    dup node>tag @ case
    Nfundef of compile-fundef endof
    Nvardef of compile-vardef endof
    Nimport of compile-import endof
        not-reachable
    endcase
;

: compile-program ( program -- )
    dup PROGRAM !
    program>defs @ dup array-size 0 ?do
        i over array@ compile-definition
    loop
    drop
; export

: emit ( w 'encoder -- )
    >r CODEPOS @ r> execute CODEPOS +!
;

: codegen ( file -- )
    >r
    %11011111 ['] encode-u8 emit
    %11111111 ['] encode-u8 emit
    5 ['] encode-uint emit  \ number of sections

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
    s" startup" lookup-fundef dup 0< if
        \ no startup code
        drop %11000000 ['] encode-u8 emit    \ none
    else
        ['] encode-uint emit
    then

    $02 ['] encode-u8 emit \ section type
    VARDEFS array-size ['] encode-uint emit
    VARDEFS array-size 0 ?do
        i VARDEFS array@ vardef>type @ ['] encode-type emit
    loop

    \ write export section
    $03 ['] encode-u8 emit  \ section type
    EXPORTS array-size ['] encode-uint emit
    EXPORTS array-size 0 ?do
        i EXPORTS array@ dup dup dup >r >r >r
        tuple0 @ ['] encode-uint emit    \ type of the ID
        r> tuple1 @ ['] encode-uint emit \ index of the ID
        r> tuple2 @ ['] encode-uint emit \ index of corresponding def
        r> tuple3 @ ['] encode-str emit  \ documentation
    loop

    \ write import section
    $04 ['] encode-u8 emit  \ section type
    IMPORTS array-size ['] encode-uint emit
    IMPORTS array-size 0 ?do
        i IMPORTS array@ ['] encode-uint emit
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
