\ planck -
\ Copyright (C) 2021 nineties

\ Control-Flow Graph representation of "subset of" PlanckIR

include lib/array.fs
include lib/string.fs

( node type )
0
    enum Nid        ( name )
    enum Nlongid     ( list of names )
    enum Nregister  ( idx )
    enum Nargument  ( idx )
    enum Nderef     ( node )

    enum Nbool      ( 0 or 1 )
    enum Nint       ( val type )
    enum Ntuple     ( values )

    enum Nnop
    enum Nmove      ( lhs rhs )
    enum Nlload     ( lhs id )
    enum Nlstore    ( id rhs )
    enum Neload     ( lhs id )
    enum Nestore    ( id rhs )
    enum Nphi       ( lhs args )
    enum Nadd
    enum Nsub
    enum Nmul
    enum Ndiv
    enum Nmod
    enum Nand
    enum Nor
    enum Nxor
    enum Neq
    enum Nne
    enum Nlt
    enum Nle

    enum Ncall
    enum Nlcall \ local call
    enum Necall \ external call

    enum Nmaketuple
    enum Ntupleat

    enum Ngoto
    enum Nreturn
    enum Niftrue    ( opd block0 block1 )
    enum Nifeq      ( opd0 opd1 block0 block1 )
    enum Nifne      ( opd0 opd1 block0 block1 )
    enum Niflt      ( opd0 opd1 block0 block1 )
    enum Nifle      ( opd0 opd1 block0 block1 )

    enum Nbblock    ( name insns jump )
    enum Nfundef    ( export name type blocks comment )
    enum Nvardef    ( export name type comment )
    enum Nimport    ( long_id )
    enum Nprogram   ( defs )

    enum TyNever
    enum TyBool
    enum TyChar
    enum TyI8
    enum TyU8
    enum TyI16
    enum TyU16
    enum TyI32
    enum TyU32
    enum TyI64
    enum TyU64
    enum TyF32
    enum TyF64
    enum TyStr
    enum TyArray ( type )
    enum TyTuple ( array of types)
    enum TyFunc  ( ret args )
drop

struct
    int%  field node>tag
    cell% field node>arg0
    cell% field node>arg1
    cell% field node>arg2
    cell% field node>arg3
    cell% field node>arg4
    cell% field node>arg5
end-struct node%

struct
    int%  field fundef>tag
    cell% field fundef>export
    cell% field fundef>name
    cell% field fundef>type
    cell% field fundef>blocks  ( array of basic blocks )
    cell% field fundef>comment
end-struct fundef%

struct
    int%  field vardef>tag
    cell% field vardef>export
    cell% field vardef>name
    cell% field vardef>type
    cell% field vardef>comment
end-struct vardef%

struct
    int%  field program>tag
    cell% field program>defs
end-struct program%

struct
    cell% field tuple0
    cell% field tuple1
    cell% field tuple2
    cell% field tuple3
    cell% field tuple4
end-struct tuple%

private{

: make-tuple1 ( arg0 -- node )
    1 cells allocate throw
    tuck tuple0 !
; export

: make-tuple2 ( arg0 arg1 -- node )
    2 cells allocate throw
    tuck tuple1 !
    tuck tuple0 !
; export

: make-tuple3 ( arg0 arg1 arg2 -- node )
    3 cells allocate throw
    tuck tuple2 !
    tuck tuple1 !
    tuck tuple0 !
; export

: make-node0 ( tag -- node )
    node% %allocate throw
    tuck node>tag !
; export

: make-node1 ( arg0 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg0 !
; export

: make-node2 ( arg0 arg1 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg1 !
    tuck node>arg0 !
; export

: make-node3 ( arg0 arg1 arg2 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg2 !
    tuck node>arg1 !
    tuck node>arg0 !
; export

: make-node4 ( arg0 arg1 arg2 arg3 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg3 !
    tuck node>arg2 !
    tuck node>arg1 !
    tuck node>arg0 !
; export

: make-node5 ( arg0 arg1 arg2 arg3 arg4 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg4 !
    tuck node>arg3 !
    tuck node>arg2 !
    tuck node>arg1 !
    tuck node>arg0 !
; export

: make-node6 ( arg0 arg1 arg2 arg3 arg4 arg5 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg5 !
    tuck node>arg4 !
    tuck node>arg3 !
    tuck node>arg2 !
    tuck node>arg1 !
    tuck node>arg0 !
; export

: make-id ( c-addr -- node ) make-string Nid make-node1 ; export
: make-register ( idx -- node ) Nregister make-node1 ; export
: make-argument ( idx -- node ) Nargument make-node1 ; export
: make-deref ( node -- node ) Nderef make-node1 ; export
: make-nop ( -- node ) Nnop make-node0 ; export
: make-goto ( label -- node ) Ngoto make-node1 ; export
: make-return ( arg -- node ) Nreturn make-node1 ; export
: make-move ( lhs rhs -- node ) Nmove make-node2 ; export
: make-lload ( lhs id -- node ) Nlload make-node2 ; export
: make-lstore ( id rhs -- node ) Nlstore make-node2 ; export
: make-eload ( lhs id -- node ) Neload make-node2 ; export
: make-estore ( id rhs -- node ) Nestore make-node2 ; export
: make-phi ( lhs args -- node ) Nphi make-node2 ; export
: make-bblock ( name phis insns jump -- node ) Nbblock make-node4 ; export
: make-fundef ( export name type blocks comment -- node )
    Nfundef make-node5
; export
: make-vardef ( export name type comment -- node )
    Nvardef make-node4
; export
: make-program ( defs -- node ) Nprogram make-node1 ; export

1 Nbool make-node1 constant true-value export
0 Nbool make-node1 constant false-value export
0 make-array Ntuple make-node1 constant unit-value export

TyNever make-node0 constant never-type export
TyBool make-node0 constant bool-type export
TyChar make-node0 constant char-type export
TyI8 make-node0 constant i8-type export
TyU8 make-node0 constant u8-type export
TyI16 make-node0 constant i16-type export
TyU16 make-node0 constant u16-type export
TyI32 make-node0 constant i32-type export
TyU32 make-node0 constant u32-type export
TyI64 make-node0 constant i64-type export
TyU64 make-node0 constant u64-type export
TyF32 make-node0 constant f32-type export
TyF64 make-node0 constant f64-type export
TyStr make-node0 constant str-type export
0 make-array TyTuple make-node1 constant unit-type export

: pp-node ( node -- )
    dup node>tag @ case
    Nid of node>arg0 @ type endof
    Nregister of ." %" node>arg0 @ 10 swap print-int endof
    Nargument of ." $" node>arg0 @ 10 swap print-int endof
    Nint of node>arg0 @ 10 swap print-int endof
    Nderef of ." *" node>arg0 @ recurse endof
    Nreturn of drop ." return" endof
    Nmove of
        dup node>arg0 @ recurse
        ."  = "
        node>arg1 @ recurse
    endof
    Nbblock of
        dup node>arg0 @ recurse ." :" cr
        dup node>arg1 @
        dup array-size 0 ?do
            ." \t" i over array@ recurse cr
        loop drop
        ." \t" dup node>arg2 @ recurse cr
        drop
    endof
    Nfundef of
        dup node>arg0 @ if ." export " then
        ." function "
        dup node>arg1 @ recurse
        dup node>arg2 @ dup array-size 0= if
            drop ." ()"
        else
            ." ("
            0 over array@ recurse
            dup array-size 1 ?do
                ." , " i over array@ recurse
            loop
            ." )"
            drop
        then
        ." : "
        dup node>arg3 @ recurse
        ."  {" cr
        dup node>arg4 @ dup array-size 0 ?do
            i over array@ recurse
        loop
        ." }" cr
        2drop
    endof
    Nprogram of
        node>arg0 @ dup array-size 0 ?do
            i over array@ recurse cr
        loop
        drop
    endof
    TyNever of drop ." !" endof
    TyBool of drop ." bool" endof
    TyChar of drop ." char" endof
    TyI8 of drop ." i8" endof
    TyU8 of drop ." u8" endof
    TyI16 of drop ." i16" endof
    TyU16 of drop ." u16" endof
    TyI32 of drop ." i32" endof
    TyU32 of drop ." u32" endof
    TyI64 of drop ." i64" endof
    TyU64 of drop ." u64" endof
    TyF32 of drop ." f32" endof
    TyF64 of drop ." f64" endof
    TyStr of drop ." str" endof
    not-implemented
    endcase
; export

}private

