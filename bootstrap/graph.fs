\ planck -
\ Copyright (C) 2021 nineties

\ Control-Flow Graph representation of "subset of" PlanckIR

include lib/string.fs

( node type )
0
    enum Nid        ( name )
    enum Nregister  ( idx )
    enum Nderef     ( node )
    enum Nassign    ( lhs rhs )
    enum Nreturn
    enum Nparamdecl ( reg type )

    enum Nbblock    ( name insns jump )
    enum Nfundecl   ( export name params body )
    enum Nprogram   ( defs )

    enum TyNever
    enum TyTrue
    enum TyFalse
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
drop

struct
    int%  field node>tag
    cell% field node>arg0
    cell% field node>arg1
    cell% field node>arg2
    cell% field node>arg3
    cell% field node>arg4
end-struct node%

struct
    cell% field basicblock>name  ( ID )
    cell% field basicblock>insns ( vector of instructions )
    cell% field basicblock>jump  ( the last instruction )
end-struct basicblock%

private{

: make-node0 ( tag -- node )
    node% %allocate throw
    tuck node>tag !
;

: make-node1 ( arg0 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg0 !
;

: make-node2 ( arg0 arg1 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg1 !
    tuck node>arg0 !
;

: make-node3 ( arg0 arg1 arg2 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg2 !
    tuck node>arg1 !
    tuck node>arg0 !
;

: make-node4 ( arg0 arg1 arg2 arg3 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg3 !
    tuck node>arg2 !
    tuck node>arg1 !
    tuck node>arg0 !
;

: make-node5 ( arg0 arg1 arg2 arg3 arg4 tag -- node )
    node% %allocate throw
    tuck node>tag !
    tuck node>arg4 !
    tuck node>arg3 !
    tuck node>arg2 !
    tuck node>arg1 !
    tuck node>arg0 !
;

: make-id ( c-addr -- node ) make-string Nid make-node1 ; export
: make-register ( idx -- node ) Nregister make-node1 ; export
: make-deref ( node -- node ) Nderef make-node1 ; export
: make-return ( -- node ) Nreturn make-node0 ; export
: make-assign ( lhs rhs -- node ) Nassign make-node2 ; export
: make-paramdecl ( reg type -- node ) Nparamdecl make-node2 ; export
: make-bblock ( name insns jump -- node ) Nbblock make-node3 ; export
: make-fundecl ( export name params rettype body -- node )
    Nfundecl make-node5
; export
: make-program ( defs -- node ) Nprogram make-node1 ; export

TyNever make-node0 constant never-type export
TyTrue make-node0 constant true-type export
TyFalse make-node0 constant false-type export
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

: pp-node ( node -- )
    dup node>tag @ case
    Nid of node>arg0 @ type endof
    Nregister of ." %" node>arg0 @ 10 swap print-int endof
    Nderef of ." *" node>arg0 @ recurse endof
    Nreturn of drop ." return" endof
    Nassign of
        dup node>arg0 @ recurse
        ."  = "
        node>arg1 @ recurse
    endof
    Nparamdecl of
        dup node>arg0 @ recurse
        ." : "
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
    Nfundecl of
        dup node>arg0 @ if ." export " then
        dup node>arg1 @ recurse
        dup node>arg2 @ dup array-size 0= if
            drop ." ()"
        else
            ." ("
            0 over array@ recurse
            dup array-size 1 do
                ." , " i over array@ recurse
            loop
            ." )"
            drop
        then
        ." : "
        dup node>arg3 @ recurse
        ."  {" cr
        dup node>arg4 @ dup array-size 0 do
            i over array@ recurse
        loop
        ." }" cr
    endof
    Nprogram of
        node>arg0 @ dup array-size 0 do
            i over array@ recurse cr
        loop
        drop
    endof
    TyNever of drop ." !" endof
    TyTrue of drop ." true" endof
    TyFalse of drop ." false" endof
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
    not-implemented
    endcase
; export

}private

