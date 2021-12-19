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
    int%  field insn>tag
    cell% field insn>arg0
    cell% field insn>arg1
end-struct insn%

struct
    cell% field basicblock>name  ( ID )
    cell% field basicblock>insns ( vector of instructions )
    cell% field basicblock>jump  ( the last instruction )
end-struct basicblock%

struct
    int%  field fundef>export   ( bool )
    cell% field fundef>name     ( ID )
    cell% field fundef>params
    cell% field fundef>rettype
    cell% field fundef>graph
end-struct fundef%

private{

: make-node0 ( tag -- node )
    insn% %allocate throw
    tuck insn>tag !
;

: make-node1 ( arg0 tag -- node )
    insn% %allocate throw
    tuck insn>tag !
    tuck insn>arg0 !
;

: make-node2 ( arg1 arg0 tag -- node )
    insn% %allocate throw
    tuck insn>tag !
    tuck insn>arg0 !
    tuck insn>arg1 !
;

: make-id ( c-addr -- node ) make-string Nid make-node1 ; export
: make-register ( idx -- node ) Nregister make-node1 ; export
: make-deref ( node -- node ) Nderef make-node1 ; export
: make-return ( -- node ) Nreturn make-node0 ; export
: make-assign ( rhs lhs -- node ) Nassign make-node2 ; export
: make-paramdecl ( type reg -- node ) Nparamdecl make-node2 ; export

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
    dup insn>tag @ case
    Nid of insn>arg0 @ type endof
    Nregister of ." %" insn>arg0 @ 10 swap print-int endof
    Nderef of ." *" insn>arg0 @ recurse endof
    Nreturn of drop ." return" endof
    Nassign of
        dup insn>arg0 @ recurse
        ."  = "
        insn>arg1 @ recurse
    endof
    Nparamdecl of
        dup insn>arg0 @ recurse
        ." : "
        insn>arg1 @ recurse
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

: pp-basic-block ( bb -- )
    dup basicblock>name @ pp-node ." :" cr
    dup basicblock>insns @
    dup array-size 0 ?do
        ." \t" i over array@ pp-node cr
    loop drop
    ." \t" dup basicblock>jump @ pp-node cr
    drop
; export

: pp-fundef ( fundef -- )
    dup fundef>export @ if ." export " then
    dup fundef>name @ pp-node
    dup fundef>params @ dup array-size 0= if
        drop ." ()"
    else
        ." ("
        0 over array@ pp-node
        dup array-size 1 do
            ." , " i over array@ pp-node
        loop
        ." )"
        drop
    then
    ." : "
    dup fundef>rettype @ pp-node
    ."  {" cr
    dup fundef>graph @ dup array-size 0 do
        i over array@ pp-basic-block
    loop
    ." }" cr
; export

}private

