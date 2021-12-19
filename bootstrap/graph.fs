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
drop

struct
    int%  field insn>tag
    cell% field insn>arg0
    cell% field insn>arg1
end-struct insn%

struct
    cell% field basicblock>name  ( ID object )
    cell% field basicblock>insns ( vector of instructions )
    cell% field basicblock>jump  ( the last instruction )
end-struct basicblock%

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

: pp-node ( node -- )
    dup insn>tag @ case
    Nid of insn>arg0 @ type endof
    Nregister of ." %" insn>arg0 @ 10 swap print-int endof
    Nderef of ." *" insn>arg0 @ recurse endof
    Nreturn of ." return" endof
    Nassign of
        dup insn>arg0 @ recurse
        ."  = "
        insn>arg1 @ recurse
        endof
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

}private

