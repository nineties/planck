\ planck -
\ Copyright (C) 2021 nineties

\ Control-Flow Graph representation of "subset of" PlanckIR

include lib/string.fs

( node type )
0
    enum Nid        ( name )
    enum Nregister  ( idx )
    enum Nderef     ( node )
    enum Nreturn
drop

struct
    int%  field insn>tag
    cell% field insn>arg0
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

: make-id ( c-addr -- node ) make-string Nid make-node1 ; export
: make-register ( idx -- node ) Nregister make-node1 ; export
: make-deref ( node -- node ) Nderef make-node1 ; export
: make-return ( -- node ) Nreturn make-node0 ; export

: pp-node ( node -- )
    dup insn>tag @ case
    Nid of insn>arg0 @ type endof
    Nregister of ." %" insn>arg0 @ . endof
    Nderef of ." *" insn>arg0 @ recurse endof
    Nreturn of ." return" endof
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

