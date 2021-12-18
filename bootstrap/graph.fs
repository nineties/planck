\ planck -
\ Copyright (C) 2021 nineties

\ Control-Flow Graph representation of "subset of" PlanckIR

include lib/string.fs

( node type )
0
    enum Nid        ( name )
    enum Nregister  ( idx )
    enum Nderef     ( node )
drop

struct
    int%  field insn>tag
    cell% field insn>arg0
end-struct insn%

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

: pretty-print ( node -- )
    dup insn>tag @ case
    Nid of insn>arg0 @ type endof
    Nregister of ." %" insn>arg0 @ . endof
    Nderef of ." *" insn>arg0 @ recurse endof
    not-implemented
    endcase
; export

}private

