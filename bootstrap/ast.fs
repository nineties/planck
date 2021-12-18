\ planck -
\ Copyright (C) 2021 nineties

\ Abstract syntax tree of "subset of" PlanckIR 

include lib/string.fs

( node type )
0
    enum Nid        ( name )
    enum Nregister  ( idx )
    enum Nderef     ( node )
drop

struct
    int%  field ast>tag
    cell% field ast>arg0
end-struct ast%

private{

: make-node0 ( tag -- node )
    ast% %allocate throw
    tuck ast>tag !
;

: make-node1 ( arg0 tag -- node )
    ast% %allocate throw
    tuck ast>tag !
    tuck ast>arg0 !
;

: make-id ( c-addr -- node ) make-string Nid make-node1 ; export
: make-register ( idx -- node ) Nregister make-node1 ; export
: make-deref ( node -- node ) Nderef make-node1 ; export

: pretty-print ( node -- )
    dup ast>tag @ case
    Nid of ast>arg0 @ type endof
    Nregister of ." %" ast>arg0 @ . endof
    Nderef of ." *" ast>arg0 @ recurse endof
    not-implemented
    endcase
; export

}private

