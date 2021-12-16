\ planck -
\ Copyright (C) 2021 nineties

\ Abstract syntax tree of "subset of" PlanckIR 

include lib/string.fs

( node type )
0
    enum Nid    ( name )
    enum Nvar   ( idx )
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

: make-id ( c-addr -- node )
    make-string Nid make-node1
; export

: make-var ( idx -- node )
    Nvar make-node1
; export

: pretty-print ( node -- )
    dup ast>tag @ case
    Nid of ast>arg0 @ type endof
    Nvar of ." %" ast>arg0 @ . endof
    not-implemented
    endcase
; export

}private

