\ planck -
\ Copyright (C) 2021 nineties

\ Interpreter of "subset of" PlanckIR object file.

include lib/array.fs
include encoding.fs

struct
    cell% field obj>ids     ( vector of identifiers )
    cell% field obj>funcs   ( vector of functions )
    cell% field obj>exports ( vector of exported items )
end-struct object-file%

struct
    cell% field block>index
    cell% field block>phis
    cell% field block>insns
end-struct basicblock%

struct
    cell% field fun>ty
    cell% field fun>blocks
    cell% field fun>nlocals ( number of local variables )
end-struct function%

struct
    cell% field expt>id
    cell% field expt>type
    cell% field expt>def
end-struct export-item%

struct
    cell% field interp>stack
    cell% field interp>sp
    cell% field interp>obj
end-struct interpreter%

1024 1024 * constant STACK-SIZE

: make-object-file ( -- obj )
    object-file% %allocate throw
    0 make-array over obj>ids !
    0 make-array over obj>funcs !
    0 make-array over obj>exports !
;

\ Since it is cumbersome to get the file size with PlanckForth's function,
\ use a arger buffer.
$2000000 constant FILE_BUFFER_SIZE

: decode-id-section ( obj buf -- obj new-buf )
    1+ decode-uint 0 ?do
        decode-str 2 pick obj>ids @ array-push
    loop
;

: decode-operand ( fun buf -- fun new-buf opd )
    dup u8@ >r 1+ r>
    dup $7f <= if Nuint make-node1 exit then
    not-implemented
;

: decode-insn ( fun buf -- fun new-buf insn )
    dup u8@ >r 1+ r> case
    %10000001 of decode-operand make-return endof
    endcase
;

: decode-basicblock ( fun buf -- fun new-buf block )
    \ decode phi instructions
    0 make-array -rot
    decode-uint 0 ?do
        dup u8@ %00000001 <> if DECODE-ERROR throw then
        not-implemented
    loop
    \ decode normal and branch instructions
    0 make-array -rot
    decode-uint 0 ?do
        decode-insn 3 pick array-push
    loop
    decode-insn 3 pick array-push
    ( phis insns fun new-buf )
    >r >r ( save new-buf and fun )
    basicblock% %allocate throw
    tuck block>insns !
    tuck block>phis !
    r> swap r> swap
;

: decode-function-section ( obj buf -- obj new-buf )
    1+ decode-uint 0 ?do
        function% %allocate throw swap
        0 2 pick fun>nlocals !
        decode-type 2 pick fun>ty !
        0 make-array -rot
        ( obj arr fun buf )
        decode-uint 0 ?do
            decode-basicblock
            i over block>index !
            3 pick array-push
        loop
        ( obj arr fun buf )
        >r \ save new-buf
        tuck fun>blocks !
        \ add new entry of function
        over obj>funcs @ array-push
        r>
    loop
;

: decode-export-section ( obj buf -- obj new-buf )
    1+ decode-uint 0 ?do
        decode-uint swap
        decode-uint swap
        decode-uint swap >r
        export-item% %allocate throw
        tuck expt>def !
        tuck expt>id !
        tuck expt>type !
        over obj>exports @ array-push
        r>
    loop
;

: load-object-file ( file -- object )
    \ Read file content
    R/O open-file throw
    FILE_BUFFER_SIZE allocate throw dup >r
    FILE_BUFFER_SIZE
    2 pick read-file throw
    dup FILE_BUFFER_SIZE >= if
        ." The size of file buffer is not enough" cr
        1 quit
    then
    drop
    close-file throw
    r> ( buf )

    make-object-file swap

    dup u8@ %11011111 <> if DECODE-ERROR throw then 1+
    dup u8@ %11111111 <> if DECODE-ERROR throw then 1+
    decode-uint 0 ?do
        dup u8@ case
        $00 of decode-id-section endof
        $01 of decode-function-section endof
        $02 of decode-export-section endof
        not-reachable
        endcase
    loop
    drop
;

: lookup-func ( interp c-addr -- function )
    \ lookup id of the name
    over interp>obj @ obj>ids @
    -1 over array-size 0 ?do
        ( c-addr arr -1 )
        i 2 pick array@ 3 pick streq if
            drop i leave
        then
    loop
    nip
    ( interp c-addr name-id )
    dup -1 = if
        ." function \"" over type ." \" is missing" cr
        1 quit
    then

    ( interp name name-id )
    \ lookup export table
    2 pick interp>obj @ obj>exports @ 0
    over array-size 0 ?do
        i 2 pick array@
        ( interp name name-id exports tup )
        dup expt>id @ 4 pick = if
            dup expt>type @ [char] F = unless DECODE-ERROR throw then
            expt>def @
            5 pick interp>obj @ obj>funcs @ array@
            nip
            leave
        else
            drop
        then
    loop
    nip nip nip nip
;

: call ( interp fun -- interp retvalue )
    \ allocate space for local variables
    dup fun>nlocals @ cells over interp>sp -!

    ( interp fun prev cur )
    0 0 2 pick fun>blocks @ array@

    \ entry block must not have phi
    dup block>phis @ array-size 0<> if DECODE-ERROR throw then

    begin
        dup block>phis @ array-size 0 ?do
            not-implemented
        loop
        dup block>insns @ array-size 0 ?do
            i over block>insns @ array@ ( insn )
            dup node>tag @ case
            Nreturn of
                node>arg0 @
                nip nip nip
                unloop exit
            endof
            not-implemented
            endcase
        loop
        not-implemented
    again
    not-implemented
;

: interpret ( obj -- exit-code )
    interpreter% %allocate throw
    STACK-SIZE cells allocate throw over interp>stack !
    dup interp>stack STACK-SIZE cells + over interp>sp !
    tuck interp>obj !
    dup s" main" lookup-func
    ( interp main )
    call
    nip
    dup node>tag @ Nuint <> if not-reachable then
    node>arg0 @
;

:noname
    argc @ 2 <> if
        ." Usage: ./planck < bootstrap.fs " argv @ @ type ."  <object file>" cr
        bye
    then 
    1 cells argv @ + @ ( object file )
    load-object-file
    interpret quit
; execute