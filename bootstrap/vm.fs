\ planck -
\ Copyright (C) 2021 nineties

\ Interpreter of "subset of" PlanckIR object file.

include lib/array.fs
include encoding.fs

s" Type Error" exception constant TYPE-ERROR

struct
    cell% field obj>ids        ( vector of identifiers )
    cell% field obj>funcs      ( vector of functions )
    cell% field obj>vars       ( vector of variables (type, value) )
    cell% field obj>exports    ( vector of exported items )
    cell% field obj>import_ids ( vector of module ids )
    cell% field obj>startup    ( index of startup function. -1 for none )
end-struct module%

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
    cell% field expt>doc
end-struct export-item%

1024 1024 * constant STACK-SIZE
variable SP
variable BP

0 make-array constant MODULES

: current-module ( -- mod )
    MODULES array-size 1- MODULES array@
;

: push-module ( mod -- ) MODULES array-push ;
: pop-module  ( -- ) MODULES array-pop drop ;


: reverse ( xu ... x1 u -- x1 ... xu )
    case
    1 of endof
    2 of swap endof
    3 of -rot swap endof
        1- dup >r roll r> swap >r recurse r> 0
    endcase
;

: make-module ( -- mod )
    module% %allocate throw
    0 make-array over obj>ids !
    0 make-array over obj>funcs !
    0 make-array over obj>vars !
    0 make-array over obj>exports !
    0 make-array over obj>import_ids !
    -1 over obj>startup !
;

\ Since it is cumbersome to get the file size with PlanckForth's function,
\ use a arger buffer.
$2000000 constant FILE_BUFFER_SIZE

: decode-id-section ( mod buf -- mod new-buf )
    1+ decode-uint 0 ?do
        decode-str 2 pick obj>ids @ array-push
    loop
;

: decode-operand ( fun buf -- fun new-buf opd )
    dup u8@ >r 1+ r>
    dup $7f <= if u8-type Nint make-node2 exit then
    dup $8f <= if
        $0f and dup >r
        ( update number of local variables )
        1+ 2 pick fun>nlocals @ max 2 pick fun>nlocals !
        r> Nregister make-node1
        exit
    then
    dup $9f <= if $0f and Nargument make-node1 exit then
    case
    %10100000 of unit-value exit then
    %11000001 of true-value exit then
    %11000010 of false-value exit then
    %11000011 of dup u8@ >r 1+ r> u8-type Nint make-node2 exit endof
    %11000100 of dup i8@ >r 1+ r> i8-type Nint make-node2 exit endof
    %11000101 of dup u16@ >r 2 + r> u16-type Nint make-node2 exit endof
    %11000110 of dup i16@ >r 2 + r> i16-type Nint make-node2 exit endof
    %11000111 of dup u32@ >r 4 + r> u32-type Nint make-node2 exit endof
    %11001000 of dup i32@ >r 4 + r> i32-type Nint make-node2 exit endof
    endcase
    not-implemented
;

: decode-binexpr ( fun buf tag -- fun new-buf insn )
    >r
    decode-operand >r
    decode-operand >r
    decode-operand
    r> r> 3 reverse r> make-node3
;

: decode-comp-branch ( fun buf tag -- fun new-buf insn )
    >r
    decode-operand >r
    decode-operand >r
    decode-uint >r
    decode-uint
    r> r> r> 4 reverse r> make-node4
;

: decode-insn ( fun buf -- fun new-buf insn )
    dup u8@ >r 1+ r> dup case
    %00000010 of
        drop
        decode-operand >r
        decode-operand
        r> swap make-move
    endof
    %00000011 of drop Nadd decode-binexpr endof
    %00000100 of drop Nsub decode-binexpr endof
    %00000101 of drop Nmul decode-binexpr endof
    %00000110 of drop Ndiv decode-binexpr endof
    %00000111 of drop Nmod decode-binexpr endof
    %00001000 of drop Nand decode-binexpr endof
    %00001001 of drop Nor  decode-binexpr endof
    %00001010 of drop Nxor decode-binexpr endof
    %00001011 of drop Neq  decode-binexpr endof
    %00001100 of drop Nne  decode-binexpr endof
    %00001101 of drop Nlt  decode-binexpr endof
    %00001110 of drop Nle  decode-binexpr endof
    %00100000 of
        drop
        decode-operand >r   ( lhs )
        decode-uint >r      ( index of function )
        0 make-array -rot   ( array for args )
        decode-uint 0 ?do
            decode-operand 3 pick array-push
        loop rot
        r> r> 3 reverse Nlcall make-node3
    endof
    %00110000 %00111111 rangeof
        %00001111 and >r    ( R:len )
        decode-operand >r   ( R:len lhs )
        0 make-array -rot
        r> r> swap >r 0 ?do
            decode-operand 3 pick array-push
        loop rot
        r> swap Nmaketuple make-node2
    endof
    %01000000 %01001111 rangeof
        %00001111 and >r    ( R:index )
        decode-operand >r   ( R:index lhs arg )
        decode-operand >r   ( R:index lhs arg )
        r> r> swap r>
        Ntupleat make-node3
    endof
    %01010000 of
        drop
        decode-operand >r
        dup u8@ >r 1+
        0 make-array -rot r> 0 ?do
            decode-operand 3 pick array-push
        loop rot
        r> swap Nmaketuple make-node2
    endof
    %01010001 of
        drop
        decode-operand >r
        decode-operand >r
        dup u8@ >r 1+ r>
        r> r> -rot swap Ntupleat make-node3
    endof
    %01100000 of
        drop
        decode-operand >r
        decode-uint r> swap Nload make-node2
    endof
    %01100001 of
        drop
        decode-uint >r
        decode-operand r> swap Nstore make-node2
    endof
    %10000000 of drop decode-uint make-goto endof
    %10000001 of drop decode-operand make-return endof
    %10000010 of
        drop
        decode-operand >r   ( arg )
        decode-uint >r      ( ifthen block )
        decode-uint         ( ifelse block )
        r> r> 3 reverse Niftrue make-node3
    endof
    %10000011 of drop Nifeq decode-comp-branch endof
    %10000100 of drop Nifne decode-comp-branch endof
    %10000101 of drop Niflt decode-comp-branch endof
    %10000110 of drop Nifle decode-comp-branch endof
    not-implemented
    endcase
;

: decode-basicblock ( fun buf -- fun new-buf block )
    \ decode phi instructions
    0 make-array -rot
    decode-uint 0 ?do
        dup u8@ %00000001 <> if DECODE-ERROR throw then
        1+
        decode-operand >r
        0 make-array -rot
        decode-uint 0 ?do
            decode-uint >r
            decode-operand
            r> swap make-tuple2
            3 pick array-push
        loop
        rot r> swap make-phi
        3 pick array-push
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

: decode-function-section ( mod buf -- mod new-buf )
    1+ decode-uint 0 ?do
        function% %allocate throw swap
        0 2 pick fun>nlocals !
        decode-type 2 pick fun>ty !
        0 make-array -rot
        ( mod arr fun buf )
        decode-uint 0 ?do
            decode-basicblock
            i over block>index !
            3 pick array-push
        loop
        ( mod arr fun buf )
        >r \ save new-buf
        tuck fun>blocks !
        \ add new entry of function
        over obj>funcs @ array-push
        r>
    loop
    dup u8@ %11000000 = if
        1+ -1 2 pick obj>startup !
    else
        decode-uint 2 pick obj>startup !
    then
;

: decode-variable-section ( mod buf -- mod new-buf )
    1+ decode-uint 0 ?do
        2 cells allocate throw swap
        decode-type 2 pick tuple0 !
        swap 2 pick obj>vars @ array-push
    loop
;

: decode-export-section ( mod buf -- mod new-buf )
    1+ decode-uint 0 ?do
        decode-uint swap
        decode-uint swap
        decode-uint swap
        decode-str swap >r
        export-item% %allocate throw
        tuck expt>doc !
        tuck expt>def !
        tuck expt>id !
        tuck expt>type !
        over obj>exports @ array-push
        r>
    loop
;

: decode-import-section ( mod buf -- mod new-buf )
    1+ decode-uint 0 ?do
        decode-uint 2 pick obj>import_ids @ array-push
    loop
;

: load-module ( file -- module )
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

    make-module swap

    dup u8@ %11011111 <> if DECODE-ERROR throw then 1+
    dup u8@ %11111111 <> if DECODE-ERROR throw then 1+
    decode-uint 0 ?do
        dup u8@ case
        $00 of decode-id-section endof
        $01 of decode-function-section endof
        $02 of decode-variable-section endof
        $03 of decode-export-section endof
        $04 of decode-import-section endof
        not-reachable
        endcase
    loop
    drop
;

: lookup-func ( c-addr -- function )
    \ lookup id of the name
    current-module obj>ids @
    -1 over array-size 0 ?do
        ( c-addr arr -1 )
        i 2 pick array@ 3 pick streq if
            drop i leave
        then
    loop
    nip
    ( c-addr name-id )
    dup -1 = if
        ." function \"" over type ." \" is missing" cr
        1 quit
    then

    ( name name-id )
    \ lookup export table
    current-module obj>exports @ 0
    over array-size 0 ?do
        i 2 pick array@
        ( name name-id exports tup )
        dup expt>id @ 4 pick = if
            dup expt>type @ [char] F = unless DECODE-ERROR throw then
            expt>def @
            current-module obj>funcs @ array@
            nip
            leave
        else
            drop
        then
    loop
    nip nip nip
;

\ address of local variable
: localp ( index -- a-addr ) 1+ cells negate BP @ + ;

\ address of call argument
: argp ( index -- a-addr ) cells BP @ + ;

\ evaluate operand to value
: to-value ( operand -- value )
    dup node>tag @ case
    Nregister of node>arg0 @ localp @ endof
    Nargument of node>arg0 @ argp @ endof
    drop 0
    endcase
;

: check-type ( ty val -- bool )
    dup node>tag @ case
    Nbool of drop bool-type = endof
    Nint of node>arg1 @ = endof
    Ntuple of
        over node>arg0 @ array-size
        over node>arg0 @ array-size = unless TYPE-ERROR throw then
        over node>arg0 @ array-size 0 ?do
            i 2 pick node>arg0 @ array@
            i 2 pick node>arg0 @ array@
            recurse unless TYPE-ERROR throw then
        loop
        2drop true
    endof
    not-implemented
    endcase
;

: move ( lhs value -- )
    over node>tag @ case
    Nregister of
        ( intep lhs val )
        >r node>arg0 @ localp r> swap !
    endof
    not-reachable
    endcase
;

: binexpr-int ( arg0 arg1 op -- value )
    >r
    over node>tag @ case
    Nint of
        over node>arg1 @ over node>arg1 @ <> if TYPE-ERROR throw then
        over node>arg0 @ over node>arg0 @ r> execute
        nip swap node>arg1 @ Nint make-node2
    endof
    not-reachable
    endcase
;

: binexpr-logical ( arg0 arg1 op -- value )
    >r
    over node>tag @ case
    Nbool of
        over node>arg1 @ over node>arg1 @ <> if TYPE-ERROR throw then
        over node>arg0 @ over node>arg0 @ r> execute
        nip nip bool-type Nbool make-node2
    endof
    Nint of
        over node>arg1 @ over node>arg1 @ <> if TYPE-ERROR throw then
        over node>arg0 @ over node>arg0 @ r> execute
        nip swap node>arg1 @ Nint make-node2
    endof
    not-reachable
    endcase
;

: binexpr-comp ( arg0 arg1 op -- value )
    >r
    over node>tag @ case
    Nint of
        over node>arg1 @ over node>arg1 @ <> if TYPE-ERROR throw then
        over node>arg0 @ over node>arg0 @ r> execute
        nip nip bool-type Nbool make-node2
    endof
    not-reachable
    endcase
;

: binexpr ( node -- )
    dup node>arg2 @ to-value >r
    dup node>arg1 @ to-value r>
    over node>tag @ over node>tag @ <> if TYPE-ERROR throw then
    2 pick node>tag @ case
    Nadd of ['] + binexpr-int endof
    Nsub of ['] - binexpr-int endof
    Nmul of ['] * binexpr-int endof
    Ndiv of ['] / binexpr-int endof
    Nmod of ['] mod binexpr-int endof
    Nand of ['] and binexpr-logical endof
    Nor  of ['] or  binexpr-logical endof
    Nxor of ['] xor binexpr-logical endof
    Neq  of ['] = binexpr-comp endof
    Nne  of ['] <>  binexpr-comp endof
    Nlt  of ['] < binexpr-comp endof
    Nle  of ['] <= binexpr-comp endof
    not-implemented
    endcase
    ( node value )
    >r node>arg0 @ r>
    move
;

: comp-branch ( fun prev cur node -- fun prev cur )
    dup node>arg0 @ to-value >r
    dup node>arg1 @ to-value r> swap
    2 pick node>tag @ case
    Nifeq of ['] = binexpr-comp endof
    Nifne of ['] <> binexpr-comp endof
    Niflt of ['] < binexpr-comp endof
    Nifle of ['] <= binexpr-comp endof
    not-reachable
    endcase
    node>arg0 @ if node>arg2 @ else node>arg3 @ then
    3 pick fun>blocks @ array@
    rot drop
;

: call ( fun -- retvalue )
    BP @ >r \ save base pointer
    SP @ BP !

    \ type check of arguments
    dup fun>ty @ node>arg1 @ dup array-size 0 ?do
        i over array@ i argp @ check-type unless TYPE-ERROR throw then
    loop drop

    \ allocate space for local variables
    dup fun>nlocals @ cells SP -!

    ( fun prev cur )
    0 0 2 pick fun>blocks @ array@

    \ entry block must not have phi
    dup block>phis @ array-size 0<> if DECODE-ERROR throw then

    begin
        dup block>phis @ array-size 0 ?do
            i over block>phis @ array@
            ( fun prev cur phi tup )
            dup node>arg1 @ array-size 0 ?do
                i over node>arg1 @ array@
                3 pick block>index @ over tuple0 @ = if
                    \ found the corresponding rhs
                    tuple1 @ leave
                then
                drop
            loop
            to-value
            swap node>arg0 @ swap move
        loop
        dup block>insns @ array-size 0 ?do
            i over block>insns @ array@ ( insn )
            dup node>tag @ case
            Nmove of
                ( fun prev cur node )
                dup node>arg1 @ to-value >r
                node>arg0 @ r> move
            endof
            Nadd of binexpr endof
            Nsub of binexpr endof
            Nmul of binexpr endof
            Ndiv of binexpr endof
            Nmod of binexpr endof
            Nand of binexpr endof
            Nor  of binexpr endof
            Nxor of binexpr endof
            Neq  of binexpr endof
            Nne  of binexpr endof
            Nlt  of binexpr endof
            Nle  of binexpr endof
            Nlcall of
                \ allocate space for arguments
                dup node>arg2 @ array-size cells SP -!
                \ push arguments to the stack
                dup node>arg2 @ array-size 0 ?do
                    i over node>arg2 @ array@ to-value
                    SP @ i cells + !
                loop
                dup node>arg1 @ ( index of the function )
                current-module obj>funcs @ array@
                recurse \ call the function
                ( fun prev cur node retval )
                over node>arg0 @ swap move \ assign retval to lhs

                \ restore stack pointer
                node>arg2 @ array-size cells SP +!
            endof
            Nmaketuple of
                0 make-array
                over node>arg1 @ array-size 0 ?do
                    i 2 pick node>arg1 @ array@
                    to-value over array-push
                loop
                Ntuple make-node1 >r
                dup node>arg0 @ r> move
                drop
            endof
            Ntupleat of
                dup node>arg1 @ to-value
                dup node>tag @ Ntuple = unless TYPE-ERROR throw then
                node>arg0 @ over node>arg2 @ swap array@
                swap node>arg0 @ swap move
            endof
            Nload of
                dup node>arg1 @ current-module obj>vars @ array@ tuple1 @
                swap node>arg0 @ swap move
                ( fun prev cur lhs value )
            endof
            Nstore of
                dup node>arg1 @ to-value swap node>arg0 @
                current-module obj>vars @ array@
                tuck tuple0 @ over check-type unless TYPE-ERROR throw then
                swap tuple1 !
            endof
            Ngoto of
                node>arg0 @ ( index of next block )
                3 pick fun>blocks @ array@ ( next block )
                rot drop ( fun prev cur next -> fun cur next )
            endof
            Nreturn of
                node>arg0 @ to-value

                \ check type of return value
                3 pick fun>ty @ node>arg0 @ over check-type unless
                    TYPE-ERROR throw
                then

                nip nip nip unloop
                \ restore base pointer
                r> BP !
                exit
            endof
            Niftrue of
                dup node>arg0 @ to-value
                dup node>tag @ Nbool = unless not-reachable then
                node>arg0 @ if node>arg1 @ else node>arg2 @ then
                3 pick fun>blocks @ array@ ( next block )
                rot drop ( fun prev cur next -> fun cur next )
            endof
            Nifeq of comp-branch endof
            Nifne of comp-branch endof
            Niflt of comp-branch endof
            Nifle of comp-branch endof
            not-implemented
            endcase
        loop
    again
    not-implemented
;

:noname
    argc @ 2 <> if
        ." Usage: ./planck < bootstrap.fs " argv @ @ type ."  <object file>" cr
        bye
    then

    STACK-SIZE cells dup allocate throw + SP !

    1 cells argv @ + @ load-module
    push-module

    \ run startup code if it exists
    current-module obj>startup @ dup 0>= unless drop else
        current-module obj>funcs @ array@
        call \ TODO: type check
        drop
    then

    s" main" lookup-func

    \ Check type of main
    dup fun>ty @ dup node>tag @ TyFunc = unless not-reachable then
    dup node>arg0 @ i32-type = unless not-reachable then
    node>arg1 @ array-size 0= unless not-reachable then

    call    \ call main
    dup node>tag @ Nint <> if not-reachable then
    node>arg0 @

    quit
; execute
