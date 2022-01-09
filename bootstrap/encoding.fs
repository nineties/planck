\ planck -
\ Copyright (C) 2021 nineties

\ Bytecode encoding of PlanckIR.

\ See spec/encoding.rst

include lib/array.fs
include graph.fs

s" Encode Error" exception constant ENCODE-ERROR
s" Decode Error" exception constant DECODE-ERROR

\ value types
0
    enum Vundef
    enum Vreg
    enum Vtuple
    enum Varray
    enum Vchar
    enum Vstr
    enum Vnone
    enum Vtrue
    enum Vfalse
    enum Vu8
    enum Vi8
    enum Vu16
    enum Vi16
    enum Vu32
    enum Vi32
    enum Vu64
    enum Vi64
    enum Vf32
    enum Vf64
    enum Vuser
drop

private{

: u8! ( u p -- ) c! ; export
: u8@ ( p -- u ) c@ $ff and ; export
: u16! ( u p -- )
    over 0xff and over c!
    over 8 rshift over 1+ c!
    2drop
; export
: u16@ ( p -- u ) @ 0xffff and ; export
: u32! ( u p -- ) ! ; export
: u32@ ( p -- u ) @ ; export
: i8! ( n p -- ) c! ; export
: i8@ ( p -- n ) c@ ; export
: i16! ( n p -- )
    over 0xff and over c!
    over 8 arshift over 1+ c!
    2drop
; export
: i16@ ( p -- n )
    @ 0xffff and
    dup 0x8000 and if
        0xffff invert or
    then
; export
: i32! ( n p -- ) ! ; export
: i32@ ( p -- n ) @ ; export

: encode-u8 ( u p -- n ) u8! 1 ; export
: encode-u16 ( u p -- n ) u16! 2 ; export
: encode-u32 ( u p -- n ) u32! 4 ; export

\ mapping from 1st byte of object to types
create value-encoding-table
( 0x00 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x08 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x10 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x18 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x20 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x28 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x30 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x38 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x40 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x48 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x50 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x58 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x60 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x68 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x70 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x78 ) Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c, Vu8 c,
( 0x80 ) Vreg c, Vreg c, Vreg c, Vreg c, Vreg c, Vreg c, Vreg c, Vreg c, 
( 0x88 ) Vreg c, Vreg c, Vreg c, Vreg c, Vreg c, Vreg c, Vreg c, Vreg c, 
( 0x90 ) Vtuple c, Vtuple c, Vtuple c, Vtuple c, Vtuple c, Vtuple c, Vtuple c, Vtuple c, 
( 0x98 ) Vtuple c, Vtuple c, Vtuple c, Vtuple c, Vtuple c, Vtuple c, Vtuple c, Vtuple c, 
( 0xA0 ) Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, 
( 0xA8 ) Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, 
( 0xB0 ) Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, 
( 0xB8 ) Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, Vstr c, 
( 0xC0 ) Vnone c, Vtrue c, Vfalse c, Vu8 c, Vi8 c, Vu16 c, Vi16 c, Vu32 c, 
( 0xC8 ) Vi32 c, Vu64 c, Vi64 c, Vf32 c, Vf64 c, Vchar c, Vstr c, Vstr c, 
( 0xD0 ) Vstr c, Vtuple c, Varray c, Varray c, Varray c, Vreg c, Vreg c, Vundef c, 
( 0xD8 ) Vundef c, Vundef c, Vundef c, Vundef c, Vundef c, Vundef c, Vundef c, Vuser c, 
( 0xE0 ) Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c,
( 0xE8 ) Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c,
( 0xF0 ) Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c,
( 0xF8 ) Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c, Vi8 c,

: decode-value-type ( buf -- u )
    u8@ value-encoding-table + u8@
;

\ Encode unsigned integer `u` to `buf`
\ and returns number of bytes.
: encode-uint ( u buf -- n )
    over $7f u<= if u8! 1 exit then
    over $ff u<= if %11000011 over u8! 1+ u8! 2 exit then
    over $ffff u<= if %11000101 over u8! 1+ u16! 3 exit then
    over $ffffffff u<= if %11000111 over u8! 1+ u32! 5 exit then
    not-implemented
; export

: decode-uint ( buf -- new-buf n )
    dup >r
    u8@ dup $7f <= if r> 1+ swap exit then
    dup %11000011 = if drop r> 1+ dup u8@ >r 1+ r> exit then
    dup %11000101 = if drop r> 1+ dup u16@ >r 2 + r> exit then
    dup %11000111 = if drop r> 1+ dup u32@ >r 4 + r> exit then
    not-implemented
; export

: encode-int-literal ( node buf -- n )
    over node>arg1 @ node>tag @ case
    TyU8 of
        swap node>arg0 @
        dup $ff u> if ENCODE-ERROR throw then
        swap encode-uint
    endof
    TyI8 of
        swap node>arg0 @
        dup  127 > if ENCODE-ERROR throw then
        dup -128 < if ENCODE-ERROR throw then
        swap
        %11000100 over u8! 1+ i8! 1
    endof
    TyU16 of
        swap node>arg0 @
        dup $ffff u> if ENCODE-ERROR throw then
        swap
        %11000101 over u8! 1+ u16! 3
    endof
    TyI16 of
        swap node>arg0 @
        dup  65535 > if ENCODE-ERROR throw then
        dup -65536 < if ENCODE-ERROR throw then
        swap
        %11000110 over u8! 1+ i16! 3
    endof
    TyU32 of
        swap node>arg0 @
        dup $fffffff u> if ENCODE-ERROR throw then
        swap
        %11000111 over u8! 1+ u32! 5
    endof
    TyI32 of
        swap node>arg0 @
        dup  2147483647 > if ENCODE-ERROR throw then
        dup -2147483648 < if ENCODE-ERROR throw then
        swap
        %11001000 over u8! 1+ i32! 5
    endof
    not-implemented
    endcase
;

T{ create test-buf 1024 allot -> }T

T{ 0 test-buf encode-uint -> 1 }T
T{ test-buf u8@ -> 0 }T
T{ 127 test-buf encode-uint -> 1 }T
T{ test-buf decode-value-type -> Vu8 }T
T{ test-buf u8@ -> 127 }T

T{ 128 test-buf encode-uint -> 2 }T
T{ test-buf decode-value-type -> Vu8 }T
T{ test-buf 1+ u8@ -> 128 }T

T{ 255 test-buf encode-uint -> 2 }T
T{ test-buf decode-value-type -> Vu8 }T
T{ test-buf 1+ u8@ -> 255 }T

T{ 256 test-buf encode-uint -> 3 }T
T{ test-buf decode-value-type -> Vu16 }T
T{ test-buf 1+ u16@ -> 256 }T

T{ 65535 test-buf encode-uint -> 3 }T
T{ test-buf decode-value-type -> Vu16 }T
T{ test-buf 1+ u16@ -> 65535 }T

T{ 65536 test-buf encode-uint -> 5 }T
T{ test-buf decode-value-type -> Vu32 }T
T{ test-buf 1+ u32@ -> 65536 }T

\ Encode null-terminated string to `buf`
\ and returns number of bytes.
: encode-str ( c-addr buf -- n )
    over strlen
    dup 32 < if
        dup >r $a0 or over u8! 1+ strcpy r> 1+
    else dup 256 < if
        dup >r >r
        %11001110 over u8! 1+   \ tag
        r> over u8! 1+          \ bytes
        strcpy
        r> 2 +
    else dup $ffff < if
        dup >r >r
        %11001111 over u8! 1+   \ tag
        r> over u16! 2 +        \ bytes
        strcpy
        r> 3 +
    else dup $ffffff < if
        dup >r >r
        %11010000 over u8! 1+   \ tag
        r> over u32! 4 +        \ bytes
        strcpy
        r> 5 +
    else
        ENCODE-ERROR throw
    then then then then
; export

: decode-str ( buf -- new-buf c-addr )
    dup u8@ case
    %11001110 of 1+ dup u8@ >r 1+ r> endof
    %11001111 of 1+ dup u16@ >r 2 + r> endof
    %11010000 of 1+ dup u32@ >r 4 + r> endof
        dup $e0 and $a0 = unless not-reachable then
        $1f and
        >r 1+ r> 0
    endcase
    ( start len )
    2dup + >r   ( start len R:new-buf )
    1+ dup  allocate throw dup >r swap ( start c-to size R:new-buf c-to )
    strncpy
    r> r> swap
; export

: encode-register ( reg buf -- n )
    over node>arg0 @ dup 16 < if
        %10000000 or over u8! 2drop 1
    else
        not-implemented
    then
;

: encode-argument ( arg buf -- n )
    over node>arg0 @ dup 16 < if
        %10010000 or over u8! 2drop 1
    else
        not-implemented
    then
;

: encode-operand ( opd buf -- n )
    over node>tag @ case
    Nbool of
        swap node>arg0 @ if %11000001 else %11000010 then
        swap u8! 1
    endof
    Nint of encode-int-literal endof
    Nregister of encode-register endof
    Nargument of encode-argument endof
    not-reachable
    endcase
;

: encode-type ( ty buf -- n )
    over node>tag @ case
    TyNever of %11000000 over u8! 2drop 1 endof
    TyBool  of %11000001 over u8! 2drop 1 endof
    TyChar  of %11000010 over u8! 2drop 1 endof
    TyU8    of %11000011 over u8! 2drop 1 endof
    TyI8    of %11000100 over u8! 2drop 1 endof
    TyU16   of %11000101 over u8! 2drop 1 endof
    TyI16   of %11000110 over u8! 2drop 1 endof
    TyU32   of %11000111 over u8! 2drop 1 endof
    TyI32   of %11001000 over u8! 2drop 1 endof
    TyU64   of %11001001 over u8! 2drop 1 endof
    TyI64   of %11001010 over u8! 2drop 1 endof
    TyF32   of %11001011 over u8! 2drop 1 endof
    TyF64   of %11001100 over u8! 2drop 1 endof
    TyStr   of %11001101 over u8! 2drop 1 endof
    TyTuple of
        over node>arg0 @ array-size dup 16 < if
            dup >r
            %10000000 or over u8! 1+ 1
            r> 0 ?do
                i 3 pick node>arg0 @ array@ 2 pick recurse tuck + >r + r>
            loop
            nip nip
        else dup 256 < if
            ( ty buf,  R:n)
            >r
            %11001110 over u8! 1+
            r> dup >r over u8! 1+
            2
            r> 0 ?do
                i 3 pick node>arg0 @ array@ 2 pick recurse tuck + >r + r>
            loop
            nip nip
        else
            not-reachable
        then then
    endof
    TyFunc of
        %11011010 over u8! 1+
        over node>arg0 @ over recurse dup 1+ >r + r> \ return type
        ( ty buf n )
        2 pick node>arg1 @ array-size 2 pick u8! 1+ >r 1+ r> \ number of args
        2 pick node>arg1 @ array-size 0 ?do
            i 3 pick node>arg1 @ array@ 2 pick recurse tuck + >r + r>
        loop
        nip nip
    endof
    not-implemented
    endcase
; export

: decode-type ( buf -- new-buf type )
    dup u8@ >r 1+ r> dup case
    %10000000 %10001111 rangeof
        %00001111 and
        0 make-array -rot
        0 ?do recurse 2 pick array-push loop
        swap TyTuple make-node1
    endof
    %11000000 of drop never-type endof
    %11000001 of drop bool-type endof
    %11000010 of drop char-type endof
    %11000011 of drop u8-type endof
    %11000100 of drop i8-type endof
    %11000101 of drop u16-type endof
    %11000110 of drop i16-type endof
    %11000111 of drop u32-type endof
    %11001000 of drop i32-type endof
    %11001001 of drop u64-type endof
    %11001010 of drop i64-type endof
    %11001011 of drop f32-type endof
    %11001100 of drop f64-type endof
    %11001101 of drop str-type endof
    %11001110 of
        drop
        0 make-array swap
        dup u8@ >r 1+ r> 0 ?do
            recurse 2 pick array-push
        loop
        swap TyTuple make-node1
    endof
    %11011010 of
        drop
        ( function type )
        recurse >r ( R: ret )
        0 make-array swap
        ( array buf )
        dup u8@ >r 1+ r> 0 ?do
            recurse 2 pick array-push
        loop
        swap r> swap TyFunc make-node2
    endof
    not-implemented
    endcase
; export

: encode-binexpr ( insn buf code -- n )
    over u8! 1+ 1 >r
    over node>arg0 @ over encode-operand dup r> + >r +
    over node>arg1 @ over encode-operand dup r> + >r +
    over node>arg2 @ over encode-operand r> + nip nip
;

: encode-insn ( insn buf -- n )
    over node>tag @ case
    Nnop of %00000000 over u8! 2drop 1 endof
    Nphi of
        %00000001 over u8! 1+ 1
        2 pick node>arg0 @ 2 pick encode-operand tuck + >r + r>
        2 pick node>arg1 @ array-size 2 pick encode-uint tuck + >r + r>
        2 pick node>arg1 @ array-size 0 ?do
            i 3 pick node>arg1 @ array@
            dup >r
            tuple0 @ 2 pick encode-uint tuck + >r + r>
            r> tuple1 @ 2 pick encode-operand tuck + >r + r>
        loop
        nip nip
    endof
    Nadd of %00000011 encode-binexpr endof
    Nsub of %00000100 encode-binexpr endof
    Nmul of %00000101 encode-binexpr endof
    Ndiv of %00000110 encode-binexpr endof
    Nmod of %00000111 encode-binexpr endof
    Nand of %00001000 encode-binexpr endof
    Nor  of %00001001 encode-binexpr endof
    Nxor of %00001010 encode-binexpr endof
    Neq  of %00001011 encode-binexpr endof
    Nne  of %00001100 encode-binexpr endof
    Nlt  of %00001101 encode-binexpr endof
    Nle  of %00001110 encode-binexpr endof
    Nmove of
        %00000010 over u8! 1+
        1 >r
        over node>arg0 @ over encode-operand dup r> + >r +
        over node>arg1 @ over encode-operand r> + nip nip
    endof
    Nlcall of
        %00100000 over u8! 1+ 1 >r
        over node>arg0 @ over encode-operand dup r> + >r +
        over node>arg1 @ over encode-uint dup r> + >r +
        over node>arg2 @ array-size over encode-uint dup r> + >r +
        r>
        ( insn buf n )
        2 pick node>arg2 @ array-size 0 ?do
            i 3 pick node>arg2 @ array@ 2 pick encode-operand tuck + >r + r>
        loop
        ( insn buf n )
        nip nip
    endof
    Nmaketuple of
        over node>arg1 @ array-size dup 16 < if
            %00110000 or over u8! 1+ 1
            2 pick node>arg0 @ 2 pick encode-operand tuck + >r + r>
            2 pick node>arg1 @ array-size 0 ?do
                i 3 pick node>arg1 @ array@ 2 pick encode-operand tuck + >r + r>
            loop
            nip nip
        else 256 < if
            %01010000 over u8! 1+ 1
            2 pick node>arg0 @ 2 pick encode-operand tuck + >r + r>
            2 pick node>arg1 @ array-size 2 pick u8! 1 tuck + >r + r>
            2 pick node>arg1 @ array-size 0 ?do
                i 3 pick node>arg1 @ array@ 2 pick encode-operand tuck + >r + r>
            loop
            nip nip
        else
            \ too long tuple
            not-reachable
        then then
    endof
    Ntupleat of
        over node>arg2 @ dup 16 < if
            %01000000 or over u8! 1+ 1
            2 pick node>arg0 @ 2 pick encode-operand tuck + >r + r>
            2 pick node>arg1 @ 2 pick encode-operand +
            nip nip
        else 256 < if
            %01010001 over u8! 1+ 1
            2 pick node>arg0 @ 2 pick encode-operand tuck + >r + r>
            2 pick node>arg1 @ 2 pick encode-operand tuck + >r + r>
            2 pick node>arg2 @ 2 pick u8! 1+
            nip nip
        else
            \ too long tuple
            not-reachable
        then then
    endof
    Ngoto of
        %10000000 over u8! 1+
        over node>arg0 @ over encode-uint 1+
        nip nip
    endof
    Nreturn of
        %10000001 over u8! 1+
        over node>arg0 @ over encode-operand 1+
        nip nip
    endof
    Niftrue of
        %10000010 over u8! 1+ 1 >r
        over node>arg0 @ over encode-operand dup r> + >r +
        over node>arg1 @ over encode-uint dup r> + >r +
        over node>arg2 @ over encode-uint r> + nip nip
    endof
    Nifeq of
        %10000011 over u8! 1+ 1 >r
        over node>arg0 @ over encode-operand dup r> + >r +
        over node>arg1 @ over encode-operand dup r> + >r +
        over node>arg2 @ over encode-uint dup r> + >r +
        over node>arg3 @ over encode-uint r> + nip nip
    endof
    Nifne of
        %10000100 over u8! 1+ 1 >r
        over node>arg0 @ over encode-operand dup r> + >r +
        over node>arg1 @ over encode-operand dup r> + >r +
        over node>arg2 @ over encode-uint dup r> + >r +
        over node>arg3 @ over encode-uint r> + nip nip
    endof
    Niflt of
        %10000101 over u8! 1+ 1 >r
        over node>arg0 @ over encode-operand dup r> + >r +
        over node>arg1 @ over encode-operand dup r> + >r +
        over node>arg2 @ over encode-uint dup r> + >r +
        over node>arg3 @ over encode-uint r> + nip nip
    endof
    Nifle of
        %10000110 over u8! 1+ 1 >r
        over node>arg0 @ over encode-operand dup r> + >r +
        over node>arg1 @ over encode-operand dup r> + >r +
        over node>arg2 @ over encode-uint dup r> + >r +
        over node>arg3 @ over encode-uint r> + nip nip
    endof
    not-implemented
    endcase
;

: encode-basicblock ( block buf -- n )
    over node>tag @ Nbblock <> if not-reachable then

    0
    \ write phi instructions
    2 pick node>arg1 @ array-size 2 pick encode-uint tuck + >r + r>
    ( block buf n )
    2 pick node>arg1 @ array-size 0 ?do
        i 3 pick node>arg1 @ array@ 2 pick encode-insn tuck + >r + r>
    loop

    \ write non-branch instructions
    2 pick node>arg2 @ array-size 2 pick encode-uint tuck + >r + r>
    ( block buf n )
    2 pick node>arg2 @ array-size 0 ?do
        i 3 pick node>arg2 @ array@ 2 pick encode-insn tuck + >r + r>
    loop

    \ write branch instruction
    2 pick node>arg3 @ 2 pick encode-insn tuck + >r + r>
    nip nip
;

: encode-basicblocks ( blocks buf -- n )
    over array-size over encode-uint dup >r + r>
    2 pick array-size 0 ?do
        ( blocks buf n )
        ." compiling basic block: " i . cr
        i 3 pick array@ 2 pick encode-basicblock tuck + >r + r>
    loop
    nip nip
; export

}private
