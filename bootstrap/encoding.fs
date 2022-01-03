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
    over $ffffff u<= if %11000111 over u8! 1+ u32! 5 exit then
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
    %11001110 of not-implemented endof
    %11001111 of not-implemented endof
    %11010000 of not-implemented endof
        dup $f0 and $a0 = unless not-reachable then
        $0f and                             ( buf len )
        >r 1+ r>                            ( c-from len )
        2dup + >r                           ( c-from len R:new-buf )
        1+ dup allocate throw dup >r swap   ( c-from c-to len R:new-buf c-to )
        strncpy
        r> r> swap 0
    endcase
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
    Nuint of over node>arg0 @ over encode-uint nip nip endof
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
    dup u8@ case
    %11000000 of 1+ never-type endof
    %11000001 of 1+ bool-type endof
    %11000010 of 1+ char-type endof
    %11000011 of 1+ u8-type endof
    %11000100 of 1+ i8-type endof
    %11000101 of 1+ u16-type endof
    %11000110 of 1+ i16-type endof
    %11000111 of 1+ u32-type endof
    %11001000 of 1+ i32-type endof
    %11001001 of 1+ u64-type endof
    %11001010 of 1+ i64-type endof
    %11001011 of 1+ f32-type endof
    %11001100 of 1+ f64-type endof
    %11001101 of 1+ str-type endof
    %11011010 of
        ( function type )
        1+ recurse >r ( R: ret )
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
