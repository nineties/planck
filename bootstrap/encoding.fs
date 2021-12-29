\ planck -
\ Copyright (C) 2021 nineties

\ Bytecode encoding of PlanckIR.

\ See spec/encoding.rst

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
    over $7f u<= if
        u8! 1
    else over $ff u<= if
        %11000011 over u8! 1+ u8! 2
    else over $ffff u<= if
        %11000101 over u8! 1+ u16! 3
    else over $ffffff u<= if
        %11000111 over u8! 1+ u32! 5
    else
        not-implemented
    then then then then
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
        r> over u16! 2 +          \ bytes
        strcpy
        r> 3 +
    else dup $ffffff < if
        dup >r >r
        %11010000 over u8! 1+   \ tag
        r> over u32! 4 +          \ bytes
        strcpy
        r> 5 +
    else
        ENCODE-ERROR throw
    then then then then
; export

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
    .s not-implemented
    endcase
; export

: encode-insn ( insn buf -- n )
    over node>tag @ case
    Nnop of %00000000 over u8! 2drop 1 endof
    Ngoto of
        %10000000 over u8! 1+
        over node>arg0 @ over encode-uint 1+
        nip nip
    endof
    Nreturn of %10000001 over u8! 2drop 1 endof
    not-implemented
    endcase
;

: encode-basicblock ( block buf -- n )
    over node>tag @ Nbblock <> if not-reachable then
    over node>arg1 @ array-size over encode-uint dup >r + r>
    ( block buf n )
    2 pick node>arg1 @ array-size 0 ?do
        i 3 pick node>arg1 @ array@ 2 pick encode-insn tuck + >r + r>
    loop
    2 pick node>arg2 @ 2 pick encode-insn tuck + >r + r>
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
