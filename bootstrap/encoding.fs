\ planck -
\ Copyright (C) 2021 nineties

\ Bytecode encoding of PlanckIR.

\ See spec/encoding.rst

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

\ Utilities
: u8! ( u p -- ) c! ;
: u8@ ( p -- u ) c@ $ff and ;
: u16! ( u p -- )
    over 0xff and over c!
    over 8 rshift over 1+ c!
    2drop
;
: u16@ ( p -- u ) @ 0xffff and ;
: u32! ( u p -- ) ! ;
: u32@ ( p -- u ) @ ;

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

}private
