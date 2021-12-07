Language Syntax
===============

Lexical Tokens::

    id    : [A-Za-z_][A-Za-z0-9_]*
    char  : ' [^'\] '
          | '\ [0abtnvfr'\] '
    int   : [1-9][0-9]*
          | "0" [0-7]*
          | "0x" [0-9A-Fa-f]+
          | "0b" [01]+
    float : [0-9]+ . [0-9]* ((e|E) (+|-)? [0-9]+)?
    str   : " ([^"\]|\[0abtnvfr"\])+ "

Types::

    unit_type  : ()
    bool_type  : true | false
    char_type  : char
    int_type   : i8 | u8 | i16 | u16 | i32 | u32 | i64 | u64
    float_type : f32 | f64
