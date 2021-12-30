Language Syntax
===============

Lexical Tokens::

    id     : [A-Za-z_][A-Za-z0-9_]*
    char   : "'" [^\n'\\] "'"
           | "'\\" [0abtnvfr"'\\] "'"
    int    : [1-9][0-9]*
           | "0" [0-7]*
           | "0" ("x"|"X") [0-9A-Fa-f]+
           | "0" ("b"|"B") [01]+
    float  : [0-9]+ "." [0-9]* (("e"|"E") ("+"|"-")? [0-9]+)?
    string : "\"" ([^\n"\\]|\[0abtnvfr"'\\])+ "\""
    symbol : [!#$%&()*+,-./:;<=>?@\[\\\]^_`{|}~]

    keyword : "nop" | "phi" | "goto" | "return" | "export" | "function"
            | "bool" | "char" "i8" | "u8" | "i16" | "u16" | "i32" | "u32"
            | "i64" | "u64" | "f32" | "f64" | "str"

Types::

    never_type : "!"
    bool_type  : "bool"
    char_type  : "char"
    int_type   : "i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "i64" | "u64"
    float_type : "f32" | "f64"
    str_type   : "str"
    tuple_type : "(" ")"
               | "(" type ("," type)* ")"
    array_type : "[" type ";" expr "]"
    slice_type : "[" type "]"
    
    type       : never_type
               | bool_type
               | char_type
               | int_type
               | float_type
               | tuple_type
               | array_type
               | slice_type

Instruction::

   label    : id
   register : "%" int
   arguments : "$" int
   operand  : "(" type ")" int
            | register
   place    : label
            | register
            | "*" place

   instruction : "nop"
               | place "=" place

   phi_instruction : place "=" "phi" "(" phi_args ")"
   phi_args : phi_arg ( "," phi_arg )*
   phi_arg : label ":" place

   branch_instruction : "goto" label
                      | "return"

Basic Block::

   basic_block : label ":"
                 phi_instruction*
                 instruction*
                 branch_instruction

Function::

   function_params :
                   | type ( "," function_params )*

   function_definition :
      "export"?
      "function" label "(" function_params ")" ":" type
      "{" basic_block+ "}"

Program::

   toplevel_definition : function_definition
   program : toplevel_definition*
