===============
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

    comment  : "//" [^\n]* "\n"
    document : "///" [^\n]* "\n"

    keyword : "nop" | "phi" | "goto" | "return" | "export" | "fun"
            | "true" | "false" | "bool" | "char" "i8" | "u8" | "i16" | "u16"
            | "i32" | "u32" | "i64" | "u64" | "f32" | "f64" | "str" | "mod" | "if"

   The lexer ignores comments.

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

   place    : label
            | register
            | "*" place

   operand  : (type) int
            | (type) ("+"|"-") int
            | "true" | "false"
            | register
            | arguments
            | "*" place

   expression : operand
              | operand "+" operand
              | operand "-" operand
              | operand "*" operand
              | operand "/" operand
              | operand "mod" operand
              | operand "&" operand
              | operand "|" operand
              | operand "^" operand
              | operand "==" operand
              | operand "!=" operand
              | operand "<" operand
              | operand "<=" operand
              | "label" "(" operands ")"
              | "(" operands ")"
              | operand "." int

   operands  :
             | operand ( "," operand )*

   instruction : "nop"
               | place "=" expression

   phi_instruction : place "=" "phi" "(" phi_args ")"
   phi_args : phi_arg ( "," phi_arg )*
   phi_arg : label ":" place

   branch_instruction : "goto" label
                      | "return" operand
                      | "if" operand label label
                      | "if" operand "==" operand label label
                      | "if" operand "!=" operand label label
                      | "if" operand "<" operand label label
                      | "if" operand "<=" operand label label

Basic Block::

   basic_block : label ":"
                 phi_instruction*
                 instruction*
                 branch_instruction

Function::

   function_params :
                   | type ( "," type )*

   function_definition :
      "export"?
      "fun" label "(" function_params ")" ":" type
      "{" basic_block+ "}"

Program::

   toplevel_definition : document* function_definition
   program : toplevel_definition*
