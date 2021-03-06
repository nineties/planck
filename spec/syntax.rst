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

    keyword : "nop" | "phi" | "goto" | "return" | "export" | "import" | "fun"
            | "true" | "false" | "bool" | "char" "i8" | "u8" | "i16" | "u16"
            | "i32" | "u32" | "i64" | "u64" | "f32" | "f64" | "str" | "mod" | "if"

   The lexer ignores comments.

Long Identifier::

   long_id : id
           | id "::" long_id

Types::

    never_type : "!"
    bool_type  : "bool"
    char_type  : "char"
    int_type   : "i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "i64" | "u64"
    float_type : "f32" | "f64"
    str_type   : "str"
    tuple_type : "(" ")"
               | "(" type "," ")"
               | "(" type ("," type)+ ")"
    array_type : "[" type ";" expr "]"
    slice_type : "[" type "]"
    function_type : "(" function_params ")" "->" type
    function_params : 
                    | type ("," type)*
    
    type       : never_type
               | bool_type
               | char_type
               | int_type
               | float_type
               | tuple_type
               | array_type
               | slice_type
               | function_type
               | "(" type ")"

Instruction::

   label    : id
   register : "%" int
   arguments : "$" int

   place    : label
            | register
            | "*" place

   operand  : int ":" type
            | ("+"|"-") int ":" type
            | "true" | "false"
            | register
            | arguments
            | "(" ")"
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
              | label "(" operands ")"
              | long_id "(" operands ")"
              | "(" operands ")"
              | operand "." int

   operands  :
             | operand ( "," operand )*

   instruction : "nop"
               | place "=" expression
               | place "=" long_id
               | long_id "=" operand

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

Toplevel Items::

   function_definition : "fun" label ":" type "{" basic_block+ "}"
   basic_block : label ":"
                 phi_instruction*
                 instruction*
                 branch_instruction

   variable_definition : label ":" type

   toplevel_definition : document* "export"? function_definition
                       | document* "export"? variable_definition
                       | "import" long_id

   program : toplevel_definition*

Order of Toplevel Definitions
=============================

PlanckIR does not care about the order where top-level variables, constants,
functions, etc. are defined.
