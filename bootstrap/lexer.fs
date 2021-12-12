\ planck -
\ Copyright (C) 2021 nineties

\ Lexer of "subset of" PlanckIR tokens.
\ Since this lexer is used only for bootstrapping phase,
\ some token types, such as floating point literals,
\ are not necessary and not implemented.
\ See spec/syntax.rst.

include lib/string.fs

\ Token types
0
    enum Tid
    enum Tchar
    enum Tint
    enum Tfloat
    enum Tstr
drop

private{

64 constant MAX-TOKEN-LEN

struct
    cell% field lexer>input ( input string )
    int%  field lexer>pos   ( current position )
    int%  field lexer>state
    int%  field lexer>line  ( current source line no. )
    cell% field lexer>value ( value of current token )
    int%  field lexer>token_len
    char% MAX-TOKEN-LEN * field lexer>token
end-struct lexer%

: make-lexer ( input -- lexer )
    lexer% %allocate throw
    tuck lexer>input !
    0 over lexer>state !
    0 over lexer>pos !
    1 over lexer>line !
    0 over lexer>value !
    0 over lexer>token_len !
; export

: reset-token-buf ( lexer -- )
    0 swap lexer>token_len !
;

\ Character group
0
    (  0 ) enum Cnull      \ \0
    (  1 ) enum Cinvalid   \ Unusable characters
    (  2 ) enum Cspaces    \ [\t\r ]
    (  3 ) enum Cnewline
    (  4 ) enum Csquote
    (  5 ) enum Cdquote
    (  6 ) enum Cbackslash
    (  7 ) enum C0
    (  8 ) enum C1
    (  9 ) enum C2-7
    ( 10 ) enum C89
    ( 11 ) enum Cb         \ [bB]
    ( 12 ) enum Cx         \ [xX]
    ( 13 ) enum Chex       \ [A-Fa-f] - above
    ( 14 ) enum Cescapech  \ [abtnvfr] - above
    ( 15 ) enum Cidentch   \ [A-Za-z_] - above above
    ( 16 ) enum Cother
drop

\    2 3 4 5 6 7
\  -------------
\ 0:   0 @ P ` p
\ 1: ! 1 A Q a q
\ 2: " 2 B R b r
\ 3: # 3 C S c s
\ 4: $ 4 D T d t
\ 5: % 5 E U e u
\ 6: & 6 F V f v
\ 7: ' 7 G W g w
\ 8: ( 8 H X h x
\ 9: ) 9 I Y i y
\ A: * : J Z j z
\ B: + ; K [ k {
\ C: , < L \ l |
\ D: - = M ] m }
\ E: . > N ^ n ~
\ F: / ? O _ o DEL

\ a map from ascii to character group
create character-group-table
( 00 )     0 c,  1 c,  1 c,  1 c,  1 c,  1 c,  1 c,  1 c,
( 08 )     1 c,  2 c,  3 c,  1 c,  1 c,  2 c,  1 c,  1 c,
( 10 )     1 c,  1 c,  1 c,  1 c,  1 c,  1 c,  1 c,  1 c, 
( 18 )     1 c,  1 c,  1 c,  1 c,  1 c,  1 c,  1 c,  1 c, 
( 20 )     2 c, 16 c,  5 c, 16 c, 16 c, 16 c, 16 c,  4 c, 
( 28 )    16 c, 16 c, 16 c, 16 c, 16 c, 16 c, 16 c, 16 c, 
( 30 )     7 c,  8 c,  9 c,  9 c,  9 c,  9 c,  9 c,  9 c,
( 38 )    10 c, 10 c, 16 c, 16 c, 16 c, 16 c, 16 c, 16 c, 
( 40 )    16 c, 13 c, 11 c, 13 c, 13 c, 13 c, 13 c, 15 c, 
( 48 )    15 c, 15 c, 15 c, 15 c, 15 c, 15 c, 15 c, 15 c, 
( 50 )    15 c, 15 c, 15 c, 15 c, 15 c, 15 c, 15 c, 15 c, 
( 58 )    12 c, 15 c, 15 c, 16 c,  6 c, 16 c, 16 c, 15 c, 
( 60 )    16 c, 14 c, 14 c, 13 c, 13 c, 13 c, 14 c, 15 c, 
( 68 )    15 c, 15 c, 15 c, 15 c, 15 c, 15 c, 14 c, 15 c, 
( 70 )    15 c, 15 c, 14 c, 15 c, 14 c, 15 c, 14 c, 15 c, 
( 78 )    12 c, 15 c, 15 c, 16 c, 16 c, 16 c, 16 c,  1 c, 
align

: character-group ( c -- n ) character-group-table + c@ ;

T{ '\n' character-group -> Cnewline }T
T{ '\'' character-group -> Csquote }T
T{ '"' character-group -> Cdquote }T
T{ '\\' character-group -> Cbackslash }T
T{ '0' character-group -> C0 }T
T{ '1' character-group -> C1 }T
T{ '2' character-group -> C2-7 }T
T{ '7' character-group -> C2-7 }T
T{ '8' character-group -> C89 }T
T{ '9' character-group -> C89 }T
T{ 'B' character-group -> Cb }T
T{ 'x' character-group -> Cx }T
T{ 'X' character-group -> Cx }T
T{ 'a' character-group -> Cescapech }T
T{ 'b' character-group -> Cescapech }T
T{ 't' character-group -> Cescapech }T
T{ 'n' character-group -> Cescapech }T
T{ 'v' character-group -> Cescapech }T
T{ 'f' character-group -> Cescapech }T
T{ 'r' character-group -> Cescapech }T
T{ 'A' character-group -> Chex }T
T{ 'F' character-group -> Chex }T
T{ 'g' character-group -> Cidentch }T
T{ 'z' character-group -> Cidentch }T
T{ 'G' character-group -> Cidentch }T
T{ 'Z' character-group -> Cidentch }T
T{ '_' character-group -> Cidentch }T
T{ '!' character-group -> Cother }T
T{ '+' character-group -> Cother }T
T{ '?' character-group -> Cother }T
T{ '~' character-group -> Cother }T

(
    state transition diagram:
    +---+
    |@n | == accepting state
    +---+

    +---+    0      +---+
    | 0 +--+------->|@1 |--+--------+----------+
    +---+  |        +---+  |        |          |
      ^    |          ^    | [0-7]  | [xX]     | [bB]
      |    |          +----+        v          v
      +----+                      +---+      +---+
   spaces  |                      | 2 |      | 4 |
           |                      +---+      +---+
           |            [0-9A-Fa-f] |    [01]  |
           |                    +---+      +---+
           |                    |   v      |   v
           |                    | +---+    | +---+
           |                    +-|@3 |    +-|@5 |
           |                      +---+      +---+
           | [1-9]  +---+
           +------->|@6 +--+
           |        +---+  | [0-9]
           |          ^    |
           |          +----+
           |
           | [A-Za-z_]  +---+
           +----------->|@7 +--+
           |            +---+  |
           |              ^    | [A-Za-z0-9_]
           |              +----+
           |
           | '      +---+ [^\n'\\]  +---+  '      +---+
           +------->| 8 +---------->| 9 +-------->|@12|
           |        +-+-+           +---+         +---+
           |          | +---+ [0abtnvfr"'\\]  +---+ ^
           |       \\ +>| 10+---------------->| 11+-+ '
           |            +---+                 +---+
           |
           | "        +---+ "                +---+
           +-+------->| 13+----------------->|@15|
           | |        +-+-+                  +---+
           | |          |
           | +----------+
           | |[^\n"\\]  | \\
           | |          v
           | |        +---+
           | +--------+ 14|
           |          +---+
           |  [0abtnvfr"'\\]
           |
           |other  +---+
           +------>|@16|
                   +---+
    error state    : 17
    finished state : 18
)

: lookahead ( lexer -- tag )
    dup lexer>pos @ swap lexer>input @ + c@ character-group
;

: current-char ( lexer -- char )
    dup lexer>pos @ swap lexer>input @ + c@
;

\ Read a character and update lexer states
: consume ( lexer -- )
    dup current-char '\n' = if
        1 over lexer>line +!
    then
    dup current-char
    over lexer>token_len @ 2 pick lexer>token + c!
    1 over lexer>token_len +!
    1 over lexer>pos +!
    drop
;

\ Return ascii-code of corresponding escaped char
\ e.g '\n' escaped-char -> 10
: escaped-char ( n -- n )
    case
    '0' of 0 endof
    'a' of 7 endof
    'b' of 8 endof
    't' of 9 endof
    'n' of 10 endof
    'v' of 11 endof
    'f' of 12 endof
    'r' of 13 endof
    '\"' of '\"' endof
    '\'' of '\'' endof
    '\\' of '\\' endof
    drop -1
    endcase
;

\ Read a character which follows back-slack, push
\ escaped character to the token buffer
: consume_escaped ( lexer -- )
    dup current-char escaped-char
    over lexer>token_len @ 1-
    2 pick lexer>token + c!
    1 over lexer>pos +!
    drop
;

T{ s" abcdefg" make-string constant test-source -> }T
T{ test-source make-lexer constant lexer -> }T
T{ lexer current-char -> 'a' }T
T{ lexer lookahead -> Cescapech }T
T{ lexer lexer>pos @ -> 0 }T
T{ lexer consume -> }T
T{ lexer lexer>pos @ -> 1 }T

create state-transition-table
(
 n    i    s    n    s    d    b    0    1    2    8    b    x    h    e    i    o
 u    n    p    e    q    q    a    :    :    |    9    :    :    e    s    d    t
 l    v    a    w    u    u    c    :    :    7    :    :    :    x    c    e    h
 l    l    c    l    o    o    k    :    :    :    :    :    :    :    a    n    e
 :    i    e    i    t    t    s    :    :    :    :    :    :    :    p    t    r
 :    d    s    n    e    e    l    :    :    :    :    :    :    :    e    c    :
 :    :    :    e    :    :    a    :    :    :    :    :    :    :    c    h    :
 :    :    :    :    :    :    s    :    :    :    :    :    :    :    h    :    :
 :    :    :    :    :    :    h    :    :    :    :    :    :    :    :    :    :
 :    :    :    :    :    :    :    :    :    :    :    :    :    :    :    :    :
)
17 , 17 ,  0 ,  0 ,  8 , 13 , 16 ,  1 ,  6 ,  6 ,  6 ,  7 ,  7 ,  7 ,  7 ,  7 , 16 , \ from 0
18 , 17 , 18 , 18 , 18 , 18 , 18 ,  1 ,  1 ,  1 , 17 ,  4 ,  2 , 17 , 17 , 17 , 18 , \ from 1
17 , 17 , 17 , 17 , 17 , 17 , 17 ,  3 ,  3 ,  3 ,  3 ,  3 , 17 ,  3 , 17 , 17 , 17 , \ from 2
18 , 17 , 18 , 18 , 18 , 18 , 18 ,  3 ,  3 ,  3 ,  3 ,  3 , 17 ,  3 , 17 , 17 , 18 , \ from 3
17 , 17 , 17 , 17 , 17 , 17 , 17 ,  5 ,  5 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , \ from 4
18 , 17 , 18 , 18 , 18 , 18 , 18 ,  5 ,  5 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 18 , \ from 5
18 , 17 , 18 , 18 , 18 , 18 , 18 ,  6 ,  6 ,  6 ,  6 , 17 , 17 , 17 , 17 , 17 , 18 , \ from 6
18 , 17 , 18 , 18 , 18 , 18 , 18 ,  7 ,  7 ,  7 ,  7 ,  7 ,  7 ,  7 ,  7 ,  7 , 18 , \ from 7
17 , 17 ,  9 , 17 , 17 ,  9 , 10 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 , \ from 8
17 , 17 , 17 , 17 , 12 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , \ from 9
17 , 17 , 17 , 17 , 11 , 11 , 11 , 11 , 17 , 17 , 17 , 11 , 17 , 17 , 11 , 17 , 17 , \ from 10
17 , 17 , 17 , 17 , 12 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , \ from 11
18 , 17 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 12
17 , 17 , 13 , 17 , 13 , 15 , 14 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , \ from 13
17 , 17 , 17 , 17 , 13 , 13 , 13 , 13 , 17 , 17 , 17 , 13 , 17 , 17 , 13 , 17 , 17 , \ from 14
18 , 17 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 15
18 , 17 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 16
17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , 17 , \ from 17
18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 18

\ Read one token, skip following spaces, returns the
\ code of the token.
: lex   ( lexer -- tag e )
    \ stub
    drop Tid success
; export

}private

