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

struct
    cell% field lexer>input ( input string )
    int%  field lexer>pos   ( current position )
    int%  field lexer>line  ( current source line no. )
    cell% field lexer>value ( value of current token )
    char% field lexer>beg   ( start addr of current token )
    char% field lexer>end   ( end addr of current token ) 
end-struct lexer%

: make-lexer ( input -- lexer )
    lexer% %allocate throw
    tuck lexer>input !
    0 over lexer>pos !
    1 over lexer>line !
; export

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

\ Read one token, skip following spaces, returns the
\ code of the token.
: lex   ( lexer -- tag e )
    \ stub
    drop Tid success
; export

}private

\ XXX: Temporary test code
T{ s" aaaaaaa" make-string constant identifier -> }T
T{ source make-lexer constant lexer -> }T
T{ lexer lex throw -> Tid }T
