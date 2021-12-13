\ planck -
\ Copyright (C) 2021 nineties

\ Lexer of "subset of" PlanckIR tokens.
\ Since this lexer is used only for bootstrapping phase,
\ some token types, such as floating point literals,
\ are not necessary and not implemented.
\ See spec/syntax.rst.

include lib/string.fs

s" Too long token" exception constant TOO-LONG-TOKEN
s" Invalid token" exception constant INVALID-TOKEN

\ Token types
0
    enum Tnull
    enum Tid
    enum Tchar
    enum Tint
    enum Tfloat
    enum Tstr
drop

private{

64 constant TOKENBUF-SIZE

struct
    cell% field lexer>input ( input string )
    int%  field lexer>pos   ( current position )
    int%  field lexer>line  ( current source line no. )
    int%  field lexer>token_tag
    cell% field lexer>token_val
    int%  field lexer>token_len
    char% TOKENBUF-SIZE * field lexer>token_buf
end-struct lexer%

: make-lexer ( input -- lexer )
    lexer% %allocate throw
    tuck lexer>input !
    0 over lexer>pos !
    1 over lexer>line !
    Tnull over lexer>token_tag !
    0 over lexer>token_val !
    0 over lexer>token_len !
; export

: reset-token-buf ( lexer -- )
    0 swap lexer>token_len !
;

\ Add c and \0 to the last of token buffer
: push-token-buf ( c lexer -- e )
    dup lexer>token_len @ 1+ TOKENBUF-SIZE >= if
        2drop TOO-LONG-TOKEN
    then
    tuck lexer>token_len @
    2 pick lexer>token_buf +
    tuck c!
    1+ 0 swap c!
    1 swap lexer>token_len +!
    success
;

: reset-token ( lexer -- )
    dup reset-token-buf
    0 over lexer>token_val !
    0 over lexer>token_len !
    0 over lexer>token_buf c!
    drop
;

: set-token-tag ( tag lexer -- )
    lexer>token_tag !
;

\ value = value*w + v
: add-to-value ( v w lexer -- )
    tuck lexer>token_val @ * 2 pick + swap lexer>token_val ! drop
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
    ( 14 ) enum Cidentch   \ [A-Za-z_] - above above
    ( 15 ) enum Cother
           enum NUM-CHARACTER-GROUP
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
( 20 )     2 c, 15 c,  5 c, 15 c, 15 c, 15 c, 15 c,  4 c,
( 28 )    15 c, 15 c, 15 c, 15 c, 15 c, 15 c, 15 c, 15 c,
( 30 )     7 c,  8 c,  9 c,  9 c,  9 c,  9 c,  9 c,  9 c,
( 38 )    10 c, 10 c, 15 c, 15 c, 15 c, 15 c, 15 c, 15 c,
( 40 )    15 c, 13 c, 11 c, 13 c, 13 c, 13 c, 13 c, 14 c,
( 48 )    14 c, 14 c, 14 c, 14 c, 14 c, 14 c, 14 c, 14 c,
( 50 )    14 c, 14 c, 14 c, 14 c, 14 c, 14 c, 14 c, 14 c,
( 58 )    12 c, 14 c, 14 c, 15 c,  6 c, 15 c, 15 c, 14 c,
( 60 )    15 c, 13 c, 11 c, 13 c, 13 c, 13 c, 13 c, 14 c,
( 68 )    14 c, 14 c, 14 c, 14 c, 14 c, 14 c, 14 c, 14 c,
( 70 )    14 c, 14 c, 14 c, 14 c, 14 c, 14 c, 14 c, 14 c,
( 78 )    12 c, 14 c, 14 c, 15 c, 15 c, 15 c, 15 c,  1 c,
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
    +---+  |                      +---+      +---+
    | 17|<-+                      | 2 |      | 4 |
    +---+  |                      +---+      +---+
    spaces |            [0-9A-Fa-f] |    [01]  |
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
    error state    : 18
    finished state : 19
)

: lookahead ( lexer -- tag )
    dup lexer>pos @ swap lexer>input @ + c@ character-group
;

: current-char ( lexer -- char )
    dup lexer>pos @ swap lexer>input @ + c@
;

\ Read a character which is a part of a token
: consume ( lexer -- )
    dup current-char '\n' = if
        1 over lexer>line +!
    then
    dup
    dup current-char swap push-token-buf throw
    1 swap lexer>pos +!
;

\ Read a character which is not a part of a token
: skip ( lexer -- )
    dup current-char '\n' = if
        1 over lexer>line +!
    then
    1 swap lexer>pos +!
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
    1 2 pick lexer>token_len -!
    over push-token-buf throw
    1 swap lexer>pos +!
;

T{ s" abcdefg" make-string constant test-source -> }T
T{ test-source make-lexer constant lexer -> }T
T{ lexer current-char -> 'a' }T
T{ lexer lookahead -> Chex }T
T{ lexer lexer>pos @ -> 0 }T
T{ lexer consume -> }T
T{ lexer lexer>token_len @ -> 1 }T
T{ lexer lexer>token_buf c@ -> 'a' }T
T{ lexer lexer>token_buf 1+ c@ -> '\0' }T
T{ lexer lexer>pos @ -> 1 }T
T{ lexer free -> }T
T{ test-source free -> }T

T{ s" \\n" make-string constant test-source -> }T
T{ test-source make-lexer constant lexer -> }T
T{ lexer consume -> }T
T{ lexer consume_escaped -> }T
T{ lexer lexer>pos @ -> 2 }T
T{ lexer lexer>token_len @ -> 1 }T
T{ lexer free -> }T
T{ test-source free -> }T

create state-transition-table
(
 n    i    s    n    s    d    b    0    1    2    8    b    x    h    i    o
 u    n    p    e    q    q    a    :    :    |    9    :    :    e    d    t
 l    v    a    w    u    u    c    :    :    7    :    :    :    x    e    h
 l    l    c    l    o    o    k    :    :    :    :    :    :    :    n    e
 :    i    e    i    t    t    s    :    :    :    :    :    :    :    t    r
 :    d    s    n    e    e    l    :    :    :    :    :    :    :    c    :
 :    :    :    e    :    :    a    :    :    :    :    :    :    :    h    :
 :    :    :    :    :    :    s    :    :    :    :    :    :    :    :    :
 :    :    :    :    :    :    h    :    :    :    :    :    :    :    :    :
 :    :    :    :    :    :    :    :    :    :    :    :    :    :    :    :
)
18 , 18 , 17 , 17 ,  8 , 13 , 16 ,  1 ,  6 ,  6 ,  6 ,  7 ,  7 ,  7 ,  7 , 16 , \ from 0
19 , 18 , 19 , 19 , 19 , 19 , 19 ,  1 ,  1 ,  1 , 18 ,  4 ,  2 , 18 , 18 , 19 , \ from 1
18 , 18 , 18 , 18 , 18 , 18 , 18 ,  3 ,  3 ,  3 ,  3 ,  3 , 18 ,  3 , 18 , 18 , \ from 2
19 , 18 , 19 , 19 , 19 , 19 , 19 ,  3 ,  3 ,  3 ,  3 ,  3 , 18 ,  3 , 18 , 19 , \ from 3
18 , 18 , 18 , 18 , 18 , 18 , 18 ,  5 ,  5 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 4
19 , 18 , 19 , 19 , 19 , 19 , 19 ,  5 ,  5 , 18 , 18 , 18 , 18 , 18 , 18 , 19 , \ from 5
19 , 18 , 19 , 19 , 19 , 19 , 19 ,  6 ,  6 ,  6 ,  6 , 18 , 18 , 18 , 18 , 19 , \ from 6
19 , 18 , 19 , 19 , 19 , 19 , 19 ,  7 ,  7 ,  7 ,  7 ,  7 ,  7 ,  7 ,  7 , 19 , \ from 7
18 , 18 ,  9 , 18 , 18 ,  9 , 10 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 ,  9 , \ from 8
18 , 18 , 18 , 18 , 12 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 9
18 , 18 , 18 , 18 , 11 , 11 , 11 , 11 , 18 , 18 , 18 , 11 , 18 , 18 , 18 , 18 , \ from 10
18 , 18 , 18 , 18 , 12 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 11
19 , 18 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , \ from 12
18 , 18 , 13 , 18 , 13 , 15 , 14 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , \ from 13
18 , 18 , 18 , 18 , 13 , 13 , 13 , 13 , 18 , 18 , 18 , 13 , 18 , 18 , 18 , 18 , \ from 14
19 , 18 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , \ from 15
19 , 18 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , \ from 16
19 , 18 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 , \ from 17
18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 18
19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , \ from 19

: next-state ( c n -- n )
    NUM-CHARACTER-GROUP * + cells state-transition-table + @
;

T{ Cspaces 3 next-state -> 19 }T

\ convert hexadecimal character to digit
: hex-to-int ( c -- n )
    dup case
    '0' ':' rangeof '0' - endof
    'A' '[' rangeof 'A' - 10 + endof
    'a' '{' rangeof 'a' - 10 + endof
    not-reachable
    endcase
;

\ Skip leading spaces, Read one token, returns the
\ tag of the token.
: lex   ( lexer -- tag )
    0 \ initial state
    begin
        ." state=" dup . ." " over current-char . ." " over lookahead . cr
        case
        0 of
            dup reset-token
            dup lookahead 0 next-state
        endof
        1 of
            ( octal digit )
            Tint over set-token-tag
            dup current-char '0' - 8 2 pick add-to-value
            dup consume dup lookahead 1 next-state
        endof
        2 of dup consume dup lookahead 2 next-state endof
        3 of
            ( hexadecimal digit )
            Tint over set-token-tag
            dup current-char hex-to-int 16 2 pick add-to-value
            dup consume dup lookahead 3 next-state
        endof
        4 of not-implemented endof
        5 of not-implemented endof
        6 of not-implemented endof
        7 of not-implemented endof
        8 of not-implemented endof
        9 of not-implemented endof
        10 of not-implemented endof
        11 of not-implemented endof
        12 of not-implemented endof
        13 of not-implemented endof
        14 of not-implemented endof
        15 of not-implemented endof
        16 of not-implemented endof
        17 of dup skip dup lookahead 17 next-state endof
        18 of drop INVALID-TOKEN throw endof
        19 of
            lexer>token_tag @ exit
        endof
        drop not-reachable
        endcase
    again
; export

T{ s"    0xa12" make-string constant test-source -> }T
T{ test-source make-lexer constant lexer -> }T
T{ lexer lex -> Tint }T
T{ lexer lexer>token_val @ .s -> 2578 }T
T{ lexer free -> }T
T{ test-source free -> }T

}private

