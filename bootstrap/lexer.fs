\ planck -
\ Copyright (C) 2021 nineties

\ Lexer of "subset of" PlanckIR tokens.
\ Since this lexer is used only for bootstrapping phase,
\ some token types, such as floating point literals,
\ are not necessary and not implemented.
\ See spec/syntax.rst.

include lib/string.fs
include lib/table.fs

s" Too long token" exception constant TOO-LONG-TOKEN
s" Invalid token" exception constant INVALID-TOKEN
s" Invalid escape" exception constant INVALID-ESCAPE

\ Token types
0
    enum Tnull
    enum Tid
    enum Tchar
    enum Tint
    enum Tfloat
    enum Tstring
    enum Tsymbol
    enum Tdocument
    \ reserved words
    enum Ttrue
    enum Tfalse
    enum Tbool
    enum Tchar
    enum Ti8
    enum Tu8
    enum Ti16
    enum Tu16
    enum Ti32
    enum Tu32
    enum Ti64
    enum Tu64
    enum Tf32
    enum Tf64
    enum Tstr
    enum Tmod
    enum Tif
    enum Tnop
    enum Tphi
    enum Tgoto
    enum Treturn
    enum Texport
    enum Tfunction
drop

1024 constant TOKENBUF-SIZE

struct
    cell% field lexer>input ( input string )
    int%  field lexer>pos   ( current position )
    int%  field lexer>line  ( current source line no. )
    int%  field lexer>token_tag
    cell% field lexer>token_val
    int%  field lexer>token_len
    char% TOKENBUF-SIZE * field lexer>token_buf
end-struct lexer%

private{

make-string-table constant reserved-words
Ttrue s" true" make-string reserved-words table!
Tfalse s" false" make-string reserved-words table!
Tbool s" bool" make-string reserved-words table!
Tchar s" char" make-string reserved-words table!
Ti8 s" i8" make-string reserved-words table!
Tu8 s" u8" make-string reserved-words table!
Ti16 s" i16" make-string reserved-words table!
Tu16 s" u16" make-string reserved-words table!
Ti32 s" i32" make-string reserved-words table!
Tu32 s" u32" make-string reserved-words table!
Ti64 s" i64" make-string reserved-words table!
Tu64 s" u64" make-string reserved-words table!
Tf32 s" f32" make-string reserved-words table!
Tf64 s" f64" make-string reserved-words table!
Tstr s" str" make-string reserved-words table!
Tmod s" mod" make-string reserved-words table!
Tif s" if" make-string reserved-words table!
Tnop s" nop" make-string reserved-words table!
Tphi s" phi" make-string reserved-words table!
Tgoto s" goto" make-string reserved-words table!
Treturn s" return" make-string reserved-words table!
Texport s" export" make-string reserved-words table!
Tfunction s" function" make-string reserved-words table!

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

: set-token-tag ( tag lexer -- )
    lexer>token_tag !
;

: reset-token ( lexer -- )
    dup reset-token-buf
    Tnull over set-token-tag
    0 over lexer>token_val !
    0 over lexer>token_len !
    0 over lexer>token_buf c!
    drop
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
    drop INVALID-ESCAPE throw
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
11 , 18 , 11 , 11 , 11 , 11 , 11 , 11 , 11 , 11 , 11 , 11 , 11 , 11 , 11 , 11 , \ from 10
18 , 18 , 18 , 18 , 12 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 11
19 , 18 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , \ from 12
18 , 18 , 13 , 18 , 13 , 15 , 14 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , \ from 13
13 , 18 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , 13 , \ from 14
19 , 18 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , \ from 15
19 , 18 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , \ from 16
19 , 18 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 ,  0 , \ from 17
18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , 18 , \ from 18
19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , 19 , \ from 19

: next-state ( c n -- n )
    NUM-CHARACTER-GROUP * + cells state-transition-table + @
;

\ convert hexadecimal character to digit
: hex-to-int ( c -- n )
    dup case
    '0' ':' rangeof '0' - endof
    'A' '[' rangeof 'A' - 10 + endof
    'a' '{' rangeof 'a' - 10 + endof
    not-reachable
    endcase
;

\ Read one token
\ Skip leading spaces when skip_spaces==true.
: lex_impl   ( skip_spaces lexer -- )
    0 \ initial state
    begin
        \ ." state=" dup . ." " over current-char . ." " over lookahead . cr
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
        4 of dup consume dup lookahead 4 next-state endof
        5 of
            ( binary digit )
            Tint over set-token-tag
            dup current-char '0' - 2 2 pick add-to-value
            dup consume dup lookahead 5 next-state
        endof
        6 of
            ( decimal digit )
            Tint over set-token-tag
            dup current-char '0' - 10 2 pick add-to-value
            dup consume dup lookahead 6 next-state
        endof
        7 of
            ( identifier )
            Tid over set-token-tag
            dup consume dup lookahead 7 next-state
        endof
        8 of dup consume dup lookahead 8 next-state endof
        9 of
            ( unescaped character )
            Tchar over set-token-tag
            dup current-char 0 2 pick add-to-value
            dup consume dup lookahead 9 next-state
        endof
        10 of dup consume dup lookahead 10 next-state endof
        11 of
            ( escaped character )
            Tchar over set-token-tag
            dup current-char escaped-char 0 2 pick add-to-value
            dup consume dup lookahead 11 next-state
        endof
        12 of dup consume dup lookahead 12 next-state endof
        13 of dup consume dup lookahead 13 next-state endof
        14 of
            ( ugly code ... )
            dup consume
            dup current-char escaped-char
            over dup lexer>token_buf swap lexer>token_len @ + 1- c!
            dup skip dup lookahead 13 next-state
        endof
        15 of
            ( string literal )
            Tstring over set-token-tag
            dup consume dup lookahead 15 next-state
        endof
        16 of
            Tsymbol over set-token-tag
            dup current-char 0 2 pick add-to-value

            \ TODO:
            \ It is not better to parse comments here
            dup current-char '/' = if
                dup consume
                dup current-char '/' = if
                    dup consume
                    dup current-char '/' = if
                        dup consume
                       \ documentation
                        dup reset-token
                        Tdocument over set-token-tag
                        begin dup current-char '\n' <> while
                                dup consume
                        repeat
                        dup consume
                        19 \ goto state 19
                    else
                        \ comment
                        begin dup current-char '\n' <> while
                                dup consume
                        repeat
                        dup consume
                        0 \ goto state 0
                    then
                else
                    dup lookahead 16 next-state
                then
            else
                dup consume dup lookahead 16 next-state
            then
        endof
        17 of
            over if \ skip spaces
                dup skip dup lookahead 17 next-state
            else
                INVALID-TOKEN throw
            then
        endof
        18 of INVALID-TOKEN throw endof
        19 of
            ( accept state )
            dup lexer>token_tag @ Tid = if
                dup lexer>token_buf reserved-words 2dup ?table-in if
                    table@ over set-token-tag
                else
                    2drop
                then
            then
            2drop exit
        endof
        not-reachable
        endcase
    again
;

: lex ( lexer -- )
    true swap lex_impl
; export

: lex_nospace ( lexer -- )
    false swap lex_impl
; export


T{ s"    +123 0xabcd '\\n'" make-string constant test-source -> }T
T{ test-source make-lexer constant lexer -> }T

T{ lexer lex -> }T
T{ lexer lexer>token_tag @ -> Tsymbol }T
T{ lexer lexer>token_buf s" +" 1 strneq -> true }T
T{ lexer lexer>token_val @ -> '+' }T

T{ lexer lex -> }T
T{ lexer lexer>token_tag @ -> Tint }T
T{ lexer lexer>token_buf s" 123" 3 strneq -> true }T
T{ lexer lexer>token_val @ -> 123 }T

T{ lexer lex -> }T
T{ lexer lexer>token_tag @ -> Tint }T
T{ lexer lexer>token_buf s" 0xabcd" 6 strneq -> true }T
T{ lexer lexer>token_val @ -> 43981 }T

T{ lexer lex -> }T
T{ lexer lexer>token_tag @ -> Tchar }T
T{ lexer lexer>token_buf s" '\\n'" 4 strneq -> true }T
T{ lexer lexer>token_val @ -> '\n' }T

T{ lexer free -> }T
T{ test-source free -> }T

}private

