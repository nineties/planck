\ planck -
\ Copyright (C) 2021 nineties

\ This is an implementation of PlanckVM compiler
\ in PlanckForth.
\ It compiles PlanckIR programs to object files
\ for the vm.

include parser.fs

struct
end-struct compiler%

0
    enum SecIdT
drop

struct
    int% field section>type
    int% field section>bytes
end-struct section%

private{

\ Construct Control-Flow Graph from an array of basic blocks
: construct-CFG ( arr -- graph )
;

: make-compiler ( -- compiler )
    compiler% %allocate throw
;

: compile-fundecl ( node compiler -- )
    ." compiling function: " over fundecl>name @ pp-node cr
    over fundecl>body construct-CFG
    not-implemented
;

: compile-definition ( def compiler -- )
    over node>tag @ case
    Nfundecl of compile-fundecl endof
        not-reachable
    endcase
;

: compile-program ( program -- obj )
    \ make-compiler swap
    \ program>defs @ dup array-size 0 ?do
    \     i over array@ 2 pick compile-definition
    \ loop
    drop 0
; export

: codegen ( obj file -- )
    s" PLNK" 4 2 pick write-file throw
    4 sp@ 4 3 pick write-file throw
    2drop
; export

}private

s" test.pk" W/O open-file throw constant testfile

s" function main(): i32 {
block:
    return
}
"
make-string parse compile-program testfile codegen

testfile flush-file throw
