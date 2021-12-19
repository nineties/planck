\ planck -
\ Copyright (C) 2021 nineties

\ This is an implementation of PlanckVM compiler
\ in PlanckForth.
\ It compiles PlanckIR programs to object files
\ for the vm.

include parser.fs

struct
end-struct compiler%

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

: compile-program ( program .. )
    make-compiler swap
    program>defs @ dup array-size 0 ?do
        i over array@ 2 pick compile-definition
    loop
    2drop
; export

}private

s" function main(): i32 {
block:
    return
}
"
make-string parse compile-program
