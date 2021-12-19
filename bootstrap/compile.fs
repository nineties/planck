\ planck -
\ Copyright (C) 2021 nineties

\ This is an implementation of PlanckVM compiler
\ in PlanckForth.
\ It compiles PlanckIR programs to object files
\ for the vm.

include parser.fs

s"
function main(): i32 {
block:
    return
}
" make-string parse pp-node
