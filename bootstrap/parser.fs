\ planck -
\ Copyright (C) 2021 nineties

\ Parser of "subset of" PlanckIR
\ Since this parser is used only for bootstrapping phase,
\ some syntax element types, such as floating point literals,
\ are not necessary and not implemented.
\ See spec/syntax.rst.

include lib/string.fs
include lexer.fs
include ast.fs
