===========
Type System
===========

PlanckIR has a **static type system** unlike most of other physical/virtual machine languages.
Thanks to the type system, it is possible to prevent bugs such as accidentally treating an
integer as a pointer by type checking.

Since the types of local variables are statically inferred, the compiled bytecode does not
contain any extra bit for type annotation, and there is no run-time overhead.
Functions, variables and constants which has global linkage have explicit type annotation
for link time type checking.

Also the type system reconstructs **regions** and **effects**.
Regions represent sets of possibly aliased reference values and effects represent
operations with side effects on regions, such as writing values to references.
Thank to reconstruction of regions and effects attractive features such as
guarantee of memory safety, advanced code optimization, auto parallelization, etc.
will be possible.

Types
=====

Regions
=======

Effects
=======

Ownership
=========

Smart Pointers
==============

