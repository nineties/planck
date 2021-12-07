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

Also the type system reconstructs **lifetimes** and **effects**.
Every reference to a memory region is assigned to a lifetime.
Lifetimes represents the span of the code where the reference may be used. 
Effects represent operations with side effects on lifetimes, such as writing values to references
in the lifetime. Thank to reconstruction of regions and effects features such as
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

