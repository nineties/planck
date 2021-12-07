===========
Type System
===========

PlanckIR has a **static type system** unlike most of other physical/virtual machine languages.
Thanks to the type system, it is possible to prevent bugs, such as unintentionally treating an
integer as a pointer, by type checking.

Every local variable is implicitly typed and the type checker infer its type at compile time.
Therefore the compiled bytecode does not contain any extra bit for type annotation, in other words
there is no run-time overhead. Functions, variables and constants which has global linkage is explicitly
typed and the information is used for link time type checking.

PlanckIR's type system is designed by strongly referring to Rust's type system.
It has a feature of **ownership**, **borrowing**, **reference** and **lifetimes (NLLs)**.
In addition to it the type system also has **effects**.

Every reference to a memory region is assigned to a lifetime.
Lifetimes represents the span of the code where the reference may be used. 
Effects represent operations with side effects on lifetimes, such as writing values to references
in the lifetime. Thank to reconstruction of regions and effects features such as
guarantee of memory safety, advanced code optimization, auto parallelization, etc.
will be possible.

Types
=====

Ownership
=========

Reference
=========

Lifetime
========

Effect
======

Smart Pointers
==============

