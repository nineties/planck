Planck Virtual Machine (PlanckVM)
=================================

This project aims to develop a **Low Level Virtual Machine (PlanckVM)** and
an **Intermediate Representation (PlanckIR)** which have **memory safety**
and **concurrency** features.

This is basically the continuation of `PlanckForth <https://github.com/nineties/planckforth>`_,
which aims to bootstrap a high-level language processor starting from hand-written machine language
without relying on any existing programming languages or libraries.
But the specification of VM and IR are designed to be independent of PlanckForth.
So this system can be implemented in other languages and bytecodes have a portable format.

Also, this project started with the idea of what would happen if we combined
`Rust <https://www.rust-lang.org/>`_'s type system with `LLVM <https://llvm.org>`_'s concepts.
The challenge is to create a low level virtual machine with a modern static type system
which enables memory safety and advanced optimization features with zero runtime overhead.

- `Specification <spec/index.rst>`_
