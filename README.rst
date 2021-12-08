Planck Virtual Machine (PlanckVM)
=================================

This project aims to develop a **Low Level Virtual Machine (PlanckVM)** and
an **Intermediate Representation (PlanckIR)** which have **memory safety**
and **concurrency** features.

This project started with the idea of what would happen if we combined
`Rust <https://www.rust-lang.org/>`_'s type system with `LLVM <https://llvm.org>`_'s concepts.
The challenge is to create a low level virtual machine with a modern static type system
which enables memory safety and advanced optimization features with zero runtime overhead.

If I dare to write down the concerns of this project, difficulty of developing a language processor
that uses this vm as a backend may increase because of the strong type system.
I would like to verify that as well in this project.

If you dare to write the concerns of this project here, it may be difficult to develop a programming language that uses this as a backend by giving a strong type to the virtual machine language. I hope we can verify that area as well.

This project is originally the continuation of `PlanckForth <https://github.com/nineties/planckforth>`_,
which aims to bootstrap a high-level language processor starting from hand-written machine language
without relying on any existing programming languages or libraries.
But the specification of VM and IR are designed to be independent of PlanckForth.
So this system can be implemented in other languages and bytecodes have a portable format.

- `Specification <spec/index.rst>`_
