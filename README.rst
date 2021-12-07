Planck Virtual Machine (PlanckVM)
=================================

This project aims to develop a **Low Level Virtual Machine (PlanckVM)** and
an **Intermediate Representation (PlanckIR)** which have **memory safety**
and **concurrency** features.

This is basically the continuation of `PlanckForth <https://github.com/nineties/planckforth>`_,
which aims to bootstrap a high-level language processor starting from hand-written machine language
without relying on any existing programming language or library.
But the specification of VM and IR are designed to be independent of PlanckForth.
So this system can be implemented in other languages and bytecodes have portable format.

- `Specification <spec/index.rst>`_
