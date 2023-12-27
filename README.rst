Planck Compiler Infrastructure Project
======================================

This project aims to develop a **Compiler Infrastructure** with advanced **memory safety** and **concurrency** features.

This project began with the concept of merging `Rust <https://www.rust-lang.org/>`_'s type system with `LLVM <https://llvm.org>`_'s concepts.
The challenge lies in creating a language and tools for a low-level virtual machine, incorporating a modern static type system. This system should enable memory safety and advanced optimization features without incurring any runtime costs.
A potential concern is the increased difficulty in developing a language processor that uses Planck as a backend, due to the stringent requirements of a strong type system. This project seeks to evaluate this aspect.

Originally, this project is a continuation of `PlanckForth <https://github.com/nineties/planckforth>`_, which focused on bootstrapping a high-level language processor from hand-written machine language, independent of any existing programming languages or libraries.
However, the VM and IR specifications are designed to be separate from PlanckForth, allowing the system to be implemented in various languages. Additionally, the bytecode format is made to be portable.

- `Specification <spec/index.rst>`_
