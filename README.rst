Planck Virtual Machine (PlanckVM)
=================================

This project aims to develop a **Low Level Virtual Machine (PlanckVM)** and
an **Intermediate Representation (PlanckIR)** which have **memory safety**
and **concurrency** features.

This is basically the continuation of `PlanckForth <https://github.com/nineties/planckforth`_,
which aims to bootstrap a high-level language processor starting from hand-writte machine language
without relying on any existing language processor or library.
But since the VM and IR are designed to be independent of PlanckForth,
they can be implemented in other languages and bytecodes are portable.
Also, since we are developing a new system from scratch, we are trying to adopt modern featues such as
memory safety and concurrency as an experiment.

- `Specification <spec/index.rst>`_
