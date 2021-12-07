=============================
PlanckVM and IR Specification
=============================

**(This is draft)**

This is a specification of **PlanckVM**, a low-level virtual machine, and **PlanckIR**,
an intermediate representation for the VM.

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
-----

Regions
-------

Effects
-------

Ownership
---------

Smart Pointers
--------------

Static Single Assignment (SSA) Form
===================================

PlanckVM adopts *Static single assgnment* form or *SSA* form,
like `LLVM <https://llvm.org>`_, which requires that every variables
can be assigned exactly once. Since the definition-use analysis of
variables in SSA form is explicit, optimization of codes become easier.

To represent a program that assigns values to the same variables in paths
of control flows in SSA form, the *phi function* is used.
The value of ``phi(x1, ..., xn)`` is the one of the argument depending on which
control flow path the program took.

::

   if (...) {
      x1 = ...
   } else {
      x2 = ...
   }
   y = phi(x1, x2)

Instruction Types
=================

+---------------------------+---------------------------+
| name                      | notation                  |
+===========================+===========================+
| builtin operations        | y = op(x1, ..., xn)       |
+---------------------------+---------------------------+
| function call             | y = f(x1, ..., xn)        |
+---------------------------+---------------------------+
| phi function              | y = phi(x1, ..., xn)      |
+---------------------------+---------------------------+
| returning from a function | return x                  |
+---------------------------+---------------------------+
| unconditional branch      | goto label                |
+---------------------------+---------------------------+
| conditional branch        | branch cond labl          |
+---------------------------+---------------------------+

Appendix
========

- `Language Syntax <syntax.rst>`_
- `Bytecode Encoding <bytecode.rst>`_
