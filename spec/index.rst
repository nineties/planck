PlanckVM and IR Specification
=============================

Static Single Assignment (SSA) Form
-----------------------------------

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
-----------------

+---------------------------+---------------------------+
| name                      | notation                  |
+===========================+===========================+
| builtin operations        | y = op(x1, ..., xn)       |
+---------------------------+---------------------------+
| function call             | y = f(x1, ..., xn)        |
+---------------------------+---------------------------+
| phi function              | y = phi(x1, ..., xn)      |
+---------------------------+---------------------------+
| return from a function    | return x                  |
+---------------------------+---------------------------+

Appendix
--------

- `Language Syntax <syntax.rst>`_
- `Bytecode Encoding <bytecode.rst>`_
