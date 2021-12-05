PlanckVM/PlanckIR Specification
===============================

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

Appendix
--------

- `Language Syntax <syntax.rst>`_
- `Bytecode Encoding <bytecode.rst>`_
