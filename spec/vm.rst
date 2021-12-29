===============
Virtual Machine
===============

PlanckIR is a portable program representation that does not depend on a specific
computer or operating system, and the semantics of the program are given by the
virtual machine, **PlanckVM**, defined in this chapter. This virtual machine is a kind of
register machine with infinitely many registers.

Registers
=========

PlanckVM has an infinite number of registers ``%0``, ``%1``, ``%2``, and so on.
Bit width and type of each register is not fixed. They are simply used as variable
names associated to some places where values are stored. The places may be physical
registers, stack memory regions, heap memory regions and so on.
Compiler backends determine the actual locations for the specific target hardware
based on the information of type inference.

Memory Model
============

Calling Convention
==================

Since PlanckVM does not have stack memory, all function call arguments are
passed by registers.

In many register machines, registers with the same name always point to the same
physical location, so when one function calls another, registers in use may be
overwritten. Therefore, the concept of a *callee-save register* to save and restore
registers on the called function side and a *caller-save register* to save and restore registers on the caller side are necessary.

On the other hand, registers of PlanckVM are not physical registers, but actually
function-local variables. Registers with the same name used in different functions
point different locations and will not be overwritten by function calls.
Therefore, there are no special function call conventions in PlanckVM.
