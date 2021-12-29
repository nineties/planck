===============
Virtual Machine
===============

PlanckIR is a portable program representation that does not depend on a specific
computer or operating system, and the semantics of the program are given by the
virtual machine, **PlanckVM**, defined in this chapter. This virtual machine is a kind of
register machine with infinitely many registers.

Registers
=========

PlanckVM has an infinite number of registers `%0`, `%1`, `%2`, and so on.
Bit width and type of each register is not fixed. They are simply used as variable
names associated to some places where values are stored. The places may be physical
registers, stack memory regions, heap memory regions and so on.
Compiler backends determine the actual locations for the specific target hardware
based on the information of type inference.

Overview
========



Memory Model
============

Calling Convention
==================
