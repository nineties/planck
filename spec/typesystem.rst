===========
Type System
===========

The language for Planck has a **static type system** unlike most of other assembly language for physical/virtual machine languages.
Thanks to the type system, it is possible to prevent bugs, such as unintentionally treating an
integer as a pointer, by type checking.

Every local variable is implicitly typed and the type checker infer its type at compile time.
Therefore the compiled bytecode does not contain any extra bit for type annotation, in other words
there is no run-time overhead. Functions, variables and constants which has global linkage is explicitly
typed and the information is used for link time type checking.

Planck's type system is designed by strongly referring to Rust's type system.
It has a feature of **ownership**, **borrowing**, **reference** and **lifetime**.
In addition to it the type system also has **effects**.

Ownership
=========

Every value has a variable that is called its **owner** and there is only one owner at a time.
When the **lifetime** of the owner ends, the value will automatically be dropped.

Types
=====

Primitive Types
---------------

- never: `!`
- boolean: `bool`
- character: `char`
- unsigned integers: `u8`, `u32`, `u32`, `u64`
- signed integers: `i8`, `i32`, `i32`, `i64`
- IEE754 floating point number: `f32`, `f64`
- string: `str`

Reference
=========

Lifetime
========

Effect
======

Smart Pointers
==============

