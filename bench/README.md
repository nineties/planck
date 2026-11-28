# Planck benchmarks

A benchmark set that covers the features of Planck, and a record of the results across releases. The benchmarks, the measurement and the tables are Planck programs that run on `planck0`.

## How a measurement runs

`bench/run.pk` runs every benchmark once in one `planck0` process, from the root of a Planck checkout:

```
planck0 bench/run.pk SAMPLES [START]
```

* `bench/cases/NAME.pk` is a benchmark and `bench/cases/NAME.out` is its expected standard output. `run.pk` loads each case with `load`, collects what it prints by replacing `put`, and stops with an error if the text differs from the `.out` file.
* For every case, `run.pk` takes the elapsed time (`os_time_ns`), the bytes allocated (`gc_allocated_bytes`) and the number of garbage collections (`gc_count`), and appends one `sample("NAME", ns, bytes, gcs)` line to the file `SAMPLES`.
* `START` is a value of `os_time_ns` taken just before `planck0` was started. When it is given, `run.pk` also records `startup`: the time from that moment to the first line of `run.pk`, which is the time `planck0` takes to read `init.pk`, `peg.pk` and `syntax.pk` and to start the program.
* The first case, `use_libs`, loads every library file with `import_lib`. The later cases do not pay for loading a library again, so `use_libs` is the benchmark of loading libraries and the others measure the use of them. `syntax_extension` is the last case because it extends the grammar of the process.

A case is a plain Planck program that prints its result. It can be run alone with `planck0 bench/cases/NAME.pk`.

The measuring programs need the builtins `os_time_ns`, `gc_allocated_bytes`, `gc_count`, `os_run` and `os_list_dir` of `planck0`.

## Start-up is a benchmark of its own

`planck0` reads `planck/init.pk`, `planck/peg.pk` and `planck/syntax.pk` before it runs a program, which takes about 27 s. A process runs all the cases, so this cost is paid once per process instead of once per benchmark. `startup` is recorded for every release in rows of its own.

## Coverage

| benchmark | feature | existing test or example with the same feature |
|---|---|---|
| `startup` | loading `init.pk`, `peg.pk`, `syntax.pk` | none |
| `use_libs` | `import_lib` of every library file | `tests/import.pk`, `tests/lib.pk` |
| `recursion` | partial functions (`fun`, `\|` alternatives), deep recursion | `examples/fib.pk` |
| `match` | `match`, constructor, tuple and typed patterns, guards | `examples/destructure.pk`, `examples/pattern_or.pk` |
| `partial_fusion` | closures composed from partial functions | `tests/define.pk` |
| `list_lib` | `lib/list.pk`: map, filter, sort, zip, append | `tests/lib.pk`, `examples/list.pk` |
| `vector_table` | `lib/vector.pk`, `lib/table.pk` | `tests/lib.pk` |
| `strings` | `lib/string.pk`: buffer, split, join, search | `tests/lib.pk` |
| `bigint` | arbitrary-precision integers | `tests/bigint.pk` |
| `bytes_bitset` | `lib/bytes.pk`, `lib/bitset.pk` | only referenced from `tests/module.pk`, `tests/import.pk` |
| `effects_nondet` | `perform` and `handle` with a resumed continuation, n queens | `examples/queens.pk`, `examples/nondet.pk` |
| `effects_state` | state passing through handlers | `examples/state.pk` |
| `exceptions` | handlers that resume many times | `examples/exception.pk` |
| `macro` | `macro`, quasi-quotation, `eval` of a quoted expression | `tests/macro.pk`, `examples/macro.pk` |
| `syntax_extension` | `::=` grammar rules, `parser_parse`, extending the syntax | `examples/extend.pk`, `examples/calc.pk` |
| `gc_alloc` | allocation of lists, tuples and arrays (garbage collector) | none |
| `pretty_error` | `lib/pretty.pk`, `lib/error.pk` | `tests/lib.pk` |
| `modules_records` | `module`, `export`, `record` | `tests/module.pk`, `examples/with.pk` |
| `loops_closures` | `while`, `for`, closures with mutable state | `tests/core.pk` |
| `getopt` | `lib/getopt.pk` | `tests/lib.pk`, `examples/greet.pk` |

## Files

* `measure.pk KEY=VALUE...` runs `run.pk` on the checkout of the measured version for the given number of rounds, then runs `record.pk` and `report.pk`. The keys are `planck0_bin`, `cur`, `out`, `ref`, `commit`, `planck0`, `rounds`, `summary` and, to record the result, `results` and `results_md`.
* `run.pk SAMPLES [START]` measures every case once and appends the samples to `SAMPLES`.
* `record.pk OUT CUR_SAMPLES META` takes the median of the samples of each case and appends one `machine(...)` line and one `result(...)` line per case to `OUT`. `META` is a file of `key=value` lines: `ref`, `commit`, `planck0`, `nproc`, `os`, `cpu`, `date`, `rounds`.
* `report.pk RESULTS OUT` writes the tables in Markdown from a file of `result(...)` lines.
* `results.pk` is the record, one line per benchmark and release. `results.md` is the tables made from it.

Fields of a `result` line: case, ref (tag or branch), commit, planck0, date, rounds, time in ns (median), bytes allocated, garbage collections. The machine is not repeated in them. `results.pk` starts with one `machine(os, cpu with the memory size, nproc)` line, which is written by the first recorded run.

Memory is recorded as the bytes allocated and the garbage collections of each case. These two values do not depend on the speed of the machine. The tables show the bytes allocated only.

## Recording

The workflow `.github/workflows/bench.yml` runs on GitHub Actions. The job runs on the dedicated VM `benchmark` (runner label `planck-bench`), which is started for the run and stopped after it. Every version is measured on this one machine, so the record keeps the OS, the CPU name and the number of cores once, in the `machine` line, and the commit of the measured version next to every column.

* A push of a tag `v*` measures that tag and opens a pull request that adds the lines to `results.pk` and regenerates `results.md`. Merging the pull request is a human decision.
* A manual run (`workflow_dispatch`) takes the version to measure (branch, tag or commit), the number of runs, and whether to open the pull request (default: no). The tables always appear in the summary of the run.
* The repository setting "Allow GitHub Actions to create and approve pull requests" has to be on for the pull request.
