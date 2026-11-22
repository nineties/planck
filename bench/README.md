# Planck benchmarks

A benchmark set that covers the features of Planck, and a record of the results across releases. The benchmarks, the measurement and the tables are Planck programs that run on `planck0`.

## How a measurement runs

`bench/run.pk` runs every benchmark once in one `planck0` process, from the root of a Planck checkout:

```
planck0 bench/run.pk SAMPLES [START]
```

* `bench/cases/NAME.pk` is a benchmark and `bench/cases/NAME.out` is its expected standard output. `run.pk` loads each case with `load`, collects what it prints by replacing `put`, and stops with an error if the text differs from the `.out` file.
* For every case, `run.pk` takes the elapsed time (`time_ns`), the bytes allocated (`heap_allocated`) and the number of garbage collections (`gc_count`), and appends one `sample("NAME", ns, bytes, gcs)` line to the file `SAMPLES`.
* `START` is a value of `time_ns` taken just before `planck0` was started. When it is given, `run.pk` also records `startup`: the time from that moment to the first line of `run.pk`, which is the time `planck0` takes to read `init.pk`, `peg.pk` and `syntax.pk` and to start the program.
* The first case, `use_libs`, loads every library file with `use`. The later cases do not pay for loading a library again, so `use_libs` is the benchmark of loading libraries and the others measure the use of them. `syntax_extension` is the last case because it extends the grammar of the process.

A case is a plain Planck program that prints its result. It can be run alone with `planck0 bench/cases/NAME.pk`.

The measuring programs need the builtins `time_ns`, `heap_allocated`, `gc_count`, `run_program` and `list_dir` of `planck0`.

## Start-up is a benchmark of its own

`planck0` reads `planck/init.pk`, `planck/peg.pk` and `planck/syntax.pk` before it runs a program, which takes about 27 s. A process runs all the cases, so this cost is paid once per process instead of once per benchmark. `startup` is recorded for every release in rows of its own.

## Coverage

| benchmark | feature | existing test or example with the same feature |
|---|---|---|
| `startup` | loading `init.pk`, `peg.pk`, `syntax.pk` | none |
| `use_libs` | `use` of every library file | `tests/import.pk`, `tests/lib.pk` |
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
| `syntax_extension` | `::=` grammar rules, `peg_parse`, extending the syntax | `examples/extend.pk`, `examples/calc.pk` |
| `gc_alloc` | allocation of lists, tuples and arrays (garbage collector) | none |
| `pretty_error` | `lib/pretty.pk`, `lib/error.pk` | `tests/lib.pk` |
| `modules_records` | `module`, `export`, `record` | `tests/module.pk`, `examples/with.pk` |
| `loops_closures` | `while`, `for`, closures with mutable state | `tests/core.pk` |
| `getopt` | `lib/getopt.pk` | `tests/lib.pk`, `examples/greet.pk` |

## Files

* `measure.pk KEY=VALUE...` runs `run.pk` in the order previous, current, current, previous for the given number of rounds, each in its own checkout, then runs `record.pk` and `report.pk`. The keys are `planck0_bin`, `cur`, `prev`, `out`, `ref`, `commit`, `prev_ref`, `prev_commit`, `planck0`, `rounds`, `summary` and, to record the result, `results` and `results_md`.
* `run.pk SAMPLES [START]` measures every case once and appends the samples to `SAMPLES`.
* `record.pk OUT CUR_SAMPLES PREV_SAMPLES META` takes the median of the samples of each case, the ratio to the previous version, and appends one `result(...)` line per case to `OUT`. `PREV_SAMPLES` may be `-`. `META` is a file of `key=value` lines: `ref`, `commit`, `prev_ref`, `prev_commit`, `planck0`, `nproc`, `os`, `cpu`, `date`, `rounds`.
* `report.pk RESULTS OUT` writes the tables in Markdown from a file of `result(...)` lines.
* `results.pk` is the record, one line per benchmark and release. `results.md` is the tables made from it.

Fields of a `result` line: case, ref (tag), commit, prev_ref, prev_commit, planck0, nproc, os, cpu, date, rounds, time in ns (median), previous time in ns, ratio (current / previous, in thousandths), bytes allocated, garbage collections.

Memory is recorded as the bytes allocated and the garbage collections of each case. These two values do not depend on the speed of the machine.

## Recording

The workflow `.github/workflows/bench.yml` runs on GitHub Actions. Shared runners differ in speed from one run to the next, so the workflow never compares numbers from different jobs. Inside one job, the previous release and the measured version run alternately (previous, current, current, previous) for the number of rounds, and the ratio of the medians is recorded together with the CPU name of the runner. Compare ratios between releases; absolute seconds are only comparable between rows that share a CPU name.

* A push of a tag `v*` measures that tag against the release before it and opens a pull request that adds the lines to `results.pk` and regenerates `results.md`. Merging the pull request is a human decision.
* A manual run (`workflow_dispatch`) takes the version to measure (branch, tag or commit), the version to compare with (empty means the release before the measured one), the number of rounds, and whether to open the pull request (default: no). The tables always appear in the summary of the run.
* The repository setting "Allow GitHub Actions to create and approve pull requests" has to be on for the pull request.
