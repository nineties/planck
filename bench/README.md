# Planck benchmarks

A benchmark set that covers the features of Planck, and a record of the results across releases.

## How a benchmark runs

`bench/cases/NAME.pk` is run as `planck0 bench/cases/NAME.pk` from the root of a Planck checkout, the same way as the files under `planck/tests`. `bench/cases/NAME.out` is the expected standard output. A run whose output differs is a failure and is not recorded.

`planck0` is the binary of the latest release of `nineties/hex-to-compiler`. Planck itself is read from the checkout that is being measured, so only the Planck side changes between two measurements.

## Start-up is a benchmark of its own

`planck0` reads `planck/init.pk`, `planck/peg.pk` and `planck/syntax.pk` before it runs a program. An empty program (`startup`) takes about 27 s with `planck0` v0.1.0, so the body of a benchmark is a small part of one run. For that reason:

* `startup` is one benchmark. Its time and its peak memory are recorded for every release, in rows of their own.
* For every other benchmark, the job also measures `startup` and records `net_s = time_s - startup_s`. The tables compare `net_s`.
* Each benchmark body is sized to take a few seconds on top of the start-up time.

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

* `run-one.sh TREE PLANCK0 NAME` runs one benchmark once, checks the output, and prints the elapsed seconds and the maximum resident set size in KiB (`/usr/bin/time`).
* `measure.sh CUR_TREE PREV_TREE PLANCK0 NAME [ROUNDS]` runs `startup` and the benchmark in the order previous, current, current, previous, `ROUNDS` times (default 3), takes the median of each, and prints one JSON line. `PREV_TREE` may be `-`. It reads `CUR_REF`, `CUR_COMMIT`, `PREV_REF`, `PREV_COMMIT` and `PLANCK0_VERSION` from the environment.
* `report.sh [FILE...]` prints the comparison tables in Markdown from JSON lines (default `results.jsonl`).
* `results.jsonl` is the record, one line per benchmark and release. `results.md` is the tables made from it.

Fields of a line: `case`, `ref` (tag), `commit`, `prev_ref`, `prev_commit`, `planck0`, `nproc`, `cpu`, `date`, `rounds`, `time_s`, `startup_s`, `net_s`, `prev_time_s`, `prev_startup_s`, `prev_net_s`, `ratio` (`net_s / prev_net_s`; for `startup`, `time_s / prev_time_s`), `max_rss_kb`.

## Recording

The workflow `.github/workflows/bench.yml` runs on GitHub Actions. Shared runners differ in speed from one run to the next, so the workflow never compares numbers from different jobs. Inside one job, the previous release and the measured version run alternately (previous, current, current, previous), and the ratio of the medians is recorded together with the CPU name of the runner. Compare ratios between releases; absolute seconds are only comparable between rows that share a CPU name.

* A push of a tag `v*` measures that tag against the release before it and opens a pull request that adds the lines to `results.jsonl` and regenerates `results.md`. Merging the pull request is a human decision.
* A manual run (`workflow_dispatch`) takes the version to measure (branch, tag or commit), the version to compare with (empty means the release before the measured one), the number of rounds, and whether to open the pull request (default: no). The tables always appear in the summary of the run.
* The repository setting "Allow GitHub Actions to create and approve pull requests" has to be on for the pull request.

To measure an older release later, run the workflow by hand with its tag as the version to measure and `pr` on.

## Adding a benchmark

Put `NAME.pk` and its expected output `NAME.out` in `cases/`, and add a row to the coverage table. Check on a quiet machine that the body takes a few seconds on top of `startup`.
