## Machine

Every version is measured on the same machine: Debian GNU/Linux 13 (trixie), Intel(R) Xeon(R) Platinum 8481C CPU @ 2.70GHz, 15.6 GiB memory, 4 cores.

## Versions

Each column of the tables below is one measured version, named by its tag or branch and the short commit. The commit is in this table.

| version | commit | date | planck0 |
|---|---|---|---|
| main (220b5fd) | [220b5fd](https://github.com/nineties/planck/commit/220b5fdac37914dbd35731bab35f2a624f990f4f) | 2026-11-23 | v0.1.5 |

## Time per benchmark (seconds, median)

| benchmark | main (220b5fd) |
|---|---:|
| use_libs | 1.968 |
| recursion | 0.139 |
| match | 0.708 |
| partial_fusion | 0.587 |
| list_lib | 2.362 |
| vector_table | 0.445 |
| strings | 0.550 |
| bigint | 1.336 |
| bytes_bitset | 0.304 |
| effects_nondet | 0.283 |
| effects_state | 0.216 |
| exceptions | 1.079 |
| macro | 0.766 |
| gc_alloc | 2.885 |
| pretty_error | 1.388 |
| modules_records | 1.076 |
| loops_closures | 1.867 |
| getopt | 0.169 |
| syntax_extension | 0.348 |
| startup | 8.988 |

## Allocated memory per benchmark (MiB)

| benchmark | main (220b5fd) |
|---|---:|
| use_libs | 55.120 |
| recursion | 5.365 |
| match | 23.548 |
| partial_fusion | 34.570 |
| list_lib | 10.509 |
| vector_table | 8.929 |
| strings | 4.594 |
| bigint | 57.913 |
| bytes_bitset | 7.698 |
| effects_nondet | 8.622 |
| effects_state | 10.329 |
| exceptions | 46.038 |
| macro | 25.861 |
| gc_alloc | 54.162 |
| pretty_error | 31.066 |
| modules_records | 26.067 |
| loops_closures | 41.677 |
| getopt | 4.992 |
| syntax_extension | 11.471 |

