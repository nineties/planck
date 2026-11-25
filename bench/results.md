## Machine

Every version is measured on the same machine: Debian GNU/Linux 13 (trixie), Intel(R) Xeon(R) Platinum 8481C CPU @ 2.70GHz, 15.6 GiB memory, 4 cores.

## Versions

Each column of the tables below is one measured version, named by its tag or branch and the short commit. The commit is in this table.

| version | commit | date | planck0 |
|---|---|---|---|
| main (220b5fd) | [220b5fd](https://github.com/nineties/planck/commit/220b5fdac37914dbd35731bab35f2a624f990f4f) | 2026-11-23 | v0.1.5 |
| main (9fb8acd) | [9fb8acd](https://github.com/nineties/planck/commit/9fb8acd04825685849ef08a693bbb7e268da1ca8) | 2026-11-25 | v0.1.6 |

## Time per benchmark (seconds, median)

| benchmark | main (220b5fd) | main (9fb8acd) |
|---|---:|---:|
| use_libs | 1.968 | 2.208 |
| recursion | 0.139 | 0.152 |
| match | 0.708 | 0.576 |
| partial_fusion | 0.587 | 0.750 |
| list_lib | 2.362 | 2.344 |
| vector_table | 0.445 | 0.399 |
| strings | 0.550 | 0.579 |
| bigint | 1.336 | 1.805 |
| bytes_bitset | 0.304 | 0.448 |
| effects_nondet | 0.283 | 0.429 |
| effects_state | 0.216 | 0.223 |
| exceptions | 1.079 | 0.925 |
| macro | 0.766 | 1.087 |
| gc_alloc | 2.885 | 1.973 |
| pretty_error | 1.388 | 2.894 |
| modules_records | 1.076 | 1.485 |
| loops_closures | 1.867 | 1.957 |
| getopt | 0.169 | 0.216 |
| syntax_extension | 0.348 | 3.633 |
| startup | 8.988 | 10.182 |

## Allocated memory per benchmark (MiB)

| benchmark | main (220b5fd) | main (9fb8acd) |
|---|---:|---:|
| use_libs | 55.120 | 61.246 |
| recursion | 5.365 | 5.547 |
| match | 23.548 | 23.972 |
| partial_fusion | 34.570 | 34.800 |
| list_lib | 10.509 | 10.812 |
| vector_table | 8.929 | 12.064 |
| strings | 4.594 | 5.245 |
| bigint | 57.913 | 58.051 |
| bytes_bitset | 7.698 | 12.146 |
| effects_nondet | 8.622 | 9.292 |
| effects_state | 10.329 | 10.673 |
| exceptions | 46.038 | 46.214 |
| macro | 25.861 | 31.905 |
| gc_alloc | 54.162 | 54.424 |
| pretty_error | 31.066 | 56.575 |
| modules_records | 26.067 | 44.096 |
| loops_closures | 41.677 | 41.828 |
| getopt | 4.992 | 6.168 |
| syntax_extension | 11.471 | 12.571 |

