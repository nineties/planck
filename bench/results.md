## Machine

Every version is measured on the same machine: Debian GNU/Linux 13 (trixie), Intel(R) Xeon(R) Platinum 8481C CPU @ 2.70GHz, 15.6 GiB memory, 4 cores.

## Versions

Each column of the tables below is one measured version, named by its tag or branch and the short commit. The commit is in this table.

| version | commit | date | planck0 |
|---|---|---|---|
| main (220b5fd) | [220b5fd](https://github.com/nineties/planck/commit/220b5fdac37914dbd35731bab35f2a624f990f4f) | 2026-11-23 | v0.1.5 |
| main (9fb8acd) | [9fb8acd](https://github.com/nineties/planck/commit/9fb8acd04825685849ef08a693bbb7e268da1ca8) | 2026-11-25 | v0.1.6 |
| main (3a9e872) | [3a9e872](https://github.com/nineties/planck/commit/3a9e872a2b73e6b3f812aab7d5d410047e8ea9a0) | 2026-11-25 | v0.1.6 |

## Time per benchmark (seconds, median)

| benchmark | main (220b5fd) | main (9fb8acd) | main (3a9e872) |
|---|---:|---:|---:|
| use_libs | 1.968 | 2.208 | 2.154 |
| recursion | 0.139 | 0.152 | 0.150 |
| match | 0.708 | 0.576 | 0.574 |
| partial_fusion | 0.587 | 0.750 | 0.726 |
| list_lib | 2.362 | 2.344 | 2.345 |
| vector_table | 0.445 | 0.399 | 0.395 |
| strings | 0.550 | 0.579 | 0.573 |
| bigint | 1.336 | 1.805 | 0.897 |
| bytes_bitset | 0.304 | 0.448 | 0.532 |
| effects_nondet | 0.283 | 0.429 | 0.301 |
| effects_state | 0.216 | 0.223 | 0.220 |
| exceptions | 1.079 | 0.925 | 1.079 |
| macro | 0.766 | 1.087 | 0.730 |
| gc_alloc | 2.885 | 1.973 | 1.983 |
| pretty_error | 1.388 | 2.894 | 2.885 |
| modules_records | 1.076 | 1.485 | 1.517 |
| loops_closures | 1.867 | 1.957 | 1.903 |
| getopt | 0.169 | 0.216 | 0.213 |
| syntax_extension | 0.348 | 3.633 | 2.757 |
| startup | 8.988 | 10.182 | 10.227 |

## Allocated memory per benchmark (MiB)

| benchmark | main (220b5fd) | main (9fb8acd) | main (3a9e872) |
|---|---:|---:|---:|
| use_libs | 55.120 | 61.246 | 61.573 |
| recursion | 5.365 | 5.547 | 5.559 |
| match | 23.548 | 23.972 | 23.994 |
| partial_fusion | 34.570 | 34.800 | 34.812 |
| list_lib | 10.509 | 10.812 | 10.826 |
| vector_table | 8.929 | 12.064 | 12.095 |
| strings | 4.594 | 5.245 | 5.266 |
| bigint | 57.913 | 58.051 | 58.061 |
| bytes_bitset | 7.698 | 12.146 | 12.171 |
| effects_nondet | 8.622 | 9.292 | 9.341 |
| effects_state | 10.329 | 10.673 | 10.708 |
| exceptions | 46.038 | 46.214 | 46.227 |
| macro | 25.861 | 31.905 | 27.355 |
| gc_alloc | 54.162 | 54.424 | 54.444 |
| pretty_error | 31.066 | 56.575 | 56.587 |
| modules_records | 26.067 | 44.096 | 44.106 |
| loops_closures | 41.677 | 41.828 | 41.846 |
| getopt | 4.992 | 6.168 | 6.190 |
| syntax_extension | 11.471 | 12.571 | 12.567 |

