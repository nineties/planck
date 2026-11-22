## Versions and machines

Each column of the tables below is one measured version, named by its tag or branch and the short commit. The commit and the machine are in this table.

| version | commit | OS | CPU | cores | date | planck0 |
|---|---|---|---|---:|---|---|
| main (b8eb650) | [b8eb650](https://github.com/nineties/planck/commit/b8eb65013e1f853361bd849aec2d460e79fcc22f) | Ubuntu 26.04.1 LTS | Intel(R) Xeon(R) 6975P-C, 15.3 GiB memory | 4 | 2026-11-22 | v0.1.1 |

## Time per benchmark (seconds, median)

| benchmark | main (b8eb650) |
|---|---:|
| use_libs | 1.707 |
| recursion | 0.120 |
| match | 0.442 |
| partial_fusion | 0.603 |
| list_lib | 1.393 |
| vector_table | 0.252 |
| strings | 0.339 |
| bigint | 1.754 |
| bytes_bitset | 0.264 |
| effects_nondet | 0.246 |
| effects_state | 0.177 |
| exceptions | 0.821 |
| macro | 0.605 |
| gc_alloc | 67.661 |
| pretty_error | 1.292 |
| modules_records | 0.788 |
| loops_closures | 1.641 |
| getopt | 0.151 |
| syntax_extension | 3.154 |
| startup | 7.490 |

## Allocated memory per benchmark (MiB)

| benchmark | main (b8eb650) |
|---|---:|
| use_libs | 55.060 |
| recursion | 5.363 |
| match | 23.546 |
| partial_fusion | 34.570 |
| list_lib | 10.509 |
| vector_table | 8.935 |
| strings | 4.597 |
| bigint | 57.911 |
| bytes_bitset | 7.700 |
| effects_nondet | 8.632 |
| effects_state | 10.325 |
| exceptions | 46.036 |
| macro | 25.819 |
| gc_alloc | 54.163 |
| pretty_error | 30.995 |
| modules_records | 26.064 |
| loops_closures | 41.676 |
| getopt | 4.987 |
| syntax_extension | 11.393 |

