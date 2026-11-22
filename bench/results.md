## Versions and machines

Each column of the tables below is one measured version, named by its tag or branch and the short commit. The commit and the machine are in this table.

| version | commit | OS | CPU | cores | date | planck0 |
|---|---|---|---|---:|---|---|
| main (e96e20c) | [e96e20c](https://github.com/nineties/planck/commit/e96e20ceb9efe222425dc6ad2dcdcb8d43e1d688) | - | - | - | 2026-11-22 | v0.1.1 |
| main (b8eb650) | [b8eb650](https://github.com/nineties/planck/commit/b8eb65013e1f853361bd849aec2d460e79fcc22f) | Ubuntu 26.04.1 LTS | Intel(R) Xeon(R) 6975P-C, 15.3 GiB memory | 4 | 2026-11-22 | v0.1.1 |

## Time per benchmark (seconds, median)

| benchmark | main (e96e20c) | main (b8eb650) |
|---|---:|---:|
| use_libs | 2.545 | 1.707 |
| recursion | 0.187 | 0.120 |
| match | 0.696 | 0.442 |
| partial_fusion | 0.972 | 0.603 |
| list_lib | 2.508 | 1.393 |
| vector_table | 0.375 | 0.252 |
| strings | 0.617 | 0.339 |
| bigint | 1.966 | 1.754 |
| bytes_bitset | 0.405 | 0.264 |
| effects_nondet | 0.366 | 0.246 |
| effects_state | 0.266 | 0.177 |
| exceptions | 1.208 | 0.821 |
| macro | 0.903 | 0.605 |
| gc_alloc | 33.035 | 67.661 |
| pretty_error | 1.758 | 1.292 |
| modules_records | 1.193 | 0.788 |
| loops_closures | 2.650 | 1.641 |
| getopt | 0.214 | 0.151 |
| syntax_extension | 2.055 | 3.154 |
| startup | 11.184 | 7.490 |

## Allocated memory per benchmark (MiB)

| benchmark | main (e96e20c) | main (b8eb650) |
|---|---:|---:|
| use_libs | 55.060 | 55.060 |
| recursion | 5.363 | 5.363 |
| match | 23.546 | 23.546 |
| partial_fusion | 34.570 | 34.570 |
| list_lib | 10.509 | 10.509 |
| vector_table | 8.935 | 8.935 |
| strings | 4.597 | 4.597 |
| bigint | 57.911 | 57.911 |
| bytes_bitset | 7.700 | 7.700 |
| effects_nondet | 8.632 | 8.632 |
| effects_state | 10.325 | 10.325 |
| exceptions | 46.036 | 46.036 |
| macro | 25.819 | 25.819 |
| gc_alloc | 54.163 | 54.163 |
| pretty_error | 30.995 | 30.995 |
| modules_records | 26.064 | 26.064 |
| loops_closures | 41.676 | 41.676 |
| getopt | 4.987 | 4.987 |
| syntax_extension | 11.393 | 11.393 |

