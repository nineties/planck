## Versions and machines

Each column of the tables below is one measured version, named by its tag or branch and the short commit. The commit and the machine are in this table.

| version | commit | OS | CPU | cores | date | planck0 |
|---|---|---|---|---:|---|---|
| main (b8eb650) | [b8eb650](https://github.com/nineties/planck/commit/b8eb65013e1f853361bd849aec2d460e79fcc22f) | Ubuntu 26.04.1 LTS | Intel(R) Xeon(R) 6975P-C, 15.3 GiB memory | 4 | 2026-11-22 | v0.1.1 |
| main (c770eb8) | [c770eb8](https://github.com/nineties/planck/commit/c770eb8f8f84c76ebbe18dd128a9c3e0430c6e77) | Ubuntu 26.04.1 LTS | INTEL(R) XEON(R) PLATINUM 8573C, 15.6 GiB memory | 4 | 2026-11-23 | v0.1.5 |

## Time per benchmark (seconds, median)

| benchmark | main (b8eb650) | main (c770eb8) |
|---|---:|---:|
| use_libs | 1.707 | 1.872 |
| recursion | 0.120 | 0.136 |
| match | 0.442 | 0.642 |
| partial_fusion | 0.603 | 0.595 |
| list_lib | 1.393 | 1.780 |
| vector_table | 0.252 | 0.385 |
| strings | 0.339 | 0.421 |
| bigint | 1.754 | 2.226 |
| bytes_bitset | 0.264 | 0.285 |
| effects_nondet | 0.246 | 0.272 |
| effects_state | 0.177 | 0.211 |
| exceptions | 0.821 | 1.054 |
| macro | 0.605 | 0.726 |
| gc_alloc | 67.661 | 2.764 |
| pretty_error | 1.292 | 1.425 |
| modules_records | 0.788 | 1.015 |
| loops_closures | 1.641 | 1.849 |
| getopt | 0.151 | 0.164 |
| syntax_extension | 3.154 | 0.334 |
| startup | 7.490 | 8.612 |

## Allocated memory per benchmark (MiB)

| benchmark | main (b8eb650) | main (c770eb8) |
|---|---:|---:|
| use_libs | 55.060 | 55.119 |
| recursion | 5.363 | 5.365 |
| match | 23.546 | 23.548 |
| partial_fusion | 34.570 | 34.570 |
| list_lib | 10.509 | 10.509 |
| vector_table | 8.935 | 8.929 |
| strings | 4.597 | 4.594 |
| bigint | 57.911 | 57.913 |
| bytes_bitset | 7.700 | 7.698 |
| effects_nondet | 8.632 | 8.622 |
| effects_state | 10.325 | 10.328 |
| exceptions | 46.036 | 46.038 |
| macro | 25.819 | 25.861 |
| gc_alloc | 54.163 | 54.162 |
| pretty_error | 30.995 | 31.066 |
| modules_records | 26.064 | 26.067 |
| loops_closures | 41.676 | 41.677 |
| getopt | 4.987 | 4.992 |
| syntax_extension | 11.393 | 11.471 |

