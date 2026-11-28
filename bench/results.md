## Machine

Every version is measured on the same machine: Debian GNU/Linux 13 (trixie), Intel(R) Xeon(R) Platinum 8481C CPU @ 2.70GHz, 15.6 GiB memory, 4 cores.

## Versions

Each column of the tables below is one measured version, named by its tag or branch and the short commit. The commit is in this table.

| version | commit | date | planck0 |
|---|---|---|---|
| main (220b5fd) | [220b5fd](https://github.com/nineties/planck/commit/220b5fdac37914dbd35731bab35f2a624f990f4f) | 2026-11-23 | v0.1.5 |
| main (9fb8acd) | [9fb8acd](https://github.com/nineties/planck/commit/9fb8acd04825685849ef08a693bbb7e268da1ca8) | 2026-11-25 | v0.1.6 |
| main (3a9e872) | [3a9e872](https://github.com/nineties/planck/commit/3a9e872a2b73e6b3f812aab7d5d410047e8ea9a0) | 2026-11-25 | v0.1.6 |
| main (0f8a49a) | [0f8a49a](https://github.com/nineties/planck/commit/0f8a49aa5b0ca5410312a24b6ec7390a0d063bb8) | 2026-11-25 | v0.1.7 |
| main (9a459be) | [9a459be](https://github.com/nineties/planck/commit/9a459be80113e8c985eef43eda09803c1f774058) | 2026-11-28 | v0.1.12 |

## Time per benchmark (seconds, median)

| benchmark | main (220b5fd) | main (9fb8acd) | main (3a9e872) | main (0f8a49a) | main (9a459be) |
|---|---:|---:|---:|---:|---:|
| use_libs | 1.968 | 2.208 | 2.154 | 2.434 | 2.029 |
| recursion | 0.139 | 0.152 | 0.150 | 0.216 | 0.150 |
| match | 0.708 | 0.576 | 0.574 | 0.825 | 0.571 |
| partial_fusion | 0.587 | 0.750 | 0.726 | 1.064 | 0.747 |
| list_lib | 2.362 | 2.344 | 2.345 | 2.395 | 2.311 |
| vector_table | 0.445 | 0.399 | 0.395 | 0.444 | 0.332 |
| strings | 0.550 | 0.579 | 0.573 | 0.179 | 0.147 |
| bigint | 1.336 | 1.805 | 0.897 | 0.406 | 0.374 |
| bytes_bitset | 0.304 | 0.448 | 0.532 | 0.533 | 0.398 |
| effects_nondet | 0.283 | 0.429 | 0.301 | 0.370 | 0.310 |
| effects_state | 0.216 | 0.223 | 0.220 | 0.289 | 0.226 |
| exceptions | 1.079 | 0.925 | 1.079 | 1.330 | 1.010 |
| macro | 0.766 | 1.087 | 0.730 | 0.971 | 0.779 |
| gc_alloc | 2.885 | 1.973 | 1.983 | 10.998 | 49.821 |
| pretty_error | 1.388 | 2.894 | 2.885 | 3.086 | 2.582 |
| modules_records | 1.076 | 1.485 | 1.517 | 2.000 | 1.559 |
| loops_closures | 1.867 | 1.957 | 1.903 | 2.056 | 1.599 |
| getopt | 0.169 | 0.216 | 0.213 | 0.248 | 0.200 |
| syntax_extension | 0.348 | 3.633 | 2.757 | 0.675 | 0.467 |
| startup | 8.988 | 10.182 | 10.227 | 10.930 | 9.539 |

## Allocated memory per benchmark (MiB)

| benchmark | main (220b5fd) | main (9fb8acd) | main (3a9e872) | main (0f8a49a) | main (9a459be) |
|---|---:|---:|---:|---:|---:|
| use_libs | 55.120 | 61.246 | 61.573 | 57.274 | 55.224 |
| recursion | 5.365 | 5.547 | 5.559 | 5.558 | 5.532 |
| match | 23.548 | 23.972 | 23.994 | 23.992 | 23.938 |
| partial_fusion | 34.570 | 34.800 | 34.812 | 34.811 | 34.783 |
| list_lib | 10.509 | 10.812 | 10.826 | 9.151 | 9.113 |
| vector_table | 8.929 | 12.064 | 12.095 | 11.256 | 10.126 |
| strings | 4.594 | 5.245 | 5.266 | 5.097 | 4.918 |
| bigint | 57.913 | 58.051 | 58.061 | 34.715 | 34.688 |
| bytes_bitset | 7.698 | 12.146 | 12.171 | 12.861 | 11.410 |
| effects_nondet | 8.622 | 9.292 | 9.341 | 9.329 | 9.207 |
| effects_state | 10.329 | 10.673 | 10.708 | 10.697 | 10.651 |
| exceptions | 46.038 | 46.214 | 46.227 | 46.224 | 46.200 |
| macro | 25.861 | 31.905 | 27.355 | 27.352 | 27.157 |
| gc_alloc | 54.162 | 54.424 | 54.444 | 54.440 | 54.405 |
| pretty_error | 31.066 | 56.575 | 56.587 | 58.218 | 49.055 |
| modules_records | 26.067 | 44.096 | 44.106 | 44.096 | 34.448 |
| loops_closures | 41.677 | 41.828 | 41.846 | 41.838 | 41.815 |
| getopt | 4.992 | 6.168 | 6.190 | 5.799 | 5.623 |
| syntax_extension | 11.471 | 12.571 | 12.567 | 12.532 | 12.473 |

