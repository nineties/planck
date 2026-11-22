## Time per benchmark (seconds, median)

| benchmark | main |
|---|---:|
| use_libs | 2.545 |
| recursion | 0.187 |
| match | 0.696 |
| partial_fusion | 0.972 |
| list_lib | 2.508 |
| vector_table | 0.375 |
| strings | 0.617 |
| bigint | 1.966 |
| bytes_bitset | 0.405 |
| effects_nondet | 0.366 |
| effects_state | 0.266 |
| exceptions | 1.208 |
| macro | 0.903 |
| gc_alloc | 33.035 |
| pretty_error | 1.758 |
| modules_records | 1.193 |
| loops_closures | 2.650 |
| getopt | 0.214 |
| syntax_extension | 2.055 |
| startup | 11.184 |

## Ratio to the previous release in the same job (current / previous, below 1 is faster)

| benchmark | main |
|---|---:|
| use_libs | - |
| recursion | - |
| match | - |
| partial_fusion | - |
| list_lib | - |
| vector_table | - |
| strings | - |
| bigint | - |
| bytes_bitset | - |
| effects_nondet | - |
| effects_state | - |
| exceptions | - |
| macro | - |
| gc_alloc | - |
| pretty_error | - |
| modules_records | - |
| loops_closures | - |
| getopt | - |
| syntax_extension | - |
| startup | - |

## Allocated memory per benchmark (MiB)

| benchmark | main |
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

## Garbage collections per benchmark

| benchmark | main |
|---|---:|
| use_libs | 1 |
| recursion | 0 |
| match | 0 |
| partial_fusion | 1 |
| list_lib | 0 |
| vector_table | 0 |
| strings | 0 |
| bigint | 1 |
| bytes_bitset | 0 |
| effects_nondet | 0 |
| effects_state | 0 |
| exceptions | 1 |
| macro | 0 |
| gc_alloc | 1 |
| pretty_error | 1 |
| modules_records | 0 |
| loops_closures | 1 |
| getopt | 0 |
| syntax_extension | 0 |

## Runs

| ref | commit | date | planck0 | CPU |
|---|---|---|---|---|
| main | e96e20c | 2026-11-22 | v0.1.1 |  |
