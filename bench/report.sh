#!/bin/bash
set -eu
bench=$(cd "$(dirname "$0")" && pwd)
files=("$@")
if [ ${#files[@]} -eq 0 ]; then
    files=("$bench"/results.jsonl)
fi

cat "${files[@]}" | awk '
function get(line, key,    m) {
    if (match(line, "\"" key "\":(\"[^\"]*\"|[^,}]*)")) {
        m = substr(line, RSTART + length(key) + 3, RLENGTH - length(key) - 3)
        gsub(/^"|"$/, "", m)
        return m
    }
    return ""
}
{
    c = get($0, "case"); r = get($0, "ref")
    if (!(r in seen)) { seen[r] = 1; refs[++nr] = r; date[r] = substr(get($0, "date"), 1, 10); commit[r] = substr(get($0, "commit"), 1, 7); cpu[r] = get($0, "cpu"); p0[r] = get($0, "planck0") }
    if (!(c in cs)) { cs[c] = 1; cases[++nc] = c }
    net[c, r] = (c == "startup") ? get($0, "time_s") : get($0, "net_s")
    ratio[c, r] = get($0, "ratio")
    rss[c, r] = get($0, "max_rss_kb")
}
END {
    print "## Net time per benchmark (seconds, start-up time subtracted)\n"
    printf "| benchmark |"
    for (i = 1; i <= nr; i++) printf " %s |", refs[i]
    printf "\n|---|"
    for (i = 1; i <= nr; i++) printf "---:|"
    printf "\n"
    for (j = 1; j <= nc; j++) {
        c = cases[j]
        label = (c == "startup") ? "startup (raw time of an empty program)" : c
        printf "| %s |", label
        for (i = 1; i <= nr; i++) { v = net[c, refs[i]]; printf " %s |", (v == "" ? "-" : v) }
        printf "\n"
    }
    print "\n## Ratio to the previous release in the same job (current / previous, below 1 is faster)\n"
    printf "| benchmark |"
    for (i = 1; i <= nr; i++) printf " %s |", refs[i]
    printf "\n|---|"
    for (i = 1; i <= nr; i++) printf "---:|"
    printf "\n"
    for (j = 1; j <= nc; j++) {
        c = cases[j]
        printf "| %s |", c
        for (i = 1; i <= nr; i++) { v = ratio[c, refs[i]]; printf " %s |", (v == "" || v == "null" ? "-" : v) }
        printf "\n"
    }
    print "\n## Peak memory per benchmark (KiB, maximum resident set size)\n"
    printf "| benchmark |"
    for (i = 1; i <= nr; i++) printf " %s |", refs[i]
    printf "\n|---|"
    for (i = 1; i <= nr; i++) printf "---:|"
    printf "\n"
    for (j = 1; j <= nc; j++) {
        c = cases[j]
        printf "| %s |", c
        for (i = 1; i <= nr; i++) { v = rss[c, refs[i]]; printf " %s |", (v == "" ? "-" : v) }
        printf "\n"
    }
    print "\n## Runs\n"
    print "| ref | commit | date | planck0 | CPU |"
    print "|---|---|---|---|---|"
    for (i = 1; i <= nr; i++) printf "| %s | %s | %s | %s | %s |\n", refs[i], commit[refs[i]], date[refs[i]], p0[refs[i]], cpu[refs[i]]
}'
