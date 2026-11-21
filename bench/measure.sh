#!/bin/bash
set -eu
cur_tree=$1
prev_tree=$2
planck0=$3
case_name=$4
rounds=${5:-3}
bench=$(cd "$(dirname "$0")" && pwd)

median() {
    sort -n | awk '{ v[NR] = $1 } END { if (NR % 2) print v[(NR + 1) / 2]; else print (v[NR / 2] + v[NR / 2 + 1]) / 2 }'
}

sample() {
    local tree=$1 name=$2
    "$bench/run-one.sh" "$tree" "$planck0" "$name"
}

collect() {
    local name=$1
    local cur_times=$(mktemp) prev_times=$(mktemp)
    local round
    for round in $(seq "$rounds"); do
        if [ "$prev_tree" != "-" ]; then
            sample "$prev_tree" "$name" | tee -a "$prev_times.raw" | cut -d' ' -f1 >> "$prev_times"
        fi
        sample "$cur_tree" "$name" | tee -a "$cur_times.raw" | cut -d' ' -f1 >> "$cur_times"
        sample "$cur_tree" "$name" | tee -a "$cur_times.raw" | cut -d' ' -f1 >> "$cur_times"
        if [ "$prev_tree" != "-" ]; then
            sample "$prev_tree" "$name" | tee -a "$prev_times.raw" | cut -d' ' -f1 >> "$prev_times"
        fi
    done
    cur_s=$(median < "$cur_times")
    cur_kb=$(cut -d' ' -f2 "$cur_times.raw" | sort -n | tail -1)
    if [ "$prev_tree" != "-" ]; then
        prev_s=$(median < "$prev_times")
        prev_kb=$(cut -d' ' -f2 "$prev_times.raw" | sort -n | tail -1)
    else
        prev_s=null
        prev_kb=null
    fi
}

collect startup
cur_startup=$cur_s
prev_startup=$prev_s
if [ "$case_name" = startup ]; then
    cur_net=0
    prev_net=0
    if [ "$prev_tree" = "-" ]; then prev_net=null; fi
    cur_s=$cur_startup
    prev_s=$prev_startup
else
    collect "$case_name"
    cur_net=$(awk -v a="$cur_s" -v b="$cur_startup" 'BEGIN { printf "%.3f", a - b }')
    if [ "$prev_tree" != "-" ]; then
        prev_net=$(awk -v a="$prev_s" -v b="$prev_startup" 'BEGIN { printf "%.3f", a - b }')
    else
        prev_net=null
    fi
fi

if [ "$prev_net" = null ]; then
    ratio=null
elif [ "$case_name" = startup ]; then
    ratio=$(awk -v a="$cur_s" -v b="$prev_s" 'BEGIN { printf "%.3f", a / b }')
else
    ratio=$(awk -v a="$cur_net" -v b="$prev_net" 'BEGIN { if (a < 0.05 || b < 0.05) print "null"; else printf "%.3f", a / b }')
fi

cpu=$(awk -F': ' '/model name/ { print $2; exit }' /proc/cpuinfo)
printf '{"case":"%s","ref":"%s","commit":"%s","prev_ref":"%s","prev_commit":"%s","planck0":"%s","nproc":%s,"cpu":"%s","date":"%s","rounds":%s,"time_s":%s,"startup_s":%s,"net_s":%s,"prev_time_s":%s,"prev_startup_s":%s,"prev_net_s":%s,"ratio":%s,"max_rss_kb":%s}\n' \
    "$case_name" "${CUR_REF:-}" "${CUR_COMMIT:-}" "${PREV_REF:-}" "${PREV_COMMIT:-}" "${PLANCK0_VERSION:-}" \
    "$(nproc)" "$cpu" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" "$rounds" \
    "$cur_s" "$cur_startup" "$cur_net" "$prev_s" "$prev_startup" "$prev_net" "$ratio" "$cur_kb"
