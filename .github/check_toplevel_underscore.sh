#!/bin/bash
base=${1:-origin/main}
names() {
    git show "$1:$2" 2>/dev/null |
        grep -oE '^(Def(Fun)?\{)?_[A-Za-z][A-Za-z0-9_]*' |
        sed -E 's/^Def(Fun)?\{//' | sort -u
}
status=0
for f in $(git diff --name-only "$base"...HEAD -- 'planck/*.pk' 'planck/lib/*.pk'); do
    added=$(comm -13 <(names "$base" "$f") <(names HEAD "$f"))
    if [ -n "$added" ]; then
        echo "$f: new top-level names that start with _: $(echo $added)"
        status=1
    fi
done
[ $status = 0 ] || echo "Write the helper inside the function that uses it, or inside one closure shared by the functions that use it."
exit $status
