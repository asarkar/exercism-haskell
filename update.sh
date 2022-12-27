#!/bin/bash

set -e

manifests=()
if [[ -z "$1" ]]; then
	manifests=(**/stack.yaml)
else
	manifests+=("$1/stack.yaml")
fi

dest="${TMPDIR}haskell"
mkdir -p "$dest"

green='\033[1;32m'
no_color='\033[0m'
for m in "${manifests[@]}"; do
	name="$(dirname "$(readlink -f "$m")")"
	name="$(basename "$name")"

	printf "Project dir: %b%s%b\n" "$green" "$name" "$no_color"

	cp -Rf "$name" "$dest"
	exercism download -t haskell -e "$name" -F
	cp -Rf "$dest/$name/src" "$name"
done

rm -rf "$dest"