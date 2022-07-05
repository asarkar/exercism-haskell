#!/bin/bash

set -e

export RUSTFLAGS="-D warnings"

no_test=0
no_lint=0

while (( $# > 0 )); do
   case "$1" in
   	--help)
			printf "run.sh [OPTION]... [DIR]\n"
			printf "options:\n"
			printf "\t--help			Show help\n"
			printf "\t--no-test		Skip tests\n"
			printf "\t--no-lint		Skip linting\n"
			exit 0
      	;;
      --no-test)
			no_test=1
			shift
      	;;
      --no-lint)
			no_lint=1
			shift
			;;
		*)
			break
	      ;;
   esac
done


manifests=()
if [[ -z "$1" ]]; then
	manifests=(**/stack.yaml)
else
	manifests+=("$1/stack.yaml")
fi

green='\033[1;32m'
no_color='\033[0m'
for m in "${manifests[@]}"; do
	name="$(dirname "$(readlink -f "$m")")"
	name="$(basename "$name")"

	printf "Project dir: %b%s%b\n" "$green" "$name" "$no_color"

	if (( no_test == 0 )); then
		stack --work-dir . --stack-yaml "$m" test --ghc-options="-Wall -Werror"
	fi

	if (( no_lint == 0 )); then
		hlint "$name"
	fi
done