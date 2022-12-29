#!/bin/bash

set -e

no_test=0
no_lint=0
stack_opts='--resolver lts --work-dir .'
ghc_opts='-Wall -Werror'

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
		--profile)
			stack_opts="--profile --force-dirty $stack_opts"
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

ormolu_mode="check"
if [[ "$OSTYPE" == "darwin"* ]]; then
	ormolu_mode="inplace"
fi

green='\033[1;32m'
no_color='\033[0m'
for m in "${manifests[@]}"; do
	name="$(dirname "$(readlink -f "$m")")"
	name="$(basename "$name")"

	printf "Project dir: %b%s%b\n" "$green" "$name" "$no_color"

	if (( no_test == 0 )); then
		# profiling https://stackoverflow.com/a/40922201/839733
		stack $stack_opts --stack-yaml "$m" test --ghc-options="$ghc_opts"
	fi

	if (( no_lint == 0 )); then
		if [[ -x "$(command -v hlint)" ]]; then
			hlint "$name/src"
		else
			printf "hlint not found"
		fi
		
		if [[ -x "$(command -v ormolu)" ]]; then
			ormolu -m "$ormolu_mode" $(find "$name/src" -name '*.hs')
		else
			printf "ormolu not found"
		fi
	fi
done