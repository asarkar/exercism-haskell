#!/bin/bash

set -e

no_test=0
no_lint=0
stack_opts='--resolver lts --verbosity warn --work-dir .'

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
if [[ -z "$CI" ]]; then
	ormolu_mode="inplace"
fi

trm="xterm-256color"
red=$(tput -T"$trm" setaf 1)
green=$(tput -T"$trm" setaf 2)
default=$(tput -T"$trm" sgr0)
for m in "${manifests[@]}"; do
	name="$(dirname "$(readlink -f "$m")")"
	name="$(basename "$name")"

	printf "Project dir: %b%s%b\n" "$green" "$name" "$default"

	if (( no_test == 0 )); then
		# profiling https://stackoverflow.com/a/40922201/839733
		stack test $stack_opts --stack-yaml "$m" \
		  --ghc-options "-Wall -Werror -Wno-x-partial"
	fi

	if (( no_lint == 0 )); then
		if [[ -x "$(command -v hlint)" ]]; then
			hlint "$name/src"
		else
			printf "%bhlint not found%b\n" "$red" "$default"
		fi
		
		if [[ -x "$(command -v ormolu)" ]]; then
			ormolu -m "$ormolu_mode" $(find "$name/src" -name '*.hs')
		else
			printf "%bormolu not found%b\n" "$red" "$default"
		fi
	fi
done
