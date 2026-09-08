#!/usr/bin/env bash
# Reference warm-aware case launcher.
# Usage: solve_chain_warm_aware.sh CASE COMMAND [ARG ...]
set -euo pipefail

CASE=${1:?usage: solve_chain_warm_aware.sh CASE COMMAND [ARG ...]}
shift
[ "$#" -gt 0 ] || { echo "solver command required" >&2; exit 2; }
cd "$CASE"
LOG=${WARM_CHAIN_LOG:-log.warm_chain}

if [ -f WARM_FIELDS ] && grep -a -q "internalField *nonuniform" 0/U 2>/dev/null; then
    touch WARM_FIELDS_KEPT
    echo "warm fields kept" >> "$LOG"
else
    rm -rf 0
    cp -a 0.orig 0
    if [ -f system/setFieldsDict ]; then
        setFields > log.setFields 2>&1 < /dev/null
    fi
    echo "cold fields initialised" >> "$LOG"
fi

exec "$@"
