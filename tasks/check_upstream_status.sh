#!/usr/bin/env bash

set -u

target=$1
make_command=$2

case "$target" in
    ../../../*/output/*)
        tasks_root=../../..
        relative=${target#../../../}
        ;;
    ../../*/output/*)
        tasks_root=../..
        relative=${target#../../}
        ;;
    *)
        exit 0
        ;;
esac

task=${relative%%/output/*}
output=../output/${relative#*/output/}

if ! MAKEFLAGS= "$make_command" -q -C "$tasks_root/$task/code" "$output" >/dev/null 2>&1; then
    printf 'FORCE_UPSTREAM'
fi
