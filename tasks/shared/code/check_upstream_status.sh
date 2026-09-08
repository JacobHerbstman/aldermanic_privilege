#!/usr/bin/env bash

set -u

target=$1
make_command=$2

case "$target" in
    ../tasks/*/output/*|../tasks/*/report/*)
        tasks_root=../tasks
        relative=${target#../tasks/}
        ;;
    ../../../*/output/*|../../../*/report/*)
        tasks_root=../../..
        relative=${target#../../../}
        ;;
    ../../*/output/*|../../*/report/*)
        tasks_root=../..
        relative=${target#../../}
        ;;
    *)
        exit 0
        ;;
esac

case "$relative" in
    */output/*) task=${relative%%/output/*}; output=../output/${relative#*/output/} ;;
    */report/*) task=${relative%%/report/*}; output=../report/${relative#*/report/} ;;
esac

if ! MAKEFLAGS= "$make_command" -q -C "$tasks_root/$task/code" "$output" >/dev/null 2>&1; then
    printf 'FORCE_UPSTREAM'
fi
