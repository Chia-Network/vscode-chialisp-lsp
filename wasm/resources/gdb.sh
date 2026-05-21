#!/bin/sh
set -eu

arg_count=$#
while [ "$arg_count" -gt 0 ]; do
    arg=$1
    shift
    arg_count=$((arg_count - 1))
    if [ "$arg" != "--interpreter=mi" ]; then
        set -- "$@" "$arg"
    fi
done

elf_file=
for candidate in /mnt/*.arm.elf /mnt/.chialisp-debug/build/*.arm.elf; do
    if [ -f "$candidate" ]; then
        elf_file=$candidate
        break
    fi
done

if [ -n "$elf_file" ]; then
    set -- "$@" --ex "file $elf_file"
fi

set -- "$@" --ex "dir /mnt" --ex "target remote /dev/ttyS1"
cd /mnt
exec gdb "$@"
