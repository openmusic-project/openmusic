#!/usr/bin/env bash
# OpenMusic on SBCL: Linux release (deliver)
set -euo pipefail
trap 'printf "FAILED: see the output above.\n" >&2' ERR
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)
build_dir=$(CDPATH= cd -- "$script_dir/.." && pwd -P)
repo_root=$(CDPATH= cd -- "$script_dir/../../../.." && pwd -P)
om_root=$repo_root/OPENMUSIC
if [[ "$script_dir" != "$om_root/build/sbcl/linux" ]]; then
    printf '%s\n' 'This file must stay in OPENMUSIC/build/sbcl/linux/ of the repository.' >&2
    exit 1
fi
cd -- "$script_dir"

shopt -s nullglob
for image in "$om_root"/OM_* "$om_root"/openmusic; do
    if [[ ! -e "$image" && ! -L "$image" ]]; then
        continue
    fi
    if [[ -L "$image" || ! -f "$image" ]]; then
        printf 'Invalid executable: %s\n' "$image" >&2
        exit 1
    fi
    printf '=== Removing the previous executable %s\n' "$image"
    rm -f -- "$image"
done

printf '%s\n' '=== GUI executable'
sbcl --dynamic-space-size 8192 --load "$build_dir/deliver-sbcl.lisp"
printf '%s\n' '=== Emacs-mode executable'
sbcl --dynamic-space-size 8192 --eval '(defvar cl-user::*emacs-mode* t)' --load "$build_dir/deliver-sbcl.lisp"
printf '%s\n' 'DONE!'
