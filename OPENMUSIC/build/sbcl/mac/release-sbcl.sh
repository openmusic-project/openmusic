#!/usr/bin/env bash
# OpenMusic on SBCL: macOS release (deliver, pack, sign, DMG)
set -euo pipefail
trap 'printf "FAILED: see the output above.\n" >&2' ERR
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd -P)
build_dir=$(CDPATH= cd -- "$script_dir/.." && pwd -P)
repo_root=$(CDPATH= cd -- "$script_dir/../../../.." && pwd -P)
om_root=$repo_root/OPENMUSIC
if [[ "$script_dir" != "$om_root/build/sbcl/mac" ]]; then
    printf '%s\n' 'This file must stay in OPENMUSIC/build/sbcl/mac/ of the repository.' >&2
    exit 1
fi
cd -- "$script_dir"

action=${1:-all}

deliver_app() {
    local old
    shopt -s nullglob
    for old in "$repo_root"/"OM "*" SBCL"/*.app; do
        printf '=== Removing the previous bundle %s\n' "$old"
        rm -rf -- "$old"
    done
    printf '%s\n' '=== GUI executable'
    sbcl --dynamic-space-size 8192 --no-userinit --load "$build_dir/deliver-sbcl.lisp"
    printf '%s\n' '=== Emacs-mode executable'
    sbcl --dynamic-space-size 8192 --no-userinit --eval '(defvar cl-user::*emacs-mode* t)' --load "$build_dir/deliver-sbcl.lisp"
}

pack_app() {
    printf '%s\n' '=== Pack'
    sbcl --dynamic-space-size 8192 --no-userinit --load "$build_dir/pack-om-sbcl.lisp"
}

sign_app() {
    printf '%s\n' '=== Sign'
    local folder app identity
    folder=$(release_folder)
    shopt -s nullglob
    local apps=("$folder"/*.app)
    if (( ${#apps[@]} != 1 )); then
        printf 'Expected one .app in %s\n' "$folder" >&2
        exit 1
    fi
    app=${apps[0]}
    identity=${OM_CODESIGN_IDENTITY:--}
    printf 'Signing %s with identity %s\n' "$app" "$identity"
    sbcl --noinform --disable-debugger --no-userinit \
         --eval '(require :asdf)' \
         --eval "(asdf:initialize-source-registry '(:source-registry (:tree \"$om_root/code/api/om-SBCL/alloy-desktop/DEPENDENCIES/\") :ignore-inherited-configuration))" \
         --eval '(asdf:load-system "deploy")' \
         --eval "(deploy:sign-app #p\"$app/\" \"$identity\" :entitlements #p\"$script_dir/entitlements-om.plist\")" \
         --eval '(sb-ext:quit)'
    codesign --verify --deep --strict --verbose=2 "$app"
    spctl --assess --type execute --verbose=2 "$app" || true
}

release_folder() {
    local folder
    shopt -s nullglob
    local folders=("$repo_root"/"OM "*" SBCL")
    if (( ${#folders[@]} != 1 )); then
        printf 'Expected one "OM <version> SBCL" folder next to %s\n' "$repo_root" >&2
        exit 1
    fi
    folder=$(CDPATH= cd -- "${folders[0]}" && pwd -P)
    printf '%s' "$folder"
}

create_dmg() {
    printf '%s\n' '=== DMG'
    local folder name dmg
    folder=$(release_folder)
    name=$(basename -- "$folder")
    dmg=$(dirname -- "$folder")/${name// /-}.dmg
    rm -f -- "$dmg"
    hdiutil create -fs HFS+ -volname "$name" -srcfolder "$folder" "$dmg"
    printf 'DMG: %s\n' "$dmg"
}

case "$action" in
    -d)   deliver_app ;;
    -p)   pack_app ;;
    -s)   sign_app ;;
    -dmg) create_dmg ;;
    all)  deliver_app; pack_app; sign_app; create_dmg ;;
    *)    printf 'Options: -d (deliver) -p (pack) -s (sign) -dmg (DMG); no option = all\n' >&2; exit 1 ;;
esac
printf '%s\n' 'DONE!'
