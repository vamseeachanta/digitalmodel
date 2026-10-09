#!/usr/bin/env bash
# Build only authenticated LibreDWG 0.14 into a user-owned, versioned prefix.
# Trust source: https://www.gnu.org/software/security/ (official GNU keyring).
# Requires curl, GnuPG gpgv, a C compiler, make, tar, xz and pkg-config.
# Build sources and logs are retained in the printed temporary directory.
set -euo pipefail

version=0.14
archive_sha256=62ebb73b984f865960f20ed26619ea5f8789d5e3fd088fa40a2598384da81275
signer_fingerprint=38A4167B0DB69E49C5F7216CB8C28866AB27A7A2
release_url="https://github.com/LibreDWG/libredwg/releases/download/${version}/libredwg-${version}.tar.xz"

fail() {
    printf 'LibreDWG build blocked: %s\n' "$1" >&2
    exit 1
}

for dependency in curl gpgv sha256sum tar xz make gcc pkg-config awk mktemp; do
    command -v "$dependency" >/dev/null || fail "missing dependency: $dependency"
done
[[ "${HOME:-}" == /* && -d "$HOME" ]] || fail 'HOME must be an existing absolute directory'
prefix="$HOME/.local/opt/libredwg-${version}"
[[ ! -e "$prefix" && ! -L "$prefix" ]] || fail 'installation prefix already exists; preserve it'
build_dir=$(mktemp -d "${TMPDIR:-/tmp}/libredwg-${version}.XXXXXXXX")
printf 'Build directory (retained): %s\n' "$build_dir"
archive="$build_dir/libredwg-${version}.tar.xz"

curl --fail --silent --show-error --location --proto '=https' --proto-redir '=https' \
    "$release_url" --output "$archive"
printf '%s  %s\n' "$archive_sha256" "$archive" | sha256sum --check --status \
    || fail 'archive checksum does not match pinned SHA-256'
# The 0.14 release publishes a detached signature. Failure to obtain or verify
# it must never downgrade this build to checksum-only authentication.
curl --fail --silent --show-error --location --proto '=https' --proto-redir '=https' \
    "${release_url}.sig" --output "${archive}.sig" \
    || fail 'release signature download failed'
curl --fail --silent --show-error --location --proto '=https' --proto-redir '=https' \
    https://ftp.gnu.org/gnu/gnu-keyring.gpg --output "$build_dir/gnu-keyring.gpg"
mkdir "$build_dir/gnupg"
chmod 700 "$build_dir/gnupg"
# Suppress identity-bearing diagnostics; expose only the fingerprint verdict.
gpgv --homedir "$build_dir/gnupg" --keyring "$build_dir/gnu-keyring.gpg" \
    --status-fd 1 "${archive}.sig" "$archive" \
    > "$build_dir/signature.status" 2>/dev/null || fail 'release signature verification failed'
awk -v signer="$signer_fingerprint" \
    '$1 == "[GNUPG:]" && $2 == "VALIDSIG" && $3 == signer {valid=1} END {exit !valid}' \
    "$build_dir/signature.status" || fail 'release signature has an unexpected signer'
printf 'Verified archive SHA-256 and GNU signing fingerprint: %s\n' "$signer_fingerprint"

tar -xJf "$archive" -C "$build_dir"
cd "$build_dir/libredwg-${version}"
./configure --prefix="$prefix" --disable-bindings --disable-shared \
    > "$build_dir/configure.log" 2>&1
make -j2 > "$build_dir/build.log" 2>&1
make install > "$build_dir/install.log" 2>&1
printf 'LibreDWG %s installed: %s/bin\n' "$version" "$prefix"
