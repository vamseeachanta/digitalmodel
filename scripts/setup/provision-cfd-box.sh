#!/usr/bin/env bash
# provision-cfd-box.sh — bring a fresh Ubuntu box to a working CFD-execution
# state matching ace-linux-2, so benchmarks are reproducible (digitalmodel #1495).
#
# Installs (idempotent — safe to re-run):
#   * OpenFOAM ESI v2312 package 2312.260127-2
#   * OpenMPI packages 4.1.6-7ubuntu2
#   * build tools, git, curl
#   * uv                  (astral installer)
#   * gh                  (GitHub CLI, optional — for issues/PRs/scheduled runs)
#   * the digitalmodel repo + its Python deps (the CFD ecosystem)
#
# Usage:
#   scripts/setup/provision-cfd-box.sh                 # full provision + clone + verify
#   REPO_DIR=/data/digitalmodel WORK_DIR=/data/cfd_work scripts/setup/provision-cfd-box.sh
#   SKIP_CLONE=1 scripts/setup/provision-cfd-box.sh    # toolchain only
#
# Env:
#   REPO_DIR   where to clone digitalmodel   (default: $HOME/digitalmodel)
#   WORK_DIR   scratch for CFD case trees    (default: $HOME/cfd_work)
#   REPO_URL   git remote                    (default: https://github.com/vamseeachanta/digitalmodel.git)
#   SKIP_CLONE / SKIP_GH   set to 1 to skip that stage
set -euo pipefail

OPENFOAM_PACKAGE_VERSION="2312.260127-2"
OPENMPI_PACKAGE_VERSION="4.1.6-7ubuntu2"
OPENFOAM_INSTALLER_SHA256="f7fa288327e936b5a85e3e4a0b29bf039c06d214916f39400b830b63a3310b5b"
REPO_URL="${REPO_URL:-https://github.com/vamseeachanta/digitalmodel.git}"
REPO_DIR="${REPO_DIR:-$HOME/digitalmodel}"
ASSETUTILITIES_URL="https://github.com/vamseeachanta/assetutilities.git"
ASSETUTILITIES_COMMIT="993f1b5ddc90b56ecf531bedb1b84f5efe096700"
ASSETUTILITIES_TREE="5f7434fcb3a348a7e04bf9de228e6ce2c49a87cb"
ASSETUTILITIES_DIR="${ASSETUTILITIES_DIR:-$(dirname "$REPO_DIR")/assetutilities}"
WORK_DIR="${WORK_DIR:-$HOME/cfd_work}"
OF_BASHRC="/usr/lib/openfoam/openfoam2312/etc/bashrc"

log()  { printf '\033[1;34m[provision]\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m[provision] WARN:\033[0m %s\n' "$*" >&2; }
die()  { printf '\033[1;31m[provision] ERROR:\033[0m %s\n' "$*" >&2; exit 1; }
have() { command -v "$1" >/dev/null 2>&1; }
package_version() { dpkg-query -W -f='${Version}' "$1" 2>/dev/null || true; }

require_clean_repo() {
  local repo="$1"
  ( cd "$repo" \
      && git diff --quiet \
      && git diff --cached --quiet \
      && [[ -z "$(git ls-files --others --exclude-standard)" ]] ) \
    || die "repository must be clean before frozen CFD environment setup: $repo"
}

verify_pinned_dependency() {
  local repo="$1"
  [[ -d "$repo/.git" ]] || die "assetutilities is not a git checkout: $repo"
  require_clean_repo "$repo"
  [[ "$(git -C "$repo" remote get-url origin)" == "$ASSETUTILITIES_URL" ]] \
    || die "assetutilities origin drift at $repo"
  [[ "$(git -C "$repo" rev-parse HEAD)" == "$ASSETUTILITIES_COMMIT" ]] \
    || die "assetutilities commit drift at $repo"
  [[ "$(git -C "$repo" rev-parse 'HEAD^{tree}')" == "$ASSETUTILITIES_TREE" ]] \
    || die "assetutilities tree drift at $repo"
}

provision_assetutilities() {
  if [[ -e "$ASSETUTILITIES_DIR" || -L "$ASSETUTILITIES_DIR" ]]; then
    verify_pinned_dependency "$ASSETUTILITIES_DIR"
    return
  fi
  mkdir -p "$(dirname "$ASSETUTILITIES_DIR")"
  log "cloning pinned assetutilities sibling → $ASSETUTILITIES_DIR…"
  git clone -q --no-checkout "$ASSETUTILITIES_URL" "$ASSETUTILITIES_DIR" \
    || die "could not clone pinned assetutilities dependency"
  git -C "$ASSETUTILITIES_DIR" checkout --detach -q "$ASSETUTILITIES_COMMIT" \
    || die "could not checkout pinned assetutilities commit"
  verify_pinned_dependency "$ASSETUTILITIES_DIR"
}

# ---- preflight ------------------------------------------------------------- #
[[ "$(uname -s)" == "Linux" ]] || die "Linux only (this box is $(uname -s))."
if [[ -r /etc/os-release ]]; then . /etc/os-release; fi
log "OS: ${PRETTY_NAME:-unknown}  kernel $(uname -r)  cores $(nproc)  mem $(free -g | awk 'NR==2{print $2}')GB"
have apt-get || die "apt-get not found — this provisioner targets Ubuntu/Debian (a-l-2 is Ubuntu 24.04 'noble')."
[[ "${VERSION_CODENAME:-}" == "noble" ]] || warn "Codename is '${VERSION_CODENAME:-?}', not 'noble' (24.04). The OpenFOAM ESI repo line is pinned to noble; adjust OF repo if this box differs."
SUDO=""; [[ $EUID -ne 0 ]] && SUDO="sudo"
$SUDO true 2>/dev/null || die "Need root or passwordless sudo for apt installs."

# ---- base packages --------------------------------------------------------- #
log "installing base packages (build tools, git, curl)…"
export DEBIAN_FRONTEND=noninteractive
$SUDO apt-get update -qq
$SUDO apt-get install -y -qq \
  build-essential git curl ca-certificates gnupg \
  rclone   # cloud data sync (inputs/outputs/reports) for the headless box

log "installing exact OpenMPI packages…"
$SUDO apt-get install -y -qq --allow-downgrades --allow-change-held-packages \
  "openmpi-bin=${OPENMPI_PACKAGE_VERSION}" \
  "libopenmpi-dev=${OPENMPI_PACKAGE_VERSION}"
[[ "$(package_version openmpi-bin)" == "$OPENMPI_PACKAGE_VERSION" ]] \
  || die "openmpi-bin version does not match $OPENMPI_PACKAGE_VERSION"
[[ "$(package_version libopenmpi-dev)" == "$OPENMPI_PACKAGE_VERSION" ]] \
  || die "libopenmpi-dev version does not match $OPENMPI_PACKAGE_VERSION"
$SUDO apt-mark hold openmpi-bin libopenmpi-dev >/dev/null

# ---- OpenFOAM ESI v2312 --------------------------------------------------- #
if [[ "$(package_version openfoam2312-default)" != "$OPENFOAM_PACKAGE_VERSION" ]]; then
  log "installing exact openfoam2312-default package…"
  installer="$(mktemp)"
  curl -fsSL -o "$installer" https://dl.openfoam.com/add-debian-repo.sh \
    || { rm -f "$installer"; die "OpenFOAM repo installer download failed"; }
  printf '%s  %s\n' "$OPENFOAM_INSTALLER_SHA256" "$installer" \
    | sha256sum -c - >/dev/null \
    || { rm -f "$installer"; die "OpenFOAM repo installer checksum mismatch"; }
  $SUDO bash "$installer"
  rm -f "$installer"
  $SUDO apt-get update -qq
  $SUDO apt-get install -y -qq --allow-downgrades --allow-change-held-packages \
    "openfoam2312-default=${OPENFOAM_PACKAGE_VERSION}"
fi
[[ "$(package_version openfoam2312-default)" == "$OPENFOAM_PACKAGE_VERSION" ]] \
  || die "openfoam2312-default version does not match $OPENFOAM_PACKAGE_VERSION"
[[ -r "$OF_BASHRC" ]] || die "OpenFOAM install did not produce $OF_BASHRC"
$SUDO apt-mark hold openfoam2312-default openmpi-bin libopenmpi-dev >/dev/null

# ---- uv (Python toolchain) ------------------------------------------------- #
if have uv; then
  log "uv present ($(uv --version))."
else
  log "installing uv (astral)…"
  curl -LsSf https://astral.sh/uv/install.sh | sh
  export PATH="$HOME/.local/bin:$PATH"
  have uv || warn "uv installed but not on PATH — add \$HOME/.local/bin to PATH."
fi

# ---- gh (optional) --------------------------------------------------------- #
if [[ "${SKIP_GH:-0}" != "1" ]] && ! have gh; then
  log "installing gh (GitHub CLI)…"
  $SUDO mkdir -p -m 755 /etc/apt/keyrings
  curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg \
    | $SUDO tee /etc/apt/keyrings/githubcli-archive-keyring.gpg >/dev/null
  $SUDO chmod go+r /etc/apt/keyrings/githubcli-archive-keyring.gpg
  echo "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" \
    | $SUDO tee /etc/apt/sources.list.d/github-cli.list >/dev/null
  $SUDO apt-get update -qq && $SUDO apt-get install -y -qq gh || warn "gh install failed (non-fatal)."
fi

# ---- clone the CFD ecosystem + install deps -------------------------------- #
if [[ "${SKIP_CLONE:-0}" != "1" ]]; then
  if [[ -d "$REPO_DIR/.git" ]]; then
    require_clean_repo "$REPO_DIR"
    log "digitalmodel already at $REPO_DIR — fetching latest main…"
    git -C "$REPO_DIR" fetch origin -q && git -C "$REPO_DIR" checkout main -q \
      && git -C "$REPO_DIR" pull --ff-only origin main -q \
      || die "could not fast-forward the clean main checkout"
  else
    log "cloning digitalmodel → $REPO_DIR…"
    git clone -q "$REPO_URL" "$REPO_DIR"
  fi
  require_clean_repo "$REPO_DIR"
  provision_assetutilities
  verify_pinned_dependency "$ASSETUTILITIES_DIR"
  log "installing the locked CFD Python environment…"
  ( cd "$REPO_DIR" \
      && uv sync --frozen --extra cfd --extra test \
      && uv run --frozen --extra cfd python -c \
        'import gmsh; assert gmsh.__version__ == "4.15.1"' )
  mkdir -p "$WORK_DIR"
fi

# ---- summary --------------------------------------------------------------- #
log "DONE. Toolchain summary:"
printf '  OpenFOAM : %s\n' "$(package_version openfoam2312-default)"
printf '  OpenMPI  : %s\n' "$(package_version openmpi-bin)"
printf '  uv       : %s\n' "$(uv --version 2>/dev/null || echo '?')"
printf '  repo     : %s @ %s\n' "$REPO_DIR" "$(git -C "$REPO_DIR" rev-parse --short HEAD 2>/dev/null || echo 'not cloned')"
printf '  dependency: %s @ %s\n' "$ASSETUTILITIES_DIR" "$(git -C "$ASSETUTILITIES_DIR" rev-parse --short HEAD 2>/dev/null || echo 'not cloned')"
printf '  work dir : %s\n' "$WORK_DIR"
cat <<EOF

Next:
  source $OF_BASHRC
  cd $REPO_DIR
  scripts/setup/verify-cfd-box.sh                       # smoke + parallel MPI check
  scripts/setup/verify-cfd-box.sh --benchmark $WORK_DIR # + the 3D scaling benchmark (labelled for this box)

HITL (secrets — not automated here):
  gh auth login --with-token < token.txt   # headless: use a PAT, NOT the browser flow
  rclone config                            # create the cloud remote for data sync
  RCLONE_REMOTE=<remote:path> scripts/setup/sync-cfd-data.sh --install-timer 30m  # always-sync
EOF
