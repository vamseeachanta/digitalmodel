#!/usr/bin/env bash
# provision-cfd-box.sh — bring a fresh Ubuntu box to a working CFD-execution
# state matching ace-linux-2, so benchmarks are reproducible (digitalmodel #1495).
#
# Installs (idempotent — safe to re-run):
#   * OpenFOAM ESI v2312  (dl.openfoam.com apt repo; pkg openfoam2312-default)
#   * OpenMPI 4.1.x       (Ubuntu openmpi-bin + libopenmpi-dev)
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
#   OF_VERSION OpenFOAM ESI version          (default: 2312 — MUST match a-l-2 for reproducible benchmarks)
#   SKIP_CLONE / SKIP_GH   set to 1 to skip that stage
set -euo pipefail

OF_VERSION="${OF_VERSION:-2312}"
REPO_URL="${REPO_URL:-https://github.com/vamseeachanta/digitalmodel.git}"
REPO_DIR="${REPO_DIR:-$HOME/digitalmodel}"
WORK_DIR="${WORK_DIR:-$HOME/cfd_work}"
OF_BASHRC="/usr/lib/openfoam/openfoam${OF_VERSION}/etc/bashrc"

log()  { printf '\033[1;34m[provision]\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m[provision] WARN:\033[0m %s\n' "$*" >&2; }
die()  { printf '\033[1;31m[provision] ERROR:\033[0m %s\n' "$*" >&2; exit 1; }
have() { command -v "$1" >/dev/null 2>&1; }

# ---- preflight ------------------------------------------------------------- #
[[ "$(uname -s)" == "Linux" ]] || die "Linux only (this box is $(uname -s))."
if [[ -r /etc/os-release ]]; then . /etc/os-release; fi
log "OS: ${PRETTY_NAME:-unknown}  kernel $(uname -r)  cores $(nproc)  mem $(free -g | awk 'NR==2{print $2}')GB"
have apt-get || die "apt-get not found — this provisioner targets Ubuntu/Debian (a-l-2 is Ubuntu 24.04 'noble')."
[[ "${VERSION_CODENAME:-}" == "noble" ]] || warn "Codename is '${VERSION_CODENAME:-?}', not 'noble' (24.04). The OpenFOAM ESI repo line is pinned to noble; adjust OF repo if this box differs."
SUDO=""; [[ $EUID -ne 0 ]] && SUDO="sudo"
$SUDO true 2>/dev/null || die "Need root or passwordless sudo for apt installs."

# ---- base packages --------------------------------------------------------- #
log "installing base packages (build tools, git, curl, OpenMPI)…"
export DEBIAN_FRONTEND=noninteractive
$SUDO apt-get update -qq
$SUDO apt-get install -y -qq \
  build-essential git curl ca-certificates gnupg \
  openmpi-bin libopenmpi-dev

# ---- OpenFOAM ESI v${OF_VERSION} ------------------------------------------- #
if [[ -r "$OF_BASHRC" ]]; then
  log "OpenFOAM v${OF_VERSION} already present ($OF_BASHRC) — skipping install."
else
  log "adding the OpenFOAM ESI apt repo (dl.openfoam.com) and installing openfoam${OF_VERSION}-default…"
  # ESI's canonical repo installer (adds signed repo for the current codename).
  curl -fsSL https://dl.openfoam.com/add-debian-repo.sh | $SUDO bash
  $SUDO apt-get update -qq
  $SUDO apt-get install -y -qq "openfoam${OF_VERSION}-default"
  [[ -r "$OF_BASHRC" ]] || die "OpenFOAM install did not produce $OF_BASHRC — check apt output."
fi

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
    log "digitalmodel already at $REPO_DIR — fetching latest main…"
    git -C "$REPO_DIR" fetch origin -q && git -C "$REPO_DIR" checkout main -q \
      && git -C "$REPO_DIR" pull --ff-only origin main -q || warn "could not fast-forward main (local changes?)."
  else
    log "cloning digitalmodel → $REPO_DIR…"
    git clone -q "$REPO_URL" "$REPO_DIR"
  fi
  log "installing Python deps (uv venv + editable install)…"
  ( cd "$REPO_DIR" && uv venv -q && UV_NO_SOURCES=true uv pip install -q -e . \
      && uv pip install -q pytest pyyaml loguru click matplotlib )
  mkdir -p "$WORK_DIR"
fi

# ---- summary --------------------------------------------------------------- #
log "DONE. Toolchain summary:"
printf '  OpenFOAM : %s\n' "$(dpkg -l 2>/dev/null | awk '/openfoam'"$OF_VERSION"'-default/{print $3; exit}' || echo '?')"
printf '  OpenMPI  : %s\n' "$(mpirun --version 2>/dev/null | head -1 || echo '?')"
printf '  uv       : %s\n' "$(uv --version 2>/dev/null || echo '?')"
printf '  repo     : %s @ %s\n' "$REPO_DIR" "$(git -C "$REPO_DIR" rev-parse --short HEAD 2>/dev/null || echo 'not cloned')"
printf '  work dir : %s\n' "$WORK_DIR"
cat <<EOF

Next:
  source $OF_BASHRC
  cd $REPO_DIR
  scripts/setup/verify-cfd-box.sh                       # smoke + parallel MPI check
  scripts/setup/verify-cfd-box.sh --benchmark $WORK_DIR # + the 3D scaling benchmark (labelled for this box)
EOF
