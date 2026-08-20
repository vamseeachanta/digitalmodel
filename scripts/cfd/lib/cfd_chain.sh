# shellcheck shell=bash
# Shared contract for the OpenFOAM ship-resistance solve chain (#1173, #2023).
#
# WHY THIS EXISTS. The chain that produced the #1173 KCS results ran entirely
# from untracked scripts on one solve host. Exactly one of the twelve
# (yplus_after.sh) was ever committed, and it referenced eleven absent
# siblings -- so the capability could not be reproduced from a clean checkout,
# and the host's copy had already drifted from the repo's.
#
# Every function here exists because the host-side originals got it wrong at
# least once. Read the comments before "simplifying" any of them.

# ---------------------------------------------------------------------------
# Fatal exit. Always to stderr, always non-zero: a chain step that fails must
# never be mistaken for one that had nothing to do.
cfd_die() { echo "FATAL: $*" >&2; exit 1; }

# ---------------------------------------------------------------------------
# Resolve the case root. NO DEFAULT ABSOLUTE PATH ON PURPOSE:
#   - the originals hard-coded ROOT="$HOME/cfd/dm1173", which is why they only
#     ran on one host and one account;
#   - the repo forbids hard-coded absolute paths
#     (scripts/enforcement/check-no-abs-paths.sh).
# Fail closed when unset: solving into an unintended directory is worse than
# not solving.
cfd_root() {
  [ -n "${DM_CFD_ROOT:-}" ] || cfd_die "DM_CFD_ROOT is unset (export it to the case root)"
  [ -d "$DM_CFD_ROOT" ] || cfd_die "DM_CFD_ROOT does not exist: $DM_CFD_ROOT"
  printf '%s' "$DM_CFD_ROOT"
}

cfd_case_dir() {
  [ -n "${1:-}" ] || cfd_die "cfd_case_dir: no case name"
  printf '%s/kcs_cases/%s' "$(cfd_root)" "$1"
}

# ---------------------------------------------------------------------------
# Timestamped append-only log. UTC because the fleet spans hosts and a local
# timestamp cannot be correlated across them.
cfd_say() { echo "$(date -u +%Y-%m-%dT%H:%M:%SZ) $*" >> "${CFD_LOG:?CFD_LOG unset}"; }

# ---------------------------------------------------------------------------
# Terminal markers, written on success AND failure.
#
# WHY BOTH: a lane that writes a marker only on success makes silence
# ambiguous -- it reads as "still running" and as "died" simultaneously, and
# in practice it reads as success. A 13.5 h zombie poller on this fleet was
# invisible for exactly this reason.
cfd_marker_ok()   { echo "OK $(date -u +%Y-%m-%dT%H:%M:%SZ) $*"     > "${CFD_MARKER:?CFD_MARKER unset}"; }
cfd_marker_fail() { echo "FAILED $(date -u +%Y-%m-%dT%H:%M:%SZ) $*" > "${CFD_MARKER:?CFD_MARKER unset}"; }

# ---------------------------------------------------------------------------
# Load the OpenFOAM environment.
#
# DELIBERATELY tolerant of `set -e` / `set -u`: openfoam2312/etc/bashrc
# dereferences unset variables (WM_PROJECT_DIR) and calls pop_var_context,
# which aborts the shell under either flag. Both have bitten this chain --
# `set -u` first, then `set -e` separately. The flags are saved and restored
# so callers keep their own strictness.
cfd_load_openfoam() {
  local bashrc="${WM_BASHRC:-/usr/lib/openfoam/openfoam2312/etc/bashrc}"
  [ -f "$bashrc" ] || cfd_die "no OpenFOAM bashrc at $bashrc (set WM_BASHRC)"
  local saved="$-"
  set +eu
  # shellcheck disable=SC1090
  source "$bashrc" >/dev/null 2>&1
  case "$saved" in *e*) set -e ;; esac
  case "$saved" in *u*) set -u ;; esac
  [ -n "${WM_PROJECT_DIR:-}" ] || cfd_die "OpenFOAM env did not load from $bashrc"
}

# ---------------------------------------------------------------------------
# Is a solver running?
#
# `pgrep -x` matches the EXECUTABLE NAME. NEVER use `pgrep -f` here: the
# pattern then matches the ssh command line that carries it, so a supervisor
# sees itself and waits forever. That produced a 13.5 h zombie on this fleet,
# and `pkill -f` with the same pattern killed the operator's own session.
cfd_solver_running() { pgrep -x "${1:-interFoam}" >/dev/null 2>&1; }
cfd_solver_ranks()   { pgrep -xc "${1:-interFoam}" 2>/dev/null || echo 0; }

# ---------------------------------------------------------------------------
# Cell count from checkMesh output. Returns empty (not 0) when unknown, so a
# caller cannot mistake "could not read" for "an empty mesh".
cfd_mesh_cells() {
  local log="$1/log.checkMesh"
  [ -f "$log" ] || return 1
  awk '/^ *cells:/ {print $2; exit}' "$log"
}

# ---------------------------------------------------------------------------
# Config accessors. Any non-zero exit from the reader is fatal: an unknown
# case must stop the chain, not silently fall through to a default.
cfd_config_path() { printf '%s' "${DM_CFD_CONFIG:?DM_CFD_CONFIG unset}"; }
cfd_cases()       { python3 "$CFD_LIB/cfd_config.py" "$(cfd_config_path)" cases || cfd_die "cannot read cases"; }
cfd_case_get()    { python3 "$CFD_LIB/cfd_config.py" "$(cfd_config_path)" get "$1" "$2" || cfd_die "cannot read $2 for $1"; }
cfd_decompose_n() { python3 "$CFD_LIB/cfd_config.py" "$(cfd_config_path)" decompose "$1" || cfd_die "no decomposition for $1 ranks"; }

# CFD_LIB is where this file lives, so callers need only source it by path.
CFD_LIB="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export CFD_LIB
