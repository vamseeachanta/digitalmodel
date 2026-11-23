#!/usr/bin/env bash
# ensure-worktree-deps.sh — make path-dependency siblings resolvable from agent
# worktrees under .claude/worktrees/ (digitalmodel).
#
# digitalmodel's pyproject pins path deps relative to the checkout, e.g.
#   assetutilities = { path = "../assetutilities", editable = true }
# From a worktree at .claude/worktrees/agent-XXXX/ the string `../assetutilities`
# resolves to .claude/worktrees/assetutilities (nonexistent) instead of the real
# sibling next to the main checkout — so `uv run` / pytest fail to build in any
# worktree. Create a symlink at .claude/worktrees/<dep> pointing at the real
# sibling so the relative path resolves for every worktree. Idempotent; safe to
# run repeatedly. .claude/worktrees/ is gitignored, so the link is never tracked.
#
# Usage: scripts/dev/ensure-worktree-deps.sh    # run from anywhere in the repo
# Exit 0 always for present siblings (missing siblings warn but don't fail — you
# may simply not have that sibling checked out).

set -euo pipefail

# Path-dep siblings to bridge (basename == pyproject `path = "../<name>"`).
DEPS=(assetutilities)

# Resolve the MAIN worktree root (first `git worktree list` entry), which is
# robust whether we're invoked from the main checkout or a nested worktree.
main_root="$(git worktree list --porcelain 2>/dev/null | awk '/^worktree /{print $2; exit}')"
if [[ -z "${main_root:-}" ]]; then
  main_root="$(git rev-parse --show-toplevel 2>/dev/null || true)"
fi
[[ -n "${main_root:-}" ]] || { echo "ensure-worktree-deps: not inside a git repo" >&2; exit 2; }

wt_dir="${main_root}/.claude/worktrees"

for dep in "${DEPS[@]}"; do
  target="$(cd "${main_root}/../${dep}" 2>/dev/null && pwd || true)"
  link="${wt_dir}/${dep}"
  if [[ -z "${target}" ]]; then
    echo "ensure-worktree-deps: sibling '${dep}' not found beside ${main_root} — skipping" >&2
    continue
  fi
  mkdir -p "${wt_dir}"
  if [[ -L "${link}" && "$(readlink "${link}")" == "${target}" ]]; then
    echo "ensure-worktree-deps: ${dep} -> ${target} (already linked)"
  else
    ln -sfn "${target}" "${link}"
    echo "ensure-worktree-deps: linked ${dep} -> ${target}"
  fi
done
