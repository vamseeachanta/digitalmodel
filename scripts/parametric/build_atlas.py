"""Regenerate a workflow's parametric atlas from its registry declaration.

Usage:  uv run python scripts/parametric/build_atlas.py mooring-fatigue

Thin CLI over digitalmodel.parametric.build; the staleness fingerprint and the
build share one source of truth (refresh.py). This is the manual seed of the
refresh job (#799) — `python -m digitalmodel.parametric.refresh --apply`
regenerates every stale atlas using the same core.
"""

from __future__ import annotations

import sys

from digitalmodel.parametric.build import build_atlas_from_registry


def main(workflow_id: str) -> None:
    atlas = build_atlas_from_registry(workflow_id)
    v = atlas.validation
    print(f"atlas_id={atlas.atlas_id}  rows={len(atlas.grid)}")
    print(f"holdout n={v['n_holdout']}  max_rel_error={v['max_rel_error']:.5f}  "
          f"threshold={v['threshold']}  passes={v['passes']}")
    for step in v.get("densification_log", []):
        print(f"  densified: round {step['round']} +knot {step['added_knot']:g} on "
              f"{step['axis']} (was {step['max_rel_error_before']:.5f})")
    if not v["passes"]:
        raise SystemExit(
            "validation FAILED after adaptive densification — raise max_rounds "
            "or revisit the seed grid/tolerance")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "mooring-fatigue")
