"""Build the drilling-riser operability atlas (#1283, epic #1279 child E).

An EXACT node-cache of the open analytical operating envelope
(drilling_riser.envelope.compute_operating_envelope, dynamic=False) over
(config x offset_pct x current_speed_mps). Offline + deterministic — no wiki,
no licensed solver. Rerunning reproduces the atlas byte-identically.

Usage:  python scripts/parametric/build_drilling_riser_operability_atlas.py
"""
from __future__ import annotations

from pathlib import Path

from digitalmodel.drilling_riser.operability_atlas import REPO_ROOT, build_atlas


def main() -> None:
    atlas = build_atlas()
    out = atlas.save(REPO_ROOT / "atlases")
    v = atlas.validation
    print(f"atlas_id={atlas.atlas_id}  rows={len(atlas.grid)}  "
          f"configs={len(atlas.categorical_axis.values)}")
    print(f"max_rel_error={v['max_rel_error']:.4f}  passes={v['passes']}")
    print(f"written -> {out}")


if __name__ == "__main__":
    main()
