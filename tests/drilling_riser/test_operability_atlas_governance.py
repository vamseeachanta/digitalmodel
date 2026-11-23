"""#1283 governance gate for the drilling-riser operability atlas (PUBLIC MIT).

The committed atlas's grid.parquet is binary numbers; its config tokens surface as
TEXT in manifest.yaml + surrogate.json. These checks run in PUBLIC CI (NO wiki):
  (a) a STRUCTURAL scan over the atlas manifest + surrogate (paths/URLs/emails),
      resolving the atlas id DYNAMICALLY via default.txt (never a hardcoded content
      hash, which changes each rebuild -> silent skip);
  (b) the config tokens equal a reviewed generic allowlist (no client token via a
      YAML edit);
  (c) every config numeric constant + axis knot is on a coarse public band grid
      (k-anonymity: no real project depth/tension);
  (d) a planted structural token in BOTH echo sites (manifest + surrogate) is caught.

Name-token coverage (client/rig/field names) is the in-context provenance tripwire
(tests/riser_database/test_provenance_tripwire.py, wiki-gated); this file does NOT
depend on the wiki.
"""
from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.drilling_riser import operability_atlas as oa
from tests.riser_database.test_leak_gate import STRUCTURAL_PATTERNS

_ATLAS_BASE = oa.REPO_ROOT / "atlases" / oa.BASENAME


def _atlas_dir() -> Path:
    """Resolve the live atlas dir DYNAMICALLY via default.txt — never hardcode the
    content-hash id (it changes each rebuild)."""
    atlas_id = (_ATLAS_BASE / "default.txt").read_text().strip()
    d = _ATLAS_BASE / atlas_id
    assert d.is_dir(), f"atlas dir {d} missing — build the atlas"
    return d


def _scan_text(text: str) -> list[str]:
    return [label for pattern, label in STRUCTURAL_PATTERNS if pattern.search(text)]


def test_atlas_text_files_are_structurally_clean():
    d = _atlas_dir()
    for fname in ("manifest.yaml", "surrogate.json"):
        hits = _scan_text((d / fname).read_text())
        assert not hits, f"structural leak in {fname}: {hits}"


def test_config_tokens_equal_reviewed_allowlist():
    surrogate = yaml.safe_load((_atlas_dir() / "manifest.yaml").read_text())
    config_axis = next(a for a in surrogate["axes"] if a["name"] == "config")
    assert tuple(config_axis["values"]) == oa.ALLOWED_TOKENS, (
        "atlas config tokens drifted from the reviewed generic allowlist")


def test_config_constants_and_knots_are_banded():
    cfg = oa.load_config()

    def _on_band(value: float, band: float) -> bool:
        return abs(value / band - round(value / band)) < 1e-9

    for name, st in cfg["stackups"].items():
        assert st["outer_diameter_m"] in oa.ALLOWED_OD_M, name
        assert st["wall_thickness_m"] in oa.ALLOWED_WT_M, name
        for key in ("water_depth_m", "length_m", "tension_n", "tj_stroke_m"):
            assert _on_band(float(st[key]), oa.BAND_GRID[key]), f"{name}.{key} off-band"
        if "moonpool_half_min_m" in st:
            assert _on_band(float(st["moonpool_half_min_m"]), oa.BAND_GRID["moonpool_half_min_m"])
    for knot in cfg["axes"]["offset_pct"]:
        assert _on_band(float(knot), oa.BAND_GRID["offset_pct"])
    for knot in cfg["axes"]["current_speed_mps"]:
        assert _on_band(float(knot), oa.BAND_GRID["current_speed_mps"])


def test_planted_structural_token_is_caught_in_both_echo_sites(tmp_path):
    """Proves the structural scan actually covers manifest.yaml AND surrogate.json
    (both places the config tokens echo), not a .py stand-in."""
    d = _atlas_dir()
    for fname in ("manifest.yaml", "surrogate.json"):
        clean = (d / fname).read_text()
        assert not _scan_text(clean)  # clean today
        planted = clean + "\n# leaked: /mnt/ace/client_projects/secret\n"
        assert _scan_text(planted), f"structural scan failed to catch a planted path in {fname}"
