"""#1283: drilling-riser operability EXACT node-cache atlas + screening consumer.

Offline — the committed atlas is built from the open analytical operating envelope
(compute_operating_envelope, dynamic=False) with public standard-factor criteria;
NO wiki. Verifies node-exactness, deterministic rebuild, out-of-range + boundary
escalation, the all-NaN-node guard, and the screening consumer.
"""
from __future__ import annotations

from pathlib import Path
from unittest import mock

import numpy as np
import pytest

from digitalmodel.drilling_riser import operability_atlas as oa
from digitalmodel.drilling_riser.operability_screening import (
    ESCALATE,
    INOPERABLE,
    OPERABLE,
    screen_operability,
)
from digitalmodel.parametric.atlas import Atlas

_ATLAS_ROOT = oa.REPO_ROOT / "atlases"


def _atlas() -> Atlas:
    return Atlas.load(_ATLAS_ROOT, oa.BASENAME)


# -- node exactness ------------------------------------------------------------


def test_committed_atlas_nodes_are_exact():
    """Every stored grid node equals the exact governing utilisation (the cache is
    exact, not a fitted approximation of the nodes)."""
    cfg = oa.load_config()
    atlas = _atlas()
    # sample a spread of nodes across configs/axes
    sample = atlas.grid.iloc[:: max(1, len(atlas.grid) // 25)]
    for _, row in sample.iterrows():
        point = {"config": row["config"], "offset_pct": row["offset_pct"],
                 "current_speed_mps": row["current_speed_mps"]}
        exact = oa.governing_utilisation(point, cfg=cfg)
        assert row[oa.RESPONSE] == pytest.approx(exact, abs=1e-9)


def test_predict_returns_stored_value_at_a_node():
    atlas = _atlas()
    row = atlas.grid.iloc[len(atlas.grid) // 3]
    p = atlas.predict({"config": row["config"], "offset_pct": row["offset_pct"],
                       "current_speed_mps": row["current_speed_mps"]})
    assert p.in_range
    assert p.value == pytest.approx(row[oa.RESPONSE], abs=1e-9)


def test_governing_utilisation_matches_direct_envelope_call():
    """The response == nanmax over the mode's active-limit utilisation from a
    direct compute_operating_envelope call (capped at the ceiling)."""
    from digitalmodel.drilling_riser.envelope import (
        EnvelopeCriteria, OperatingMode, RigEnvelopeLimits, RiserSection, SeaState,
        compute_operating_envelope,
    )
    cfg = oa.load_config()
    st = cfg["stackups"]["rsu_a"]
    crit = cfg["criteria"]
    res = compute_operating_envelope(
        section=RiserSection(st["outer_diameter_m"], st["wall_thickness_m"]),
        water_depth_m=st["water_depth_m"], length_m=st["length_m"], tension_n=st["tension_n"],
        criteria=EnvelopeCriteria(crit["flexjoint_angle_mean_deg"],
                                  crit["flexjoint_angle_max_deg"], crit["von_mises_design_factor"]),
        offsets_pct=[4.0], current_speeds_mps=[0.5], seastates=[SeaState(0.0, 1.0)],
        rig_limits=RigEnvelopeLimits(tj_stroke_m=st["tj_stroke_m"]),
        mode=OperatingMode("drilling"),
    )
    cell = [res.per_limit_utilisation[k][0, 0, 0] for k in res.per_limit_utilisation]
    expected = min(float(np.nanmax(cell)), oa.UTILISATION_CEILING)
    got = oa.governing_utilisation(
        {"config": "rsu_a__drilling", "offset_pct": 4.0, "current_speed_mps": 0.5}, cfg=cfg)
    assert got == pytest.approx(expected, abs=1e-9)


def test_all_nan_node_is_guarded():
    """A node whose active-limit set yields all-NaN raises (never silently 0/NaN)."""
    cfg = oa.load_config()
    fake = mock.Mock()
    fake.per_limit_utilisation = {"von_mises": np.full((1, 1, 1), np.nan),
                                  "stroke": np.full((1, 1, 1), np.nan)}
    with mock.patch.object(oa, "compute_operating_envelope", return_value=fake):
        with pytest.raises(ValueError, match="all-NaN"):
            oa.governing_utilisation(
                {"config": "rsu_a__hang_off", "offset_pct": 2.0, "current_speed_mps": 0.5}, cfg=cfg)


# -- deterministic rebuild -----------------------------------------------------


def test_rebuild_is_byte_identical(tmp_path):
    atlas = oa.build_atlas()
    out = atlas.save(tmp_path)
    committed_id = (_ATLAS_ROOT / oa.BASENAME / "default.txt").read_text().strip()
    assert atlas.atlas_id == committed_id  # same content fingerprint
    committed = _ATLAS_ROOT / oa.BASENAME / committed_id
    for fname in ("grid.parquet", "surrogate.json"):
        assert (out / fname).read_bytes() == (committed / fname).read_bytes(), fname
    # the recomputed fingerprint matches the committed provenance
    assert atlas.provenance["content_fingerprint"] == _atlas().provenance["content_fingerprint"]
    assert oa.content_fingerprint() == _atlas().provenance["content_fingerprint"]


# -- escalation contract -------------------------------------------------------


def test_out_of_range_escalates_never_clamps():
    # off-grid offset (beyond the max knot) -> escalate, no value
    s = screen_operability("rsu_a__drilling", 99.0, 0.5, atlas_root=_ATLAS_ROOT)
    assert s.light == ESCALATE and not s.in_range and s.governing_utilisation is None
    # unknown config -> escalate
    s2 = screen_operability("client_rig__drilling", 2.0, 0.5, atlas_root=_ATLAS_ROOT)
    assert s2.light == ESCALATE and not s2.in_range


def test_boundary_point_escalates_within_local_error_band():
    """A point whose interpolated UC band straddles 1.0 escalates (never a smooth
    interpolated operable/inoperable call)."""
    atlas = _atlas()
    cfg = oa.load_config()
    # find an off-node point (an offset midpoint) whose true UC is nearest to 1.0
    best = None
    for off in [x + 0.5 for x in range(0, 10)]:
        for cur in [0.25, 0.75, 1.25]:
            uc = oa.governing_utilisation(
                {"config": "rsu_a__drilling", "offset_pct": off, "current_speed_mps": cur}, cfg=cfg)
            d = abs(uc - 1.0)
            if uc < oa.UTILISATION_CEILING and (best is None or d < best[0]):
                best = (d, off, cur)
    _, off, cur = best
    s = screen_operability("rsu_a__drilling", off, cur, atlas_root=_ATLAS_ROOT)
    assert s.light == ESCALATE, f"near-boundary point off={off} cur={cur} should escalate, got {s}"


def test_consumer_grades_operable_to_inoperable():
    lo = screen_operability("rsu_a__drilling", 0.0, 0.0, atlas_root=_ATLAS_ROOT)
    hi = screen_operability("rsu_a__drilling", 10.0, 2.0, atlas_root=_ATLAS_ROOT)
    assert lo.light == OPERABLE and lo.governing_utilisation < 1.0
    assert hi.light == INOPERABLE and hi.governing_utilisation > 1.0
    # provenance surfaced (standards + tier + disclaimer), never the limit values
    assert lo.provenance["atlas_id"] and lo.provenance["standards"]
    assert "solver" not in lo.provenance
