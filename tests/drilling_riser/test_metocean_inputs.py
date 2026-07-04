"""#1282: metocean input reducer + adapters feeding the operating envelope.

All offline — a synthetic NDBC-format fixture drives reducer -> adapter ->
compute_operating_envelope with no network. Verifies the bin-midpoint
convention (bins are upper edges) and the fixture provenance/no-credential rule.
"""
from __future__ import annotations

import json
from pathlib import Path

from digitalmodel.drilling_riser.envelope import (
    CurrentProfile,
    EnvelopeCriteria,
    RiserSection,
    compute_operating_envelope,
    resolve_envelope_criteria,  # noqa: F401 (import-smoke)
)
from digitalmodel.drilling_riser.metocean_inputs import (
    current_to_surface_speeds,
    reduce_to_scatter,
    scatter_to_seastates,
)
from digitalmodel.orcaflex.weather_window import ScatterDiagram

_FIXTURE = Path(__file__).parent / "fixtures" / "metocean_noaa_sample.json"


def _fixture():
    return json.loads(_FIXTURE.read_text())


# -- reducer -------------------------------------------------------------------


def test_reduce_bins_records_into_scatter():
    records = [
        {"wave_height": 1.2, "wave_period": 6.4},   # Hs bin (1.0,1.5], Tp bin (6,7]
        {"wave_height": 1.3, "wave_period": 6.9},   # same cell
        {"wave_height": 3.3, "wave_period": 9.4},   # Hs (3.0,3.5], Tp (9,10]
    ]
    sc = reduce_to_scatter(records)
    assert isinstance(sc, ScatterDiagram)
    # hs_bins=[0.5,1.0,1.5,...]; Hs 1.2/1.3 -> index 2 (bin (1.0,1.5])
    # tp_bins=[4,5,6,7,...];    Tp 6.4/6.9 -> index 3 (bin (6,7])
    assert sc.occurrences[2][3] == 2.0
    # Hs 3.3 -> index 6 (bin (3.0,3.5]); Tp 9.4 -> index 6 (bin (9,10])
    assert sc.occurrences[6][6] == 1.0
    assert sum(sum(row) for row in sc.occurrences) == 3.0


def test_reduce_skips_missing_and_out_of_range():
    records = [
        {"wave_height": None, "wave_period": 7.0},       # missing Hs
        {"wave_height": 2.0},                             # missing Tp
        {"wave_height": 99.0, "wave_period": 8.0},        # Hs above top edge
        {"wave_height": 2.0, "wave_period": 99.0},        # Tp above top edge
        {"wave_height": 2.0, "wave_period": 8.0},         # the one valid record
    ]
    sc = reduce_to_scatter(records)
    assert sum(sum(row) for row in sc.occurrences) == 1.0


def test_reduce_honours_tp_key_for_noaa_records():
    # NOAA raw records use dominant_wave_period, not wave_period
    records = _fixture()["records"]
    sc = reduce_to_scatter(records, tp_key="dominant_wave_period")
    # 10 records, one has null Hs -> 9 binned
    assert sum(sum(row) for row in sc.occurrences) == 9.0


# -- adapter (midpoint convention) ---------------------------------------------


def test_scatter_to_seastates_emits_bin_midpoints():
    sc = ScatterDiagram(hs_bins=[1.0, 2.0], tp_bins=[6.0, 8.0],
                        occurrences=[[1.0, 0.0], [0.0, 3.0]])
    ss = scatter_to_seastates(sc)
    # occupied cells: (Hs bin0 [0,1]->mid 0.5, Tp bin0 [0,6]->mid 3.0) and
    #                 (Hs bin1 [1,2]->mid 1.5, Tp bin1 [6,8]->mid 7.0)
    got = sorted((s.hs_m, s.tp_s) for s in ss)
    assert got == [(0.5, 3.0), (1.5, 7.0)]


def test_scatter_to_seastates_skips_empty_cells():
    sc = ScatterDiagram(hs_bins=[1.0, 2.0], tp_bins=[6.0, 8.0],
                        occurrences=[[0.0, 0.0], [0.0, 0.0]])
    assert scatter_to_seastates(sc) == []


# -- current adapter -----------------------------------------------------------


def test_current_to_surface_speeds():
    a = CurrentProfile(surface_speed_mps=1.0)
    b = CurrentProfile(surface_speed_mps=1.5, depths_m=(0.0, 100.0), speeds_mps=(1.5, 0.6),
                       current_type="loop")
    assert current_to_surface_speeds(a) == [1.0]
    assert current_to_surface_speeds([a, b]) == [1.0, 1.5]
    # dormant depth structure is carried but does not change the surface speed
    assert b.depths_m == (0.0, 100.0) and b.current_type == "loop"


# -- end-to-end: fixture -> reducer -> adapter -> envelope (offline) ------------


def test_fixture_drives_the_envelope_offline():
    records = _fixture()["records"]
    scatter = reduce_to_scatter(records, tp_key="dominant_wave_period")
    seastates = scatter_to_seastates(scatter)
    assert seastates  # non-empty design-case set
    section = RiserSection(outer_diameter_m=0.5334, wall_thickness_m=0.0254)
    criteria = EnvelopeCriteria(2.0, 4.0, 0.67)
    speeds = current_to_surface_speeds(CurrentProfile(surface_speed_mps=1.0))
    result = compute_operating_envelope(
        section=section, water_depth_m=1500.0, length_m=1500.0, tension_n=3.0e6,
        criteria=criteria, offsets_pct=[2.0], current_speeds_mps=speeds, seastates=seastates,
    )
    assert result.allowable_mask.shape == (1, 1, len(seastates))


# -- governance: fixture carries provenance + no credentials -------------------


def test_fixture_provenance_and_no_credentials():
    fx = _fixture()
    prov = fx["provenance"]
    assert prov["source"] and prov["licence"]
    raw = _FIXTURE.read_text().lower()
    for forbidden in ("authorization", "x-api-key", "password", "token", "signature=", "aws"):
        assert forbidden not in raw
