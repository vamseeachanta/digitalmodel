"""Fatigue channels of a wave-fatigue window (digitalmodel-data #13, W414; plan damage path): OrcaFlex
RainflowHalfCycles of the axial (ZZ) stress at 8 points around the wall at each station, binned on a range derived
from the observed maximum with no half-cycle out of range, and the peak-to-valley invariant checked per point."""

from __future__ import annotations

import pytest

from digitalmodel.drilling_riser.global_model import fatigue_channels as fc
from digitalmodel.solvers.orcaflex import orcaflex_api

from .conftest import synthetic_spec


def test_histogram_bins_every_half_cycle_on_a_range_from_the_observed_maximum():
    hc = [0.5, 1.0, 1.0, 2.5, 7.3, 7.3, 0.0]
    h = fc.half_cycle_histogram(hc, n_bins=10)
    assert sum(h["counts"]) == len(hc) and h["half_cycles"] == len(hc)
    assert h["max_range"] == 7.3 and h["bin_width"] * 10 >= 7.3
    assert h["counts"][-1] == 2  # the maximum lands in the last bin (upper edge closed)
    assert h["out_of_range"] == 0


def test_histogram_of_no_cycles_is_empty_and_valid():
    h = fc.half_cycle_histogram([], n_bins=10)
    assert h["half_cycles"] == 0 and h["max_range"] == 0.0 and sum(h["counts"]) == 0


def test_peak_to_valley_invariant():
    series = [0.0, 3.0, -2.0, 4.0, -1.0]
    assert fc.peak_to_valley_ok([5.0, 6.0, 5.0], series)
    assert not fc.peak_to_valley_ok([5.0, 5.9], series)  # misses the 6.0 span (non-conservative)


def test_stations_cover_riser_boundaries_and_spacing_and_the_stack_connectors():
    s = synthetic_spec()
    st = fc.stations(s, spacing_m=25.0)
    riser = [a for line, a, _ in st if line == "Riser"]
    total = sum(x.length_m for x in s.riser)
    assert riser[0] == pytest.approx(0.0) and riser[-1] == pytest.approx(total)
    arc = 0.0
    for sec in s.riser[:-1]:
        arc += sec.length_m
        assert any(abs(a - arc) < 1e-9 for a in riser), arc  # every section boundary
    assert max(b - a for a, b in zip(riser, riser[1:])) <= 25.0 + 1e-9
    assert any(line == "Stack" for line, _, _ in st)
    assert fc.THETAS_DEG == (0.0, 45.0, 90.0, 135.0, 180.0, 225.0, 270.0, 315.0)


@pytest.mark.solver
@pytest.mark.skipif(not orcaflex_api.available(), reason="OrcFxAPI not available")
def test_fatigue_channels_of_a_short_irregular_window(tmp_path):
    from digitalmodel.drilling_riser import campaign as cp
    from digitalmodel.drilling_riser.global_model import orcaflex_run as orun

    import yaml

    p = tmp_path / "spec.yml"
    p.write_text(yaml.safe_dump({"model": synthetic_spec().model_dump(mode="json")}), encoding="utf-8")
    case = {"case_id": "FAT-1", "analysis": "dynamics", "params": {
        "base_spec": str(p), "heading_deg": 0.0, "fatigue": {"spacing_m": 50.0, "n_bins": 50},
        "irregular_wave": {"hs_m": 2.0, "tp_s": 8.0, "gamma": 2.0, "seed": 1},
        "dynamics": {"time_step_s": 0.1, "build_up_s": 16.0, "duration_s": 60.0}}}
    m = orun.load_model(cp.ADAPTER.build(case, tmp_path / "m"))
    cp.ADAPTER.statics(m, case)
    orun.run_dynamics(m)
    out = cp.ADAPTER.extract(m, case)
    fat = out["w5"]["fatigue"]
    assert fat["variable"] == "ZZ stress" and fat["thetas_deg"] == list(fc.THETAS_DEG)
    assert fat["invariant_failures"] == 0 and fat["out_of_range"] == 0
    pts = fat["stations"]
    assert len(pts) >= 5 and all(len(s["points"]) == 8 for s in pts)
    assert any(p["max_range"] > 0 for s in pts for p in s["points"])
