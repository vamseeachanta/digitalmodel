"""Invariant tests for the mooring-resilience screening model.

Pure-logic checks always run. The checks that need the pre-computed atlases
(read from parquet) are skipped where no parquet engine is importable under the
test runner, so they never turn a green CI domain red on an environment quirk.
"""

import math

import pytest

from digitalmodel.mooring_resilience import (
    Metocean,
    MooringConfig,
    ResilienceFactors,
    assess,
)
from digitalmodel.mooring_resilience.screening import (
    _fatigue_life_years,
    _traffic_light,
)

BASE = MooringConfig(n_lines=12, mbl_kN=15000, line_area_mm2=22600,
                     anchor_diameter_m=5.0, anchor_length_m=20.0,
                     soil_su_kpa=60.0, water_depth_m=1000.0)


def _atlas_available() -> bool:
    """True if the atlases can actually be read (parquet engine present)."""
    try:
        from pathlib import Path

        from digitalmodel.parametric.atlas import Atlas
        from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

        Atlas.load(Path(DEFAULT_ATLAS_ROOT), "fpso_mooring_full")
        return True
    except Exception:  # ImportError (no parquet engine) or missing atlas
        return False


needs_atlas = pytest.mark.skipif(
    not _atlas_available(), reason="atlas parquet engine unavailable under runner"
)


# --- pure-logic tests (no atlas) -------------------------------------------

def test_traffic_light_thresholds():
    assert _traffic_light(0.5, 5.0, escalate=False) == "GREEN"
    assert _traffic_light(0.9, 5.0, escalate=False) == "AMBER"
    assert _traffic_light(0.5, 1.5, escalate=False) == "AMBER"
    assert _traffic_light(1.2, 5.0, escalate=False) == "RED"
    assert _traffic_light(0.5, 0.5, escalate=False) == "RED"
    assert _traffic_light(0.5, 5.0, escalate=True) == "ESCALATE"


def test_fatigue_life_decreases_with_sea_state():
    f = ResilienceFactors()
    calm = _fatigue_life_years(Metocean(2.0, 11.0), BASE, f)
    rough = _fatigue_life_years(Metocean(5.0, 11.0), BASE, f)
    assert rough < calm
    # T-N with m=3: doubling stress range -> ~1/8 the life
    assert _fatigue_life_years(Metocean(4.0, 11.0), BASE, f) == pytest.approx(
        _fatigue_life_years(Metocean(2.0, 11.0), BASE, f) / 8.0, rel=1e-6)


def test_larger_chain_area_extends_fatigue_life():
    f = ResilienceFactors()
    thin = _fatigue_life_years(Metocean(4.0, 11.0),
                               MooringConfig(12, 15000, 15000, 5, 20, 60, 1000), f)
    thick = _fatigue_life_years(Metocean(4.0, 11.0),
                                MooringConfig(12, 15000, 30000, 5, 20, 60, 1000), f)
    assert thick > thin


# --- atlas-dependent tests --------------------------------------------------

@needs_atlas
def test_in_range_baseline_produces_finite_result():
    r = assess(BASE, Metocean(3.0, 11.0))
    assert math.isfinite(r.util_intact)
    assert math.isfinite(r.util_foundation)
    assert r.light in {"GREEN", "AMBER", "RED"}


@needs_atlas
def test_damaged_peak_exceeds_intact_peak():
    r = assess(BASE, Metocean(3.0, 11.0))
    assert r.t_damaged_kN > r.t_intact_kN


@needs_atlas
def test_out_of_range_escalates_not_extrapolates():
    r = assess(BASE, Metocean(9.0, 11.0))  # Hs outside atlas [2,5]
    assert r.light == "ESCALATE"
    assert math.isnan(r.util_intact)
    assert any("out of range" in n for n in r.notes)


@needs_atlas
def test_deeper_water_raises_tension_utilization():
    shallow = assess(MooringConfig(12, 15000, 22600, 5, 20, 60, 500),
                     Metocean(3.0, 11.0)).util_intact
    deep = assess(MooringConfig(12, 15000, 22600, 5, 20, 60, 1500),
                  Metocean(3.0, 11.0)).util_intact
    assert deep > shallow


@needs_atlas
def test_stronger_soil_reduces_foundation_utilization():
    soft = assess(MooringConfig(12, 15000, 22600, 5, 20, 25, 1000),
                  Metocean(3.0, 11.0)).util_foundation
    stiff = assess(MooringConfig(12, 15000, 22600, 5, 20, 100, 1000),
                   Metocean(3.0, 11.0)).util_foundation
    assert stiff < soft


@needs_atlas
def test_higher_mbl_reduces_tension_utilization():
    weak = assess(MooringConfig(12, 12000, 22600, 5, 20, 60, 1000),
                  Metocean(3.0, 11.0)).util_intact
    strong = assess(MooringConfig(12, 18000, 22600, 5, 20, 60, 1000),
                    Metocean(3.0, 11.0)).util_intact
    assert strong < weak


# --- citations -------------------------------------------------------------

def _write_e301_fixture(tmp_path):
    page = (tmp_path / "wikis" / "engineering" / "wiki" / "standards"
            / "dnv-os-e301.md")
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: DNV-OS-E301\npublisher: DNV\nrevision: 2021-07\n---\n# E301\n")
    return tmp_path


def test_safety_factor_citations_resolve_and_match_factors(tmp_path):
    from digitalmodel.mooring_resilience import safety_factor_citations
    cites = safety_factor_citations(repo_root=_write_e301_fixture(tmp_path))
    f = ResilienceFactors()
    assert cites["intact"].value == f.fos_intact == 1.67
    assert cites["damaged"].value == f.fos_damaged == 1.25
    assert cites["intact"].citation.code_id == "DNV-OS-E301"


def test_safety_factor_citations_degrade_gracefully(tmp_path):
    import digitalmodel.mooring_resilience.screening as mod
    from digitalmodel.mooring_resilience import safety_factor_citations
    mod._CITATION_WARNED = False
    empty = tmp_path / "empty_clone"
    (empty / "wikis").mkdir(parents=True)  # valid clone, no E301 page
    with pytest.warns(RuntimeWarning):
        assert safety_factor_citations(repo_root=empty) == {}
