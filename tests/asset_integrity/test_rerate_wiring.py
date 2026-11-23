"""RE_RATE verdict wiring (#1273) — MAWP_r flows decision → coordinator → report.

MAWP_r = MAWP x min(1, RSF/RSFa) per API 579-1 §2.4.2.2.
"""

import numpy as np
import pytest

from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
    FFSComponent,
    assess_component,
)
from digitalmodel.asset_integrity.assessment.ffs_decision import FFSDecision
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport


def _decide(rsf, *, design_pressure=None, l1="FAIL_LEVEL_1", l2="FAIL_LEVEL_2"):
    return FFSDecision.decide(
        level1_verdict=l1,
        level2_verdict=l2,
        rsf=rsf,
        rsf_a=0.90,
        t_mm_in=0.60,
        t_min_in=0.40,
        corrosion_rate_in_per_yr=0.01,
        design_pressure_psi=design_pressure,
    )


def test_rerate_band_reports_rerated_mawp():
    d = _decide(0.72, design_pressure=1000.0)
    assert d["verdict"] == "RE_RATE"
    assert d["rerated_mawp_psi"] == pytest.approx(1000.0 * 0.72 / 0.90)
    assert "MAWP_r=800 psi" in d["governing_criterion"]
    assert "2.4.2.2" in d["governing_criterion"]


def test_accept_caps_rerated_at_design_pressure():
    d = _decide(0.97, design_pressure=1000.0, l1="ACCEPT", l2="ACCEPT")
    assert d["verdict"] == "ACCEPT"
    assert d["rerated_mawp_psi"] == pytest.approx(1000.0)  # min(1, RSF/RSFa) cap


def test_without_design_pressure_falls_back():
    d = _decide(0.72)
    assert d["verdict"] == "RE_RATE"
    assert d["rerated_mawp_psi"] is None
    assert "Reduce MAWP or operating pressure" in d["governing_criterion"]


def test_replace_floor_still_carries_rerated_value():
    d = _decide(0.40, design_pressure=1000.0)
    assert d["verdict"] == "REPLACE"
    assert d["rerated_mawp_psi"] == pytest.approx(1000.0 * 0.40 / 0.90)


def test_coordinator_single_sources_rerated_pressure():
    """A deep uniform-loss grid lands in the re-rating band; the result's
    rerated_pressure_psi is exactly the decision layer's MAWP_r."""
    component = FFSComponent(
        component_id="RERATE-1",
        design_code="B31.8",
        nominal_od_in=21.25,
        nominal_wt_in=0.875,
        design_pressure_psi=2000.0,
        smys_psi=80_000.0,
        corrosion_rate_in_per_yr=0.005,
    )
    # generous uniform wall with one wide 35%-loss patch
    grid = np.full((60, 24), 0.875)
    grid[20:40, 6:18] = 0.875 * 0.65
    result = assess_component(component, grid)
    d = result.decision
    assert d["rerated_mawp_psi"] is not None
    assert result.rerated_pressure_psi == pytest.approx(d["rerated_mawp_psi"])
    assert result.rerated_pressure_psi <= component.design_pressure_psi
    if result.verdict == "RE_RATE":
        assert "MAWP_r" in d["governing_criterion"]


def test_report_shows_mawp_r_row_on_rerate():
    import pandas as pd

    grid_df = pd.DataFrame(np.full((10, 8), 0.60))
    decision = _decide(0.72, design_pressure=1000.0)
    html_doc = FFSReport.generate_html(
        grid_df=grid_df,
        decision=decision,
        component_id="RERATE-1",
        nominal_od_in=21.25,
        nominal_wt_in=0.875,
        t_min_in=0.40,
        design_code="B31.8",
        design_pressure_psi=1000.0,
        smys_psi=80_000.0,
        corrosion_rate_in_per_yr=0.01,
    )
    assert "Re-rated MAWP" in html_doc
    assert "800 psi" in html_doc
