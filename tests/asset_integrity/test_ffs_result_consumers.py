# ABOUTME: Characterisation of the existing FFSAssessmentResult consumer surface (GML, LML,
# ABOUTME: PITTING, REPAIR) written before the #2157 P3 Protocol, so it cannot regress.
"""Consumer characterisation for ``FFSAssessmentResult`` (#2157 P3, owner card G11).

Comparator class: regression anchor. The values below are the current output of the
Phase-1 chain on generic synthetic grids, captured before ``crack_fe_assessment`` added
the shared ``FFSResultProtocol``. They pin the serialised key set, the verdict and
``passes`` so the sibling-result change leaves the metal-loss record unchanged. They are
not claims of agreement with an external authority.
"""

from __future__ import annotations

import dataclasses

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment import (
    FFSAssessmentResult,
    FFSComponent,
    assess_component,
)

BASE_KEYS = {
    "component_id", "assessment_type", "level_reached", "t_nominal_in", "t_min_in",
    "t_measured_min_in", "t_measured_avg_in", "fca_in", "rsf", "rsf_a",
    "folias_factor", "remaining_life_yr", "verdict", "rerated_pressure_psi",
    "sufficiency_status", "passes", "code_reference",
}

REQUIRED_FIELDS = [
    "component_id", "assessment_type", "level_reached", "t_nominal_in", "t_min_in",
    "t_measured_min_in", "t_measured_avg_in", "fca_in", "rsf", "rsf_a",
    "folias_factor", "remaining_life_yr", "verdict", "rerated_pressure_psi",
    "sufficiency_status",
]


def _pipe() -> FFSComponent:
    return FFSComponent(
        component_id="LINE-001",
        design_code="B31.8",
        nominal_od_in=12.75,
        nominal_wt_in=0.500,
        design_pressure_psi=1000.0,
        smys_psi=52_000.0,
        corrosion_rate_in_per_yr=0.005,
        rsf_a=0.90,
    )


def _uniform():
    return pd.DataFrame([[0.48] * 6 for _ in range(6)])


def _local():
    g = np.full((6, 6), 0.50)
    g[2, 3] = 0.30
    g[3, 3] = 0.32
    return g


def _pits():
    g = np.full((30, 40), 0.50)
    for (r, c), v in {
        (3, 5): 0.30, (3, 20): 0.30, (8, 10): 0.25, (12, 30): 0.30,
        (15, 4): 0.30, (20, 22): 0.30, (25, 12): 0.30, (27, 35): 0.30,
    }.items():
        g[r, c] = v
    return pd.DataFrame(g)


def _repair():
    g = np.full((6, 6), 0.50)
    g[2, 3] = 0.12
    return g


CASES = {
    "GML": (lambda: assess_component(_pipe(), _uniform(), force_type="GML"),
            {"assessment_type": "GML", "verdict": "ACCEPT", "passes": True,
             "level_reached": 1, "folias_factor": 1.0, "t_measured_min_in": 0.48,
             "remaining_life_yr": 61.94551282051282, "sufficiency_status": "SUFFICIENT"}),
    "LML": (lambda: assess_component(_pipe(), _local()),
            {"assessment_type": "LML", "verdict": "ACCEPT", "passes": True,
             "level_reached": 1, "folias_factor": 1.5812689937754407,
             "t_measured_min_in": 0.3, "remaining_life_yr": 25.945512820512818,
             "sufficiency_status": "TAKE_MORE"}),
    "PITTING": (lambda: assess_component(_pipe(), _pits(), force_type="PITTING"),
                {"assessment_type": "PITTING", "verdict": "ACCEPT", "passes": True,
                 "level_reached": 1, "folias_factor": 13.892443989449804,
                 "t_measured_min_in": 0.25, "remaining_life_yr": 15.94551282051282,
                 "sufficiency_status": "SUFFICIENT"}),
    "REPAIR": (lambda: assess_component(_pipe(), _repair()),
               {"assessment_type": "LML", "verdict": "REPAIR", "passes": False,
                "level_reached": 2, "rsf": 0.9419036737688335,
                "remaining_life_yr": 0.0, "sufficiency_status": "TAKE_MORE"}),
}


@pytest.mark.parametrize("name", list(CASES))
def test_existing_consumers_unchanged(name):
    build, expected = CASES[name]
    res = build()
    assert isinstance(res, FFSAssessmentResult)
    payload = res.to_dict()
    extra = {"repair_recommendation"} if name == "REPAIR" else set()
    assert set(payload) == BASE_KEYS | extra
    for key, value in expected.items():
        if isinstance(value, float):
            assert payload[key] == pytest.approx(value, rel=1e-12), key
        else:
            assert payload[key] == value, key
    assert payload["code_reference"] == "API 579-1/ASME FFS-1 (2021)"
    assert payload["passes"] is (payload["verdict"] in ("ACCEPT", "MONITOR"))
    assert res.passes is payload["passes"]


def test_metal_loss_fields_stay_required():
    """The metal-loss fields remain required (no defaults were added for CRACK)."""
    fields = {f.name: f for f in dataclasses.fields(FFSAssessmentResult)}
    for name in REQUIRED_FIELDS:
        f = fields[name]
        assert f.default is dataclasses.MISSING, name
        assert f.default_factory is dataclasses.MISSING, name
    assert fields["code_reference"].default == "API 579-1/ASME FFS-1 (2021)"
