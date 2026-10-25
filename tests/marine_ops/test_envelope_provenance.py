# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Screening provenance contracts; no standard qualification is inferred."""

from dataclasses import replace
import json

import pytest

from digitalmodel.marine_ops.operation_envelope import (
    OPERATIONS,
    MotionLimit,
    operation_envelope,
)
from tests.marine_ops.test_envelope_validation import full_rao


@pytest.mark.parametrize("operation", list(OPERATIONS))
def test_catalogue_has_explicit_qualification_gaps(operation):
    """Issue #2769 requires unverified status pending standard evidence.

    Qualification is separate from calculation success. The synthetic period
    is a calculation input only; it supplies no planned operation duration.
    """
    result = operation_envelope(full_rao(), operation, tp_range_s=[12.0])
    data = json.loads(json.dumps(result.to_dict(), allow_nan=False))
    assert data["points"][0]["evaluated"] is True
    assert data["alpha_source"]["status"] == "unverified_screening"
    assert data["alpha_source"]["dependencies"] == {
        "planned_operation_period_h": None,
        "design_hs_m": None,
        "forecast_level": None,
    }
    qualification = data["citation_sidecar"]["qualification"]
    assert qualification == {
        "status": "NOT_EVALUATED",
        "reason": "Standard evidence and alpha selection inputs are unavailable.",
        "standard_designation": None,
        "standard_edition": None,
        "motion_limit_clause": None,
        "alpha_clause": None,
        "alpha_table": None,
        "evidence_reference": None,
    }


def test_catalogue_sidecar_records_exact_screening_inputs():
    """Literal inputs below are from envelope-basis.md, revision 2, table B6.

    B6 labels these values unverified; this tests transcription, not compliance.
    """
    result = operation_envelope(full_rao(), "transit", tp_range_s=[12.0])
    sidecar = result.to_dict()["citation_sidecar"]
    assert result.alpha_source["value"] == 0.90
    assert sidecar["catalogue_table"] == "B6"
    assert sidecar["motion_limits"] == [
        {"dof": "heave", "max_significant": 3.0, "unit": "m"},
        {"dof": "roll", "max_significant": 6.0, "unit": "deg"},
        {"dof": "pitch", "max_significant": 2.5, "unit": "deg"},
    ]


def test_custom_inputs_are_recorded_without_catalogue_attribution():
    """Synthetic alpha=0.75, C=2 m and unit gain give Hs=3 m by hand.

    From envelope-basis.md B1/B2: m0_wave=Hs²/16, s=2 sqrt(m0)=Hs/2,
    so alpha*C/(1/2) = 0.75*2/0.5 = 3. No standard value is supplied.
    A reused catalogue key and a caller's basis cannot establish qualification.
    """
    operation = replace(
        OPERATIONS["transit"],
        limits=(MotionLimit("heave", 2.0, "m"),),
        alpha=0.75,
        basis="caller claims qualification",
    )
    result = operation_envelope(full_rao(), operation, tp_range_s=[12.0])
    data = result.to_dict()
    assert result.hs_limit_at(12.0) == pytest.approx(3.0)
    assert data["alpha_source"]["value"] == 0.75
    assert data["alpha_source"]["basis"] == "caller claims qualification"
    assert data["alpha_source"]["status"] == "unverified_screening"
    assert data["citation_sidecar"]["catalogue_table"] is None
    assert data["citation_sidecar"]["motion_limits"] == [
        {"dof": "heave", "max_significant": 2.0, "unit": "m"},
    ]
    assert data["citation_sidecar"]["qualification"]["status"] == "NOT_EVALUATED"


def test_provenance_records_are_independent_between_results():
    """Mutating one returned gap record must not qualify another result."""
    first = operation_envelope(full_rao(), "transit", tp_range_s=[12.0])
    second = operation_envelope(full_rao(), "transit", tp_range_s=[12.0])
    first.citation_sidecar["qualification"]["status"] = "caller modified"
    first.citation_sidecar["motion_limits"].clear()
    first.alpha_source["dependencies"].clear()
    assert second.citation_sidecar["qualification"]["status"] == "NOT_EVALUATED"
    assert second.citation_sidecar["motion_limits"]
    assert second.alpha_source["dependencies"]["planned_operation_period_h"] is None
