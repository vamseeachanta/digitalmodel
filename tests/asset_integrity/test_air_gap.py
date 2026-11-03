# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic subtraction checks; see the air-gap method note.

Basis: D=10, W=2, C=3, required=1 metres. For supplied subsidence
0, 2, 4, 5 metres, D-S-W-C gives 5, 3, 1, 0 metres; subtracting the
minimum gives 4, 2, 0, -1 metres. Labels 2000 through 2003 are samples.
Other numbers below are synthetic boundary and coordinate probes, not criteria.
"""

from dataclasses import replace

import pytest

from digitalmodel.asset_integrity.air_gap import (
    AirGapBasis,
    Elevation,
    SubsidenceSample,
    calculate_air_gap,
    evaluate_sampled_air_gap,
)


@pytest.fixture
def basis():
    """Return the independently specified synthetic basis, in metres."""
    return AirGapBasis(
        deck_underside=Elevation(10.0, "shared datum"),
        still_water=Elevation(2.0, "shared datum"),
        crest_height_m=3.0,
        required_clearance_m=1.0,
    )


def test_air_gap_and_sampled_breach_match_hand_calculation(basis):
    samples = (
        SubsidenceSample(2000, 0.0),
        SubsidenceSample(2001, 2.0),
        SubsidenceSample(2002, 4.0),
        SubsidenceSample(2003, 5.0),
    )
    result = evaluate_sampled_air_gap(basis, samples)
    assert tuple(row.date_label for row in result.samples) == (2000, 2001, 2002, 2003)
    assert tuple(row.air_gap.clearance_m for row in result.samples) == (5, 3, 1, 0)
    assert tuple(row.air_gap.margin_m for row in result.samples) == (4, 2, 0, -1)
    assert tuple(row.air_gap.meets_required_clearance for row in result.samples) == (
        True, True, True, False,
    )
    assert all(row.air_gap.datum == "shared datum" for row in result.samples)
    assert result.first_sampled_breach == 2003


def test_no_sampled_breach_including_equality(basis):
    result = evaluate_sampled_air_gap(
        basis, (SubsidenceSample(2000, 0), SubsidenceSample(2002, 4))
    )
    assert result.first_sampled_breach is None
    assert result.samples[-1].air_gap.margin_m == 0
    assert result.samples[-1].air_gap.meets_required_clearance


def test_first_supplied_sample_can_already_breach(basis):
    result = evaluate_sampled_air_gap(
        basis, (SubsidenceSample(2000, 5), SubsidenceSample(2003, 0))
    )
    assert result.first_sampled_breach == 2000
    assert result.samples[-1].air_gap.meets_required_clearance


@pytest.mark.parametrize("offset", [-100.0, 0.0, 100.0])
def test_shared_datum_translation_is_invariant(basis, offset):
    translated = replace(
        basis,
        deck_underside=Elevation(10 + offset, "translated datum"),
        still_water=Elevation(2 + offset, "translated datum"),
    )
    result = calculate_air_gap(translated, 2)
    assert result.clearance_m == 3  # (10+offset)-2-(2+offset)-3
    assert result.margin_m == 2  # 3-1
    assert result.meets_required_clearance
    assert result.datum == "translated datum"


@pytest.mark.parametrize("field", ["deck_underside", "still_water"])
@pytest.mark.parametrize("label", [None, "", " ", 1, "other datum", "shared datum "])
def test_invalid_or_ambiguous_inputs_are_rejected(basis, field, label):
    elevation = replace(getattr(basis, field), datum=label)
    with pytest.raises(ValueError, match="datum"):
        calculate_air_gap(replace(basis, **{field: elevation}), 0)


@pytest.mark.parametrize("field", [
    "deck_underside", "still_water", "crest_height_m", "required_clearance_m",
    "subsidence_m",
])
@pytest.mark.parametrize("value", [float("nan"), float("inf"), -float("inf"), None, True, "2"])
def test_nonfinite_and_nonnumeric_inputs_are_rejected(basis, field, value):
    subsidence = 0
    if field == "subsidence_m":
        subsidence = value
    elif field in ("deck_underside", "still_water"):
        basis = replace(basis, **{field: replace(getattr(basis, field), value_m=value)})
    else:
        basis = replace(basis, **{field: value})
    with pytest.raises(ValueError):
        calculate_air_gap(basis, subsidence)


@pytest.mark.parametrize("field", ["subsidence_m", "crest_height_m", "required_clearance_m"])
def test_negative_magnitudes_are_rejected(basis, field):
    with pytest.raises(ValueError):
        if field == "subsidence_m":
            calculate_air_gap(basis, -1)
        else:
            calculate_air_gap(replace(basis, **{field: -1}), 0)


@pytest.mark.parametrize("samples", [
    (),
    (SubsidenceSample(2000, 0), SubsidenceSample(2000, 2)),
    (SubsidenceSample(2001, 0), SubsidenceSample(2000, 2)),
    (SubsidenceSample(None, 0),),
    (SubsidenceSample("2000", 0),),
    (SubsidenceSample(True, 0),),
    (SubsidenceSample(2000, float("nan")),),
    (SubsidenceSample(2000, -1),),
])
def test_invalid_duplicate_or_unordered_samples_are_rejected(basis, samples):
    with pytest.raises(ValueError):
        evaluate_sampled_air_gap(basis, samples)


def test_zero_magnitudes_and_negative_elevations_are_valid(basis):
    result = calculate_air_gap(replace(
        basis, deck_underside=Elevation(-2, "shared datum"),
        still_water=Elevation(-3, "shared datum"),
        crest_height_m=0, required_clearance_m=0,
    ), 0)
    assert result.clearance_m == 1  # -2-0-(-3)-0
    assert result.margin_m == 1  # 1-0
    assert result.meets_required_clearance


def test_nonfinite_derived_arithmetic_is_rejected(basis):
    with pytest.raises(ValueError, match="finite"):
        calculate_air_gap(replace(
            basis, deck_underside=Elevation(-1e308, "shared datum"),
            still_water=Elevation(1e308, "shared datum"),
        ), 1e308)
