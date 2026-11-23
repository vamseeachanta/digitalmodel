# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Decision tests for the documented production-method selection policy."""

import pytest

from digitalmodel.production_engineering.method_selector import (
    select_ipr_model,
    select_vlp_correlation,
)


@pytest.mark.parametrize(
    "state, expected",
    [
        ({"reservoir_pressure_psi": 3000, "bubble_point_psi": 2000,
          "minimum_pwf_psi": 2200}, "linear"),
        ({"reservoir_pressure_psi": 3000, "bubble_point_psi": 2000,
          "minimum_pwf_psi": 2000}, "linear"),
        ({"reservoir_pressure_psi": 3000, "bubble_point_psi": 2000,
          "minimum_pwf_psi": 1000}, "composite"),
        ({"reservoir_pressure_psi": 2000, "bubble_point_psi": 2000,
          "minimum_pwf_psi": 1000}, "vogel"),
        ({"reservoir_pressure_psi": 1500, "bubble_point_psi": 2000,
          "minimum_pwf_psi": 1000}, "vogel"),
        ({"reservoir_pressure_psi": 3000, "bubble_point_psi": 2000,
          "minimum_pwf_psi": 2200, "multirate_test_available": True},
         "fetkovich"),
    ],
)
def test_ipr_policy_matches_documented_decisions(state, expected):
    assert select_ipr_model(state, {"phase": "oil", "solution_gas_drive": True}) == expected


@pytest.mark.parametrize("phase", ["gas", "gas_condensate", "three_phase"])
def test_ipr_rejects_unsupported_fluid(phase):
    state = {"reservoir_pressure_psi": 3000, "bubble_point_psi": 2000,
             "minimum_pwf_psi": 1000}
    with pytest.raises(NotImplementedError):
        select_ipr_model(state, {"phase": phase, "solution_gas_drive": True})


def test_vogel_requires_solution_gas_drive_evidence():
    state = {"reservoir_pressure_psi": 1500, "bubble_point_psi": 2000,
             "minimum_pwf_psi": 1000}
    with pytest.raises(NotImplementedError):
        select_ipr_model(state, {"phase": "oil"})


@pytest.mark.parametrize("key, value", [
    ("reservoir_pressure_psi", 0), ("bubble_point_psi", -1),
    ("minimum_pwf_psi", -1), ("minimum_pwf_psi", 4000),
    ("reservoir_pressure_psi", float("nan")),
    ("bubble_point_psi", float("inf")), ("minimum_pwf_psi", True),
    ("multirate_test_available", "yes"),
])
def test_invalid_reservoir_state_is_rejected(key, value):
    state = {"reservoir_pressure_psi": 3000, "bubble_point_psi": 2000,
             "minimum_pwf_psi": 1000, key: value}
    with pytest.raises(ValueError):
        select_ipr_model(state, {"phase": "oil", "solution_gas_drive": True})


def test_missing_pressure_is_rejected():
    with pytest.raises(ValueError, match="reservoir_pressure_psi"):
        select_ipr_model({}, {"phase": "oil"})


@pytest.mark.parametrize("regime", ["vertical_oil", "inclined_oil", "gas_condensate"])
def test_published_vlp_does_not_fall_back_to_aliases(regime):
    with pytest.raises(NotImplementedError, match="not implemented"):
        select_vlp_correlation(regime, {"phase": "oil"})


def test_simplified_vlp_requires_explicit_screening_request():
    assert select_vlp_correlation("screening", {"phase": "oil"}) == "simplified_homogeneous"


def test_gas_screening_is_not_qualified():
    with pytest.raises(NotImplementedError):
        select_vlp_correlation("screening", {"phase": "gas"})


def test_unknown_vlp_regime_is_rejected():
    with pytest.raises(ValueError):
        select_vlp_correlation("unknown", {"phase": "oil"})
