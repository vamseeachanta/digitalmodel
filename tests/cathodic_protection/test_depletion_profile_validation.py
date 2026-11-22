"""Input-domain and numerical compatibility checks for depletion profiles."""

import numpy as np
import pytest

from digitalmodel.cathodic_protection import anode_depletion as depletion

VALID_INPUTS = {
    "original_mass_kg": 1000.0,
    "mean_current_A": 1.0,
    "design_life_years": 10.0,
    "anode_capacity_Ah_kg": 2000.0,
    "utilization_factor": 0.9,
    "time_step_years": 1.0,
}


@pytest.fixture
def reject_kernel_call(monkeypatch):
    """Fail before the baseline can reach an invalid sampling loop."""
    def unexpected_call(*args, **kwargs):
        pytest.fail("invalid profile input reached the consumption kernel")

    monkeypatch.setattr(depletion.kernel, "mass_consumed", unexpected_call)


@pytest.mark.parametrize("argument", VALID_INPUTS)
@pytest.mark.parametrize(
    "value",
    [float("nan"), float("inf"), -float("inf"), True, False, None, "1", 1j, [], {}],
    ids=["nan", "inf", "negative-inf", "true", "false", "none", "text", "complex",
         "list", "dict"],
)
def test_rejects_nonfinite_or_nonreal_input(argument, value, reject_kernel_call):
    inputs = {**VALID_INPUTS, argument: value}
    with pytest.raises(ValueError, match=argument):
        depletion.generate_depletion_profile(**inputs)


@pytest.mark.parametrize(
    "argument",
    ["original_mass_kg", "anode_capacity_Ah_kg", "utilization_factor", "time_step_years"],
)
@pytest.mark.parametrize("value", [0.0, -1.0])
def test_rejects_nonpositive_input(argument, value, reject_kernel_call):
    with pytest.raises(ValueError, match=argument):
        depletion.generate_depletion_profile(**{**VALID_INPUTS, argument: value})


@pytest.mark.parametrize("argument", ["mean_current_A", "design_life_years"])
def test_rejects_negative_nonnegative_input(argument, reject_kernel_call):
    with pytest.raises(ValueError, match=argument):
        depletion.generate_depletion_profile(**{**VALID_INPUTS, argument: -1.0})


@pytest.mark.parametrize("value", [1.01, 2.0])
def test_rejects_utilization_above_one(value, reject_kernel_call):
    with pytest.raises(ValueError, match="utilization_factor"):
        depletion.generate_depletion_profile(**{**VALID_INPUTS, "utilization_factor": value})


def test_ten_year_profile_matches_independent_charge_budget():
    profile = depletion.generate_depletion_profile(**VALID_INPUTS)
    # Synthetic charge accounting: 1 A * 8760 h/year / 2000 Ah/kg.
    consumed_per_year = 4.38
    assert profile.years == list(range(11))
    assert profile.remaining_mass_kg == pytest.approx(
        [1000.0 - consumed_per_year * year for year in range(11)], abs=0.01
    )
    assert profile.usable_mass_kg == pytest.approx(
        [900.0 - consumed_per_year * year for year in range(11)], abs=0.01
    )
    assert profile.depletion_percentage == [
        round(consumed_per_year * year / 1000.0 * 100.0, 1) for year in range(11)
    ]
    assert profile.end_of_life_year == round(900.0 / consumed_per_year, 2)


def test_zero_current_preserves_mass_and_projection_end_of_life():
    profile = depletion.generate_depletion_profile(**{**VALID_INPUTS, "mean_current_A": 0.0})
    assert profile.years == list(range(11))
    assert profile.remaining_mass_kg == [1000.0] * 11
    assert profile.usable_mass_kg == [900.0] * 11
    assert profile.depletion_percentage == [0.0] * 11
    assert profile.end_of_life_year == 10.0


def test_zero_duration_retains_initial_sample():
    profile = depletion.generate_depletion_profile(
        **{**VALID_INPUTS, "design_life_years": 0.0}
    )
    assert profile.years == [0.0]
    assert profile.remaining_mass_kg == [1000.0]
    assert profile.usable_mass_kg == [900.0]
    assert profile.depletion_percentage == [0.0]
    assert profile.end_of_life_year == round(900.0 / 4.38, 2)


@pytest.mark.parametrize("scalar_type", [float, int, np.float32, np.float64, np.int64])
def test_accepts_real_scalar_types(scalar_type):
    inputs = {key: scalar_type(value) for key, value in VALID_INPUTS.items()}
    inputs["utilization_factor"] = scalar_type(1)
    profile = depletion.generate_depletion_profile(**inputs)
    assert profile.years == list(range(11))
    assert profile.remaining_mass_kg[-1] == pytest.approx(956.2, abs=0.01)
    assert profile.usable_mass_kg == profile.remaining_mass_kg


@pytest.mark.parametrize("argument", VALID_INPUTS)
@pytest.mark.parametrize("value", [np.float32("nan"), np.float64("inf"), np.bool_(True)])
def test_rejects_invalid_numpy_scalars(argument, value, reject_kernel_call):
    with pytest.raises(ValueError, match=argument):
        depletion.generate_depletion_profile(**{**VALID_INPUTS, argument: value})
