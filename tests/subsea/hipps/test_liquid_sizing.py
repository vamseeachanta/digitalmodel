# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Liquid sizing qualification against published and independent arithmetic.

Source: Emerson/Fisher, Control Valve Handbook, sixth edition (August 2023),
sections 5.8.2–5.8.4, pp. 113–115. Fixtures below are constructed arithmetic
inputs unless explicitly identified as the published propane example.
"""

import pytest

from digitalmodel.subsea.hipps.valve_sizing import (
    liquid_capacity,
    required_liquid_coefficient,
)


def liquid_inputs(**changes):
    """Construct a unit-normalized case, not fluid or equipment design data.

    By hand, dp_limit=(0.5/1)^2*(100-1*0)=25 and dp=100-0=100.
    Thus q=1*1*2*sqrt(25/1)=10; all fixture values define this example.
    """
    inputs = dict(
        p_upstream=100.0,
        p_downstream=0.0,
        specific_gravity=1.0,
        unit_constant=1.0,
        piping_factor=1.0,
        recovery_factor=0.5,
        critical_pressure_ratio=1.0,
        vapor_pressure=0.0,
    )
    return inputs | changes


def test_liquid_capacity_rejects_overflow():
    """Constructed stress case: q=1e308*1*2*sqrt(25)=1e309.

    That positive real result exceeds binary64 range; infinity is not capacity.
    """
    with pytest.raises(ValueError, match="capacity_rate"):
        liquid_capacity(coefficient=2.0, **liquid_inputs(unit_constant=1e308))


def test_liquid_capacity_rejects_underflow():
    """Constructed case: q=1e-200*1*1e-200*sqrt(25)=5e-400.

    Positive coefficient and drop cannot imply true zero flow; binary64
    underflow will be rejected rather than reported as a closed valve.
    """
    with pytest.raises(ValueError, match="underflow"):
        liquid_capacity(coefficient=1e-200, **liquid_inputs(unit_constant=1e-200))


@pytest.mark.parametrize("rate, unit_constant", [(1e308, 0.01), (1e-300, 1e300)])
def test_liquid_inverse_rejects_unrepresentable_coefficient(rate, unit_constant):
    """Independent binary64 stress cases with unit capacity=5*N1.

    C=1e308/0.05=2e309 overflows; C=1e-300/5e300=2e-601 underflows.
    Neither infinite nor zero C represents the required positive coefficient.
    The coefficients and exponents define constructed numerical probes.
    """
    with pytest.raises(ValueError, match="required_coefficient"):
        required_liquid_coefficient(
            rate=rate, **liquid_inputs(unit_constant=unit_constant)
        )


def test_liquid_choking_overflow_is_a_value_error():
    """Constructed FLP/Fp=1e200: the final differential 1e402 overflows.

    Domain failures shall use ValueError rather than leaking OverflowError.
    This is a representability check, not a permitted valve-factor range.
    """
    with pytest.raises(ValueError, match="choked_pressure_drop"):
        liquid_capacity(coefficient=2.0, **liquid_inputs(recovery_factor=1e200))


def test_published_propane_required_coefficient():
    """Handbook, sixth ed. (2023), §5.8.4, pp. 114–115: Cv=125.7.

    Published rounded inputs: dp=314.7-289.7=25 psi;
    dp_limit=(0.81/0.90)^2*(314.7-0.83*124.3)=171.34011 psi.
    Thus Cv=800/(1*0.90*sqrt(25/0.50))=125.7078722109418.
    The 0.05 Cv absolute tolerance is half the example's final 0.1 increment;
    relative tolerance is disabled. No expected value uses production code.
    """
    inputs = dict(
        p_upstream=314.7, p_downstream=289.7, specific_gravity=0.50,
        unit_constant=1.0, piping_factor=0.90, recovery_factor=0.81,
        critical_pressure_ratio=0.83, vapor_pressure=124.3,
    )  # All inputs: Handbook (2023), §5.8.4, pp. 114–115.
    result = required_liquid_coefficient(rate=800.0, **inputs)
    assert result == pytest.approx(125.7, abs=0.05, rel=0)


@pytest.mark.parametrize(
    "downstream, drop, rate, choked",
    [(84.0, 16.0, 8.0, False), (75.0, 25.0, 10.0, True),
     (0.0, 25.0, 10.0, True)],
)
def test_independent_choking_and_inverse(downstream, drop, rate, choked):
    """Handbook (2023), §§5.8.2–5.8.3, evaluated by hand for synthetic data.

    The fixture gives dp_limit=25. With C=2, N1=Fp=SG=1:
    p2=84 gives dp=16 and q=2*sqrt(16)=8;
    p2=75 gives dp=25 and q=2*sqrt(25)=10 at choking onset;
    p2=0 gives dp=100, limited to 25, so q=10 again.
    Each inverse is q/sqrt(dp_sizing)=2, exactly in binary64.
    """
    inputs = liquid_inputs(p_downstream=downstream)
    result = liquid_capacity(coefficient=2.0, **inputs)
    assert result.effective_drop == drop
    assert result.rate == rate
    assert result.choked is choked
    assert required_liquid_coefficient(rate=rate, **inputs) == 2.0


def test_true_zero_capacity_and_demand():
    """By §5.8.3 (2023), q=C*sqrt(dp) for the normalized synthetic case.

    C=0 or dp=0 gives q=0 exactly. For q=0, required C=0 by convention;
    q=1 at dp=0 has no finite C. These zeros are not underflow.
    """
    assert liquid_capacity(coefficient=0.0, **liquid_inputs()).rate == 0.0
    inputs = liquid_inputs(p_downstream=100.0)
    assert liquid_capacity(coefficient=2.0, **inputs).rate == 0.0
    assert required_liquid_coefficient(rate=0.0, **inputs) == 0.0
    with pytest.raises(ValueError, match="positive pressure drop"):
        required_liquid_coefficient(rate=1.0, **inputs)


@pytest.mark.parametrize(
    "coefficient, changes, effective, choked",
    [(0.0, dict(specific_gravity=1e-310), 25.0, True),
     (1e300, dict(p_downstream=100.0, unit_constant=1e10), 0.0, False)],
)
def test_true_zero_capacity_precedes_overflowing_products(
    coefficient, changes, effective, choked,
):
    """Constructed zeros in q=N1*Fp*C*sqrt(dp_sizing/SG), §5.8.3.

    C=0 makes q=0 even though 25/1e-310 overflows. With dp=0, q=0
    even though N1*C=1e310 overflows. Neither zero is underflow.
    The normalized fixture gives dp_max=25 and the stated choking flags.
    """
    inputs = liquid_inputs(**changes)
    result = liquid_capacity(coefficient=coefficient, **inputs)
    assert result.rate == 0.0
    assert result.effective_drop == effective
    assert result.choked is choked
    assert required_liquid_coefficient(rate=0.0, **inputs) == 0.0


@pytest.mark.parametrize(
    "changes",
    [dict(recovery_factor=1e-200, p_upstream=1e300),
     dict(recovery_factor=2.0**700, p_upstream=2.0**-1000)],
)
def test_zero_demand_preserves_representable_scaled_choking_drop(changes):
    """Shared validation must retain representable limits at zero demand.

    Hand calculation: dp_max=1e-400*1e300=1e-100 for the first case;
    dp_max=2^1400*2^-1000=2^400 for the second. Both are finite and
    positive despite the intermediate square's range, so required C=0.
    """
    inputs = liquid_inputs(**changes)
    assert required_liquid_coefficient(rate=0.0, **inputs) == 0.0
    assert liquid_capacity(coefficient=0.0, **inputs).rate == 0.0


@pytest.mark.parametrize("downstream", [0.0, 100.0])
@pytest.mark.parametrize(
    "changes",
    [dict(unit_constant=1e308),
     dict(unit_constant=1e-200, specific_gravity=1e300)],
    ids=["unit-capacity-overflow", "unit-capacity-underflow"],
)
def test_zero_demand_does_not_require_representable_unit_capacity(changes, downstream):
    """Zero demand requires C=0, even when unit capacity is out of range.

    At positive drop the constructed cases have unit capacities 5e308 and
    5e-350; at zero drop both are exactly zero. None changes zero demand.
    """
    inputs = liquid_inputs(p_downstream=downstream, **changes)
    assert liquid_capacity(coefficient=0.0, **inputs).rate == 0.0
    assert required_liquid_coefficient(rate=0.0, **inputs) == 0.0


@pytest.mark.parametrize(
    "changes, error",
    [(dict(unit_constant=1e308), "capacity_rate"),
     (dict(unit_constant=1e-200, specific_gravity=1e300), "underflow")],
)
def test_positive_demand_still_rejects_unrepresentable_unit_capacity(changes, error):
    """Positive demand still needs a finite, positive unit capacity."""
    with pytest.raises(ValueError, match=error):
        required_liquid_coefficient(rate=1.0, **liquid_inputs(**changes))


@pytest.mark.parametrize("value", [-1.0, float("nan"), float("inf"), -float("inf")])
@pytest.mark.parametrize("field", list(liquid_inputs()))
def test_zero_demand_rejects_invalid_liquid_inputs(field, value):
    """Zero demand does not accept negative or non-finite physical inputs."""
    with pytest.raises(ValueError):
        required_liquid_coefficient(rate=0.0, **liquid_inputs(**{field: value}))


@pytest.mark.parametrize(
    "changes",
    [dict(p_upstream=0.0), dict(specific_gravity=0.0),
     dict(unit_constant=0.0), dict(piping_factor=0.0),
     dict(recovery_factor=0.0), dict(critical_pressure_ratio=0.0),
     dict(p_downstream=101.0), dict(vapor_pressure=100.0),
     dict(vapor_pressure=50.0, critical_pressure_ratio=2.0),
     dict(vapor_pressure=50.0, critical_pressure_ratio=3.0),
     dict(recovery_factor=1e200), dict(recovery_factor=1e-200)],
)
def test_zero_demand_preserves_liquid_domain_and_choking_validation(changes):
    """Retain factor, forward-flow, inlet and choking-range rejection."""
    with pytest.raises(ValueError):
        required_liquid_coefficient(rate=0.0, **liquid_inputs(**changes))


@pytest.mark.parametrize("value", [-1.0, float("nan"), float("inf"), -float("inf")])
@pytest.mark.parametrize(
    "field",
    ["coefficient", "p_upstream", "p_downstream", "specific_gravity",
     "unit_constant", "piping_factor", "recovery_factor",
     "critical_pressure_ratio", "vapor_pressure"],
)
def test_invalid_liquid_inputs(field, value):
    """Construct invalid domains for §5.8.2–5.8.3 positive-factor arithmetic.

    Negative physical inputs and non-finite numbers cannot form a qualified
    capacity. These probes define invalid data, not standard acceptance limits.
    """
    inputs = dict(coefficient=2.0, **liquid_inputs())
    inputs[field] = value
    with pytest.raises(ValueError):
        liquid_capacity(**inputs)


@pytest.mark.parametrize("rate", [-1.0, float("nan"), float("inf"), -float("inf")])
def test_invalid_liquid_demand(rate):
    """Construct non-finite/negative demand, outside forward-flow sizing."""
    with pytest.raises(ValueError):
        required_liquid_coefficient(rate=rate, **liquid_inputs())
