"""Mooring QRA composition with a real, solver-free catenary output.

All quantities are synthetic arithmetic assumptions. Independent hand case:
weight = 1000 N/m = 1 kN/m, depth = 100 m, top pretension = 300 kN.
The catenary identity T = H + w*h gives H = 200 kN. Its suspended length
satisfies s*s = h*h + 2*(H/w)*h = 50000 square metres, so the 500 m line
has seabed contact. QRA: 0.02 events/year * 0.5 fatalities/event = 0.01/year.
The catenary identity is the method documented by solve_catenary, citing
Sea Loads on Ships and Offshore Structures (1990), chapter 6. It does not
determine the assumed event frequency.
"""

import io

import pytest
from pydantic import ValidationError

from digitalmodel.asset_integrity.safety_case import (
    EvaluationStatus,
    MAHRegister,
    assess_mooring_failure,
)


@pytest.fixture
def design():
    from digitalmodel.orcaflex.mooring_design import solve_catenary

    return solve_catenary(
        water_depth=100,
        line_length=500,
        submerged_weight_per_m=1000,
        pretension=300,
    )


def assess(design, **changes):
    return assess_mooring_failure(
        design,
        **(
            dict(
                hazard_id="line-break",
                description="Single line break",
                frequency_per_year=0.02,
                fatalities_per_event=0.5,
                frequency_basis="Synthetic event rate, not derived from tension",
                consequence_basis="Synthetic conditional mean fatalities",
                barriers=("Inspection",),
                responsible_roles=("Integrity engineer",),
            )
            | changes
        ),
    )


def test_real_catenary_composition_and_register_round_trip(design):
    result = assess(design)
    assert result.top_tension_kn == pytest.approx(300)
    assert result.horizontal_tension_kn == pytest.approx(200)
    assert result.hazard.expected_fatalities_per_year == pytest.approx(0.01)
    register = MAHRegister(hazards=(result.hazard,))
    assert MAHRegister.from_csv(io.StringIO(register.to_csv())) == register


def test_unknown_frequency_is_not_evaluated(design):
    result = assess(design, frequency_per_year=None)
    assert result.hazard.status is EvaluationStatus.NOT_EVALUATED
    assert result.hazard.expected_fatalities_per_year is None


@pytest.mark.parametrize(
    "field,value",
    [
        ("frequency_per_year", -1),
        ("frequency_per_year", float("nan")),
        ("fatalities_per_event", float("inf")),
        ("frequency_basis", ""),
    ],
)
def test_invalid_qra_input_rejected(design, field, value):
    with pytest.raises(ValidationError):
        assess(design, **{field: value})


@pytest.mark.parametrize(
    "changes",
    [
        {"top_tension": float("nan")},
        {"horizontal_tension": -1},
        {"horizontal_tension": 400},
    ],
)
def test_invalid_design_output_rejected(design, changes):
    with pytest.raises(ValueError):
        assess(design.model_copy(update=changes))


def test_tension_does_not_imply_a_failure_rate(design):
    """Doubling load evidence leaves an explicitly supplied event rate unchanged."""
    changed = design.model_copy(update={"top_tension": 600, "horizontal_tension": 400})
    assert (
        assess(changed).hazard.expected_fatalities_per_year
        == assess(design).hazard.expected_fatalities_per_year
    )
