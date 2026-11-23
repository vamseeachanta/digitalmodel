# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Regression cases for unsupported criteria in wave-only operability.

All numbers are synthetic test inputs, not operational recommendations.
The independent hand calculation is documented in ``wave_scatter`` below.
"""

import json
import runpy
from pathlib import Path

import pytest
from pydantic import ValidationError

from digitalmodel.orcaflex.weather_window import OperabilityInput, ScatterDiagram


@pytest.fixture
def wave_scatter():
    """Supply a synthetic probability table and its independent answer.

    Hs upper edges = [1, 2] m, Tp upper edges = [6, 8] s;
    row probabilities = [[0.1, 0.2], [0.3, 0.4]], total = 1.0.
    With Hs <= 2 m and Tp <= 6 s, the accepted disjoint cells sum to
    0.1 + 0.3 = 0.4; operability = 40.0%, complement = 60.0%.
    With Hs <= 2 m alone, all cells pass: 100.0%.
    Source: independent hand calculation using the addition/complement rules;
    OpenStax, Introductory Statistics 2e (2023), section 3.3.
    """
    return ScatterDiagram(
        hs_bins=[1.0, 2.0],
        tp_bins=[6.0, 8.0],
        occurrences=[[0.1, 0.2], [0.3, 0.4]],
    )


@pytest.mark.parametrize("field", ["wind_limit_mps", "current_limit_mps"])
def test_active_non_wave_limit_is_rejected(wave_scatter, field):
    """Wave-only probability is 100%; either extra criterion is unevaluable.

    The synthetic speed limit of 1 m/s is explicitly supplied. There are no
    wind/current observations in the table, so no combined percentage exists.
    Expected rejection follows the issue's apply-or-reject contract.
    """
    with pytest.raises(ValidationError, match=field):
        OperabilityInput(hs_limit=2.0, scatter=wave_scatter, **{field: 1.0})


def test_both_unsupported_limits_are_named(wave_scatter):
    """Both synthetic 1 m/s criteria lack observations; name both omissions."""
    with pytest.raises(ValidationError) as exc:
        OperabilityInput(
            hs_limit=2.0,
            wind_limit_mps=1.0,
            current_limit_mps=1.0,
            scatter=wave_scatter,
        )
    assert "wind_limit_mps" in str(exc.value)
    assert "current_limit_mps" in str(exc.value)
    assert "Hs/Tp" in str(exc.value)


def test_json_input_cannot_silently_ignore_criteria(wave_scatter):
    """The same synthetic 1 m/s criterion is rejected through JSON validation."""
    payload = {
        "hs_limit": 2.0,
        "wind_limit_mps": 1.0,
        "scatter": wave_scatter.model_dump(),
    }
    with pytest.raises(ValidationError, match="wind_limit_mps"):
        OperabilityInput.model_validate_json(json.dumps(payload))


@pytest.mark.parametrize("field", ["wind_limit_mps", "current_limit_mps"])
@pytest.mark.parametrize("bypass", ["assignment", "model_copy", "model_construct"])
def test_calculation_rechecks_active_criteria(wave_scatter, field, bypass):
    """Changing a synthetic 1 m/s limit after validation still denies a result.

    Ordinary assignment and Pydantic's trusted-data copy/construct APIs do not
    run the model validator by default. The calculation must check again.
    """
    data = {"hs_limit": 2.0, "scatter": wave_scatter}
    if bypass == "model_construct":
        operation = OperabilityInput.model_construct(**data, **{field: 1.0})
    else:
        operation = OperabilityInput(**data)
        if bypass == "assignment":
            setattr(operation, field, 1.0)
        else:
            operation = operation.model_copy(update={field: 1.0})

    with pytest.raises(ValueError, match=field):
        operation.calculate_operability()


@pytest.mark.parametrize("explicit_none", [False, True])
def test_wave_only_result_retains_hand_calculated_values(wave_scatter, explicit_none):
    """The fixture gives 40.0% operability, 60.0% complement at Tp <= 6 s."""
    limits = {"wind_limit_mps": None, "current_limit_mps": None} if explicit_none else {}
    result = OperabilityInput(
        hs_limit=2.0, tp_limit=6.0, scatter=wave_scatter, **limits
    ).calculate_operability()
    assert result == {
        "operability_pct": 40.0,
        "non_operability_pct": 60.0,
        "hs_limit_m": 2.0,
        "tp_limit_s": 6.0,
    }


def test_wave_only_without_period_limit(wave_scatter):
    """All four synthetic cells pass Hs <= 2 m: 100.0%, complement 0.0%."""
    result = OperabilityInput(hs_limit=2.0, scatter=wave_scatter).calculate_operability()
    assert result["operability_pct"] == 100.0
    assert result["non_operability_pct"] == 0.0
    assert result["tp_limit_s"] is None


def test_unsupported_limit_fails_before_scatter_evaluation(wave_scatter, monkeypatch):
    """The synthetic 1 m/s wind criterion must prevent even reading wave cells."""
    operation = OperabilityInput(hs_limit=2.0, scatter=wave_scatter)
    operation.wind_limit_mps = 1.0

    def unexpected_evaluation(*args):
        pytest.fail("Wave probability evaluated despite an unsupported criterion")

    monkeypatch.setattr(ScatterDiagram, "total_probability_below", unexpected_evaluation)
    with pytest.raises(ValueError, match="wind_limit_mps"):
        operation.calculate_operability()


def test_worked_example_matches_hand_calculation_and_rejections(capsys):
    """The fixture's 40.0/60.0% answer accompanies two NOT_EVALUATED criteria."""
    example = (
        Path(__file__).resolve().parents[2]
        / "examples/solvers/orcaflex/wave_only_operability.py"
    )
    runpy.run_path(str(example), run_name="__main__")
    result = json.loads(capsys.readouterr().out)
    assert result["wave_only"]["operability_pct"] == 40.0
    assert result["wave_only"]["non_operability_pct"] == 60.0
    assert set(result["unsupported_criteria"]) == {
        "wind_limit_mps", "current_limit_mps"
    }
    for field, criterion in result["unsupported_criteria"].items():
        assert criterion["status"] == "NOT_EVALUATED"
        assert field in criterion["reason"]
        assert "operability_pct" not in criterion
