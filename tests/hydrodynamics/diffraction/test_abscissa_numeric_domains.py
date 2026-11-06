"""Policy domains derived from benchmark_abscissa, without solver execution.

The sample floor follows _validated_abscissa: two endpoints define an interval.
Coverage is shared span / smaller span, hence 0 < coverage <= 1. A relative
gap is a positive difference / positive frequency, hence a positive finite bound.
All other numbers below are synthetic boundary probes, not physical criteria.
Python Library Reference, Built-in Types, Boolean Type (Python 3) explains why
bool inherits int; truth values are excluded from all three policy domains.
"""

from pathlib import Path
import runpy
from unittest.mock import Mock

import numpy as np
import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_abscissa import (
    AbscissaConfig,
)
from digitalmodel.hydrodynamics.diffraction.benchmark_matrix_alignment import (
    align_matrix_sets,
)


@pytest.mark.parametrize("field", ["min_samples", "min_coverage", "max_relative_gap"])
@pytest.mark.parametrize("value", [True, False, np.bool_(True), np.bool_(False)])
@pytest.mark.parametrize("custom_justification", [True, False])
def test_truth_values_are_not_numeric_policy(field, value, custom_justification):
    """Truth values must fail by field name before justification is inspected."""
    kwargs = {field: value}
    if custom_justification:
        kwargs["justification"] = "Synthetic type-domain probe."
    with pytest.raises(ValueError, match=field):
        AbscissaConfig(**kwargs)


@pytest.mark.parametrize("field", ["min_samples", "min_coverage", "max_relative_gap"])
@pytest.mark.parametrize("value", [-np.inf, None, [1], np.array([1]), 1j])
def test_unsupported_domains_fail_without_coercion(field, value):
    """None is the sole coverage sentinel; arrays are not scalar Real values."""
    if field == "min_coverage" and value is None:
        assert AbscissaConfig(min_coverage=None).min_coverage is None
        return
    with pytest.raises(ValueError, match=field):
        AbscissaConfig(**{field: value}, justification="Synthetic invalid policy.")


@pytest.mark.parametrize(
    "field,value",
    [
        ("min_samples", np.int64(2)),
        ("min_samples", np.int32(7)),
        ("min_coverage", 1),
        ("min_coverage", np.float32(0.5)),
        ("min_coverage", np.float64(1.0)),
        ("max_relative_gap", 1),
        ("max_relative_gap", np.float32(0.5)),
        ("max_relative_gap", np.float64(1.0)),
    ],
)
def test_numeric_scalars_remain_supported(field, value):
    """Integer unity is numeric; binary-exact halves avoid rounding criteria."""
    config = AbscissaConfig(**{field: value}, justification="Synthetic valid policy.")
    assert getattr(config, field) == value
    assert type(getattr(config, field)) is type(value)


@pytest.mark.parametrize("field", ["min_samples", "min_coverage", "max_relative_gap"])
@pytest.mark.parametrize("value", [True, np.nan, np.inf])
def test_malformed_policy_prevents_matrix_caller(field, value, two_identical_results):
    """Construction must reject malformed policy before public caller entry."""
    first, second = two_identical_results.values()
    caller = Mock(wraps=align_matrix_sets)
    with pytest.raises(ValueError, match=field):
        caller(
            first.added_mass,
            second.added_mass,
            config=AbscissaConfig(
                **{field: value}, justification="Synthetic nested policy."
            ),
        )
    caller.assert_not_called()


def test_worked_example(capsys):
    """Independent hand calculation for the synthetic example.

    Coarse grid: 1 + k/4, k = 0..4. Dense grid: 1 + k/8, k = 0..8.
    Shared coverage = (2 - 1)/(2 - 1) = 1. Largest relative gaps occur at
    the first interval: (1.25 - 1)/1 = 0.25 and (1.125 - 1)/1 = 0.125.
    Both satisfy the declared 0.25 limit, so the five coarse points remain.
    """
    example = (
        Path(__file__).resolve().parents[3]
        / "examples/hydrodynamics/diffraction/abscissa_policy.py"
    )
    namespace = runpy.run_path(str(example), run_name="__main__")
    np.testing.assert_array_equal(
        namespace["build_example_grid"](), [1.0, 1.25, 1.5, 1.75, 2.0]
    )
    output = capsys.readouterr().out
    assert "min_coverage: rejected boolean" in output
    assert "max_relative_gap: rejected boolean" in output
