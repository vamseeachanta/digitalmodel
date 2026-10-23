# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Numeric domains are mathematical, independent of physical default choices.

At least two points define an interval, coverage is a positive ratio at most
unity, and a relative-gap bound must be finite and positive. Values below
are synthetic domain probes rather than engineering acceptance criteria.
"""

import math
from unittest.mock import Mock

import pytest

from digitalmodel.hydrodynamics.diffraction.benchmark_abscissa import AbscissaConfig


@pytest.mark.parametrize("field", ["min_samples", "min_coverage", "max_relative_gap"])
@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf, True, False, "1"])
def test_invalid_nested_numeric_domain_prevents_caller(field, value):
    from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
        ComparisonPolicy,
    )

    caller = Mock()
    with pytest.raises(ValueError, match=field):
        policy = ComparisonPolicy(
            abscissa_config=AbscissaConfig(**{field: value}, justification="domain probe")
        )
        caller(policy)
    caller.assert_not_called()


@pytest.mark.parametrize("value", ["", " ", None, 1])
def test_custom_policy_requires_nonblank_text_justification(value):
    with pytest.raises(ValueError, match="justification"):
        AbscissaConfig(max_relative_gap=1.0, justification=value)


def test_physical_defaults_are_unchanged():
    from digitalmodel.hydrodynamics.diffraction.benchmark_abscissa import (
        DEFAULT_JUSTIFICATION, MAX_RELATIVE_GAP, MIN_SAMPLES,
    )

    config = AbscissaConfig()
    assert (config.min_samples, config.min_coverage, config.max_relative_gap,
            config.justification) == (MIN_SAMPLES, None, MAX_RELATIVE_GAP,
                                      DEFAULT_JUSTIFICATION)


def test_finite_integer_gap_does_not_acquire_an_arbitrary_upper_limit():
    """A synthetic large integer is finite and strictly positive by construction."""
    value = 10**100
    assert AbscissaConfig(max_relative_gap=value, justification="domain probe").max_relative_gap == value


def test_large_integer_coverage_is_a_domain_error():
    """A synthetic integer greater than unity is outside the coverage domain."""
    with pytest.raises(ValueError, match="min_coverage"):
        AbscissaConfig(min_coverage=10**100, justification="domain probe")
