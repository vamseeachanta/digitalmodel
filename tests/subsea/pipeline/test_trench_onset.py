"""Trench correction tests based on the equation in issue #1731.

All geometry, frequencies, mass and velocities below are synthetic test inputs,
not standard or solver reference values. Expected trench ratios are independent
hand calculations of that issue's equation; no standards qualification is made.
"""

from dataclasses import replace

import pytest

from digitalmodel.subsea.pipeline.free_span.models import PipeSpanInput
from digitalmodel.subsea.pipeline.free_span.span_onset_screening import (
    SpanOnsetScreening,
)


@pytest.fixture
def span_input():
    """Synthetic geometry in metres: D = 0.5, gap = 0.25, wall = 0.02, L = 10."""
    return PipeSpanInput(
        od_m=0.5,
        wt_m=0.02,
        span_length_m=10.0,
        seabed_gap_m=0.25,
    )


def screening(inp):
    """Hold both frequencies at 1 Hz and effective mass at 1 kg/m for isolation."""
    return SpanOnsetScreening(inp, fn_IL=1.0, fn_CF=1.0, m_e=1.0)


@pytest.mark.parametrize(
    "gap, legacy_threshold",
    [(-0.25, 24 / 13), (0.0, 24 / 13), (0.25, 111 / 52), (1.0, 30 / 13)],
)
def test_zero_trench_preserves_cf_onset(span_input, gap, legacy_threshold):
    """Issue #1731: zero trench gives factor 1, preserving legacy gap handling.

    For nonnegative e, (1.25 * 0 - e) / D <= 0, so delta_D = 0 and
    factor = 1 + 0.5 * 0 = 1. The existing negative-gap clamp is also preserved.

    Legacy hand calculations use the public, pre-existing formula documented
    in span_onset_screening.py, with the existing default gamma_on_CF = 1.3:
      e <= 0: psi = 0.2 * 4 = 0.8; threshold = 3 * 0.8 / 1.3 = 24/13.
      e = 0.25: e/D = 0.5; psi = 0.2 * (4 + 1.25 * 0.5) = 0.925;
                threshold = 3 * 0.925 / 1.3 = 111/52.
      e = 1: e/D = 2; psi = 1; threshold = 3 / 1.3 = 30/13.
    These are compatibility checks against existing code, not a standard.
    """
    default = replace(span_input, seabed_gap_m=gap)
    explicit = replace(default, trench_depth_m=0.0)
    assert default.trench_depth_m == 0.0
    assert screening(default).cf_onset_velocity() == pytest.approx(legacy_threshold)
    assert screening(explicit).cf_onset_velocity() == screening(default).cf_onset_velocity()
    assert screening(explicit).screening_flags() == screening(default).screening_flags()


@pytest.mark.parametrize(
    "depth, diameter, gap, expected_ratio",
    [
        (0.1, 0.5, 0.25, 1.0),
        (0.2, 0.5, 0.25, 1.0),
        (0.4, 0.5, 0.25, 1.25),
        (0.6, 0.5, 0.25, 1.5),
        (1.0, 0.5, 0.25, 1.5),
        (0.4, 1.0, 0.25, 1.125),
        (0.2, 0.5, 0.0, 1.25),
    ],
)
def test_trench_clamps(span_input, depth, diameter, gap, expected_ratio):
    """Hand calculations from issue #1731; lengths are in metres.

    For D = 0.5 and e = 0.25:
      t = 0.1: (0.125 - 0.25) / 0.5 = -0.25 -> 0 -> factor 1.
      t = 0.2: (0.25 - 0.25) / 0.5 = 0 -> factor 1 (lower boundary).
      t = 0.4: (0.5 - 0.25) / 0.5 = 0.5 -> factor 1.25 (interior).
      t = 0.6: (0.75 - 0.25) / 0.5 = 1 -> factor 1.5 (upper boundary).
      t = 1.0: (1.25 - 0.25) / 0.5 = 2 -> 1 -> factor 1.5.
    With D = 1, e = 0.25, t = 0.4: delta_D = 0.25 -> factor 1.125.
    With D = 0.5, e = 0, t = 0.2: delta_D = 0.5 -> factor 1.25.
    These last cases independently check the diameter and proximity mappings.
    """
    base = replace(span_input, od_m=diameter, seabed_gap_m=gap)
    trenched = replace(base, trench_depth_m=depth)
    assert screening(trenched).cf_onset_velocity() == pytest.approx(
        screening(base).cf_onset_velocity() * expected_ratio
    )


@pytest.mark.parametrize(
    "velocity_ratio, base_onset, trenched_onset",
    [(0.9, False, False), (1.1, True, False), (1.3, True, True)],
)
def test_trench_changes_cf_screening(span_input, velocity_ratio, base_onset, trenched_onset):
    """Issue #1731 hand calculation: t = 0.4, e = 0.25, D = 0.5 gives 1.25.

    delta_D = (1.25 * 0.4 - 0.25) / 0.5 = 0.5; factor = 1.25.
    Synthetic reduced velocities at 0.9, 1.1 and 1.3 times the untrenched
    threshold lie below both thresholds, between them, and above both.
    U = Ur * 1 Hz * 0.5 m. Only the CF threshold and flag may change.
    """
    threshold = screening(span_input).cf_onset_velocity()
    base = replace(span_input, current_velocity_ms=threshold * velocity_ratio * 0.5)
    trenched = replace(base, trench_depth_m=0.4)
    before = screening(base).screening_flags()
    after = screening(trenched).screening_flags()

    assert before.cf_viv_onset is base_onset
    assert after.cf_viv_onset is trenched_onset
    assert after.Ur_onset_CF == pytest.approx(before.Ur_onset_CF * 1.25)
    assert after.Ur_onset_CF > before.Ur_onset_CF
    assert after.Ks == before.Ks
    assert after.Ur_IL == before.Ur_IL
    assert after.Ur_CF == before.Ur_CF
    assert after.Ur_onset_IL == before.Ur_onset_IL
    assert after.il_viv_onset is before.il_viv_onset


@pytest.mark.parametrize("depth", [-0.1, float("nan"), float("inf"), -float("inf")])
def test_invalid_trench_depth_raises(span_input, depth):
    """A synthetic negative length and every nonfinite float violate the domain."""
    with pytest.raises(ValueError, match="trench_depth_m.*finite.*nonnegative"):
        replace(span_input, trench_depth_m=depth)
