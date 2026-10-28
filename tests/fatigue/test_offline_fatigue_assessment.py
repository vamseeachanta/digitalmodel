# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Offline assessment tests with an independently calculated synthetic basis.

All numerical fixtures are chosen here, not taken from a design standard.
The curve is N(R) = 1000 / R**2, with R a stress RANGE in MPa.
For samples [0, 6, 2, 4, -2, 5, 0], the inner excursion 2 -> 4
closes between 6 and -2: one full cycle of range 2, mean 3.
Removing that pair leaves [0, 6, -2, 5, 0]. Adjacent residual pairs
give half-cycles (range, mean): (6, 3), (8, 2), (7, 1.5), (5, 2.5).
Thus sum(n) = 1 + 4/2 = 3, and Miner damage is
  1*2**2/1000 + (6**2 + 8**2 + 7**2 + 5**2)/(2*1000)
  = (4 + 18 + 32 + 24.5 + 12.5)/1000 = 0.091.
Two time units of exposure repeated over ten units of life multiply damage
by 10/2 = 5, giving 0.455. Doubling every stress multiplies damage by
2**2 = 4, giving exposure damage 0.364 and life damage 1.82.
The tolerances are pytest's default approximate floating-point comparisons.
"""

import os
import subprocess
import sys
from dataclasses import replace
from pathlib import Path

import numpy as np
import pytest

from digitalmodel.fatigue.orcaflex_fatigue import (
    CurveBasis,
    FatigueReport,
    StressHistory,
    fatigue_assessment,
)


class SyntheticCurve:
    """Chosen power law, accepting ranges rather than stress amplitudes."""

    def cycles(self, stress_ranges):
        return 1000.0 / np.asarray(stress_ranges) ** 2


@pytest.fixture
def history():
    return StressHistory([0, 6, 2, 4, -2, 5, 0], stress_unit="MPa")


@pytest.fixture
def basis():
    return CurveBasis(
        description="Synthetic N(R) = 1000 / R**2; no corrections",
        stress_unit="MPa",
    )


def assess(histories, basis, **changes):
    """The synthetic example explicitly uses years for BOTH durations."""
    options = dict(
        exposure_duration=2.0, design_life=10.0, time_unit="year",
        curve_basis=basis,
    )
    options.update(changes)
    return fatigue_assessment(histories, SyntheticCurve(), **options)


def test_known_cycles_match_miner_sum(history, basis):
    report = assess({"detail": history}, basis)
    assert isinstance(report, FatigueReport)
    location = report.locations["detail"]
    # Hand enumeration in the module docstring; order is immaterial.
    counted = sorted((c.stress_range, c.mean_stress, c.cycles)
                     for c in location.cycles)
    assert counted == [
        (2.0, 3.0, 1.0), (5.0, 2.5, 0.5), (6.0, 3.0, 0.5),
        (7.0, 1.5, 0.5), (8.0, 2.0, 0.5),
    ]
    assert sum(c.cycles for c in location.cycles) == 3.0
    for cycle in location.cycles:
        assert cycle.allowable_cycles == pytest.approx(1000 / cycle.stress_range**2)
        assert cycle.damage == pytest.approx(
            cycle.cycles * cycle.stress_range**2 / 1000
        )
    assert location.exposure_damage == pytest.approx(0.091)
    assert location.design_life_damage == pytest.approx(0.455)
    assert report.curve_basis == basis
    assert report.exposure_duration == 2.0
    assert report.design_life == 10.0
    assert report.time_unit == "year"
    assert report.half_cycle_policy == "retain"


def test_exposure_scales_damage(history, basis):
    reference = assess({"detail": history}, basis)
    longer_exposure = assess({"detail": history}, basis, exposure_duration=10.0)
    longer_life = assess({"detail": history}, basis, design_life=20.0)
    assert reference.exposure_scale == 5.0
    assert longer_exposure.exposure_scale == 1.0
    assert longer_exposure.locations["detail"].exposure_damage == pytest.approx(0.091)
    assert longer_exposure.locations["detail"].design_life_damage == pytest.approx(
        0.091
    )
    # Twenty/two = ten repetitions of the same 0.091 exposure damage.
    assert longer_life.locations["detail"].design_life_damage == pytest.approx(0.91)


def test_governing_location(history, basis):
    doubled = replace(history, samples=np.asarray(history.samples) * 2)
    report = assess({"lower": history, "higher": doubled}, basis)
    assert report.governing_location == "higher"
    assert report.locations["higher"].exposure_damage == pytest.approx(0.364)
    assert report.locations["higher"].design_life_damage == pytest.approx(1.82)


def test_governing_ties_are_lexical_and_independent_of_input_order(history, basis):
    for names in (("zeta", "alpha"), ("alpha", "zeta")):
        report = assess(dict.fromkeys(names, history), basis)
        assert report.governing_location == "alpha"
        assert list(report.locations) == ["alpha", "zeta"]


@pytest.mark.parametrize("samples", [
    [], [0], [0, np.nan], [np.inf, 0], [0, -np.inf], [[0, 1]],
    [0, "bad"], [0, 1j],
])
def test_invalid_history(samples, basis):
    with pytest.raises(ValueError, match="history"):
        assess({"detail": StressHistory(samples, "MPa")}, basis)


@pytest.mark.parametrize("samples, mask", [
    ([0, 2], [False, True]),
    ([0, 2, 0], [False, True, False]),
    ([0, 2], [True, True]),
    ([5, 5, 5], [False, True, False]),
])
def test_masked_history_samples_are_rejected(samples, mask, basis):
    history = StressHistory(np.ma.array(samples, mask=mask), "MPa")
    with pytest.raises(ValueError, match="history"):
        assess({"detail": history}, basis)


def test_history_without_masked_samples_is_accepted(basis):
    history = StressHistory(np.ma.array([0, 2], mask=False), "MPa")
    report = assess({"detail": history}, basis)
    # One range-2 half-cycle: 0.5 * 2**2 / 1000 = 0.002.
    assert report.locations["detail"].exposure_damage == pytest.approx(0.002)


@pytest.mark.parametrize("field", ["exposure_duration", "design_life"])
@pytest.mark.parametrize("value", [0, -1, np.nan, np.inf, -np.inf, True, "bad"])
def test_invalid_exposure_or_life(field, value, history, basis):
    with pytest.raises(ValueError, match=field):
        assess({"detail": history}, basis, **{field: value})


def test_constant_history_has_zero_damage(basis):
    # No stress excursion: no cycles, so the empty Miner sum is zero.
    report = assess({"detail": StressHistory([5, 5, 5], "MPa")}, basis)
    assert report.locations["detail"].cycles == ()
    assert report.locations["detail"].exposure_damage == 0.0
    assert report.locations["detail"].design_life_damage == 0.0


def test_two_sample_history_retains_half_cycle(basis):
    # Single range 2, half-cycle: 0.5*2**2/1000 = 0.002.
    report = assess({"detail": StressHistory([0, 2], "MPa")}, basis)
    assert report.locations["detail"].exposure_damage == pytest.approx(0.002)
    assert report.locations["detail"].cycles[0].cycles == 0.5


def test_no_implicit_unit_conversion(history, basis):
    with pytest.raises(ValueError, match="stress_unit"):
        assess({"detail": replace(history, stress_unit="Pa")}, basis)


@pytest.mark.parametrize("changes", [
    {"time_unit": ""}, {"time_unit": " "},
    {"curve_basis": CurveBasis("", "MPa")},
    {"curve_basis": CurveBasis("synthetic", "")},
])
def test_basis_and_time_unit_are_required(changes, history, basis):
    with pytest.raises(ValueError):
        assess({"detail": history}, basis, **changes)


@pytest.mark.parametrize("histories", [{}, {"": [0, 1]}, {" ": [0, 1]}, {1: [0, 1]}])
def test_named_histories_are_required(histories, basis):
    with pytest.raises(ValueError):
        assess(histories, basis)


def test_unitless_history_is_rejected(basis):
    with pytest.raises(ValueError, match="StressHistory"):
        assess({"detail": [0, 1]}, basis)


def test_overflow_in_exposure_scale_is_rejected(history, basis):
    # Binary64 limit probes; these are not physical duration assumptions.
    with pytest.raises(ValueError, match="exposure_scale"):
        assess({"detail": history}, basis,
               exposure_duration=np.finfo(float).tiny,
               design_life=np.finfo(float).max)


@pytest.mark.parametrize("allowable", [0, -1, np.nan, -np.inf])
def test_invalid_curve_endurance_is_rejected(history, basis, allowable):
    class InvalidCurve:
        def cycles(self, ranges):
            return np.full_like(ranges, allowable)

    with pytest.raises(ValueError, match="allowable_cycles"):
        fatigue_assessment({"detail": history}, InvalidCurve(), 10,
                           exposure_duration=2, time_unit="year", curve_basis=basis)


@pytest.mark.parametrize("allowable", [1000 + 1000j, 1000 + 0j, complex(np.inf, 1)])
def test_complex_curve_endurance_is_rejected(history, basis, allowable):
    class ComplexCurve:
        def cycles(self, ranges):
            return np.full(ranges.shape, allowable)

    with pytest.raises(ValueError, match="allowable_cycles"):
        fatigue_assessment(
            {"detail": history}, ComplexCurve(), 10,
            exposure_duration=2, time_unit="year", curve_basis=basis,
        )


@pytest.mark.parametrize("allowable", [1000.0, np.inf])
def test_masked_curve_endurance_is_rejected(history, basis, allowable):
    class MaskedCurve:
        def cycles(self, ranges):
            return np.ma.array(
                np.full(ranges.shape, allowable), mask=np.arange(ranges.size) == 0,
            )

    with pytest.raises(ValueError, match="allowable_cycles"):
        fatigue_assessment(
            {"detail": history}, MaskedCurve(), 10,
            exposure_duration=2, time_unit="year", curve_basis=basis,
        )


def test_curve_without_masked_counts_is_accepted(history, basis):
    class UnmaskedCurve:
        def cycles(self, ranges):
            return np.ma.array(np.full(ranges.shape, 1000.0), mask=False)

    report = fatigue_assessment(
        {"detail": history}, UnmaskedCurve(), 10,
        exposure_duration=2, time_unit="year", curve_basis=basis,
    )
    # One full cycle and four half-cycles: (1 + 4 * 0.5) / 1000 = 0.003.
    assert report.locations["detail"].exposure_damage == pytest.approx(0.003)


def test_infinite_endurance_means_zero_damage(history, basis):
    class EnduranceLimitCurve:
        def cycles(self, ranges):
            return np.full_like(ranges, np.inf)

    report = fatigue_assessment(
        {"detail": history}, EnduranceLimitCurve(), 10,
        exposure_duration=2, time_unit="year", curve_basis=basis,
    )
    assert report.locations["detail"].exposure_damage == 0.0


def test_inputs_are_not_modified(history, basis):
    samples_before = list(history.samples)
    histories = {"detail": history}
    assess(histories, basis)
    assert list(history.samples) == samples_before
    assert list(histories) == ["detail"]


def test_constant_history_does_not_evaluate_curve(basis):
    class UnusedCurve:
        def cycles(self, ranges):
            raise AssertionError("a constant history has no ranges to evaluate")

    report = fatigue_assessment(
        {"detail": StressHistory([5, 5, 5], "MPa")}, UnusedCurve(), 10,
        exposure_duration=2, time_unit="year", curve_basis=basis,
    )
    assert report.locations["detail"].design_life_damage == 0.0


def test_stress_range_overflow_is_rejected(basis):
    # Finite samples can still have a range exceeding the binary64 limit.
    limit = np.finfo(float).max
    with pytest.raises(ValueError, match="overflow"):
        assess({"detail": StressHistory([-limit, limit], "MPa")}, basis)


def test_curve_output_shape_must_match_ranges(history, basis):
    class ScalarCurve:
        def cycles(self, ranges):
            return 1000.0

    with pytest.raises(ValueError, match="allowable_cycles"):
        fatigue_assessment(
            {"detail": history}, ScalarCurve(), 10,
            exposure_duration=2, time_unit="year", curve_basis=basis,
        )


def test_fresh_process_never_requests_solver_binding():
    # The finder rejects both imports and availability queries before any binding
    # can be loaded. A fresh process also exercises package initialization.
    script = '''
import sys
from importlib.abc import MetaPathFinder

class NoSolverBinding(MetaPathFinder):
    def find_spec(self, fullname, path=None, target=None):
        if fullname.split(".")[0] == "OrcFxAPI":
            raise AssertionError("offline assessment requested a solver binding")
        return None

sys.meta_path.insert(0, NoSolverBinding())
from digitalmodel.fatigue.orcaflex_fatigue import (
    CurveBasis, StressHistory, fatigue_assessment,
)
class SyntheticCurve:
    def cycles(self, ranges):
        return 1000.0 / ranges**2

report = fatigue_assessment(
    {"detail": StressHistory([0, 6, 2, 4, -2, 5, 0], "MPa")},
    SyntheticCurve(), 10, exposure_duration=2, time_unit="year",
    curve_basis=CurveBasis("Synthetic N(R) = 1000 / R**2", "MPa"),
)
print(report.locations["detail"].exposure_damage)
'''
    env = dict(os.environ, PYTHONPATH=str(Path(__file__).resolve().parents[2] / "src"))
    result = subprocess.run(
        [sys.executable, "-c", script], env=env, capture_output=True,
        text=True, check=True,
    )
    assert float(result.stdout.strip()) == pytest.approx(0.091)
