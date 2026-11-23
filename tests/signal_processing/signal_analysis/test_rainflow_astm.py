"""Cycle counts in MPa and uncorrected Miner damage (m=3, log10(a)=12)."""

import numpy as np
import pytest
from scipy.signal import lfilter

from digitalmodel.signal_processing.signal_analysis import RainflowCounter


def miner_damage(ranges, counts):
    return np.sum(counts * ranges**3) / 1e12


# Repeat one sampled sinusoid so its extrema are identical across all periods.
period = -50.0 * np.cos(np.arange(32) * (2 * np.pi / 32))


@pytest.mark.parametrize(
    "signal, expected_counts, expected_damage",
    [
        # Hand-derived example in fatigue/counting_contract.py (CONTRACT_EXACT).
        ([-2, 1, -3, 5, -1, 3, -4, 4, -2], {3: 0.5, 4: 1.5, 6: 0.5, 8: 1, 9: 0.5}, 1.094e-9),
        (np.append(np.tile(period, 100), period[0]), {100: 100}, 1.000e-4),
        ([0, 100, 0, 40, 0], {100: 1, 40: 1}, 1.064e-6),
    ],
    ids=["worked-example", "100-sine-periods", "two-level"],
)
def test_weighted_range_counts_and_damage(signal, expected_counts, expected_damage):
    cycles = RainflowCounter().count_cycles(signal)
    assert cycles.groupby("range")["count"].sum().to_dict() == expected_counts
    assert miner_damage(cycles["range"], cycles["count"]) == pytest.approx(
        expected_damage, rel=1e-9, abs=0
    )


def test_descending_ranges_retain_both_residuals_and_their_indices():
    cycles = RainflowCounter().count_cycles([0, 10, 3])
    assert list(cycles.columns) == ["range", "mean", "count", "i_start", "i_end"]
    assert list(cycles.itertuples(index=False, name=None)) == [
        (10, 5, 0.5, 0, 1), (7, 6.5, 0.5, 1, 2)
    ]


def test_grouped_and_half_cycle_modes_preserve_original_reversal_indices():
    signal = [0, 8, 3, 3, 6, 0]
    grouped = RainflowCounter().count_cycles(signal)
    assert list(grouped.itertuples(index=False, name=None)) == [
        (3, 4.5, 1, 2, 4), (8, 4, 1, 0, 1)
    ]

    counter = RainflowCounter(method="half_cycle")
    cycles = counter.count_cycles(signal)
    assert list(cycles.itertuples(index=False, name=None)) == [
        (3, 4.5, 0.5, 2, 4), (3, 4.5, 0.5, 2, 4),
        (8, 4, 0.5, 0, 1), (8, 4, 0.5, 1, 5),
    ]
    compact = counter.count_cycles(signal, extract_info=False)
    assert compact.equals(cycles[["range", "mean", "count"]])


def test_million_sample_broadband_matches_pylife():
    pytest.importorskip("pylife")
    from digitalmodel.fatigue.rainflow import rainflow_count

    # Reproduce the seeded comparison history with a zero initial filter state.
    noise = np.random.default_rng(20260911).standard_normal(1_000_000)
    signal = lfilter([25 * np.sqrt(1 - 0.89**2)], [1, 0.89], noise)
    reference = rainflow_count(signal)
    cycles = RainflowCounter().count_cycles(signal, extract_info=False)

    edges = [0, 25, 50, 75, 100, np.inf]
    expected, _ = np.histogram(reference.stress_range, bins=edges, weights=reference.cycles)
    actual, _ = np.histogram(cycles["range"], bins=edges, weights=cycles["count"])
    np.testing.assert_array_equal(actual, expected)
    assert miner_damage(cycles["range"], cycles["count"]) == pytest.approx(
        miner_damage(reference.stress_range, reference.cycles), rel=1e-9, abs=0
    )
