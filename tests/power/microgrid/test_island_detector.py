"""Tests for IslandDetector — ROCOF, vector shift, trip logic."""

from __future__ import annotations

import pytest

from digitalmodel.power.microgrid.island_detector import IslandDetector


class TestROCOF:
    def test_declining_frequency(self):
        """60 → 59 Hz over 1 second = -1.0 Hz/s ROCOF."""
        det = IslandDetector()
        timestamps = [0.0, 0.25, 0.5, 0.75, 1.0]
        frequencies = [60.0, 59.75, 59.5, 59.25, 59.0]
        rocof = det.compute_rocof(timestamps, frequencies)
        assert rocof == pytest.approx(-1.0, abs=0.05)

    def test_stable_frequency(self):
        """Constant 60 Hz → ROCOF ≈ 0."""
        det = IslandDetector()
        timestamps = [0.0, 0.5, 1.0]
        frequencies = [60.0, 60.0, 60.0]
        rocof = det.compute_rocof(timestamps, frequencies)
        assert rocof == pytest.approx(0.0, abs=0.01)

    def test_insufficient_samples_raises(self):
        """Need at least 2 samples for linear regression."""
        det = IslandDetector()
        with pytest.raises(ValueError, match="samples"):
            det.compute_rocof([0.0], [60.0])

    def test_rising_frequency(self):
        """59 → 60 Hz over 1s = +1.0 Hz/s."""
        det = IslandDetector()
        timestamps = [0.0, 0.5, 1.0]
        frequencies = [59.0, 59.5, 60.0]
        rocof = det.compute_rocof(timestamps, frequencies)
        assert rocof == pytest.approx(1.0, abs=0.05)


class TestVectorShift:
    def test_small_shift(self):
        """10 → 15 degrees = 5 degree shift."""
        det = IslandDetector()
        shift = det.compute_vector_shift(10.0, 15.0)
        assert shift == pytest.approx(5.0)

    def test_wraps_at_180(self):
        """350 → 10 degrees = 20 degree shift (not 340)."""
        det = IslandDetector()
        shift = det.compute_vector_shift(350.0, 10.0)
        assert shift == pytest.approx(20.0)

    def test_negative_wrap(self):
        """10 → 350 degrees = 20 degree shift."""
        det = IslandDetector()
        shift = det.compute_vector_shift(10.0, 350.0)
        assert shift == pytest.approx(20.0)


class TestDetect:
    def test_trip_on_high_rocof(self):
        """ROCOF exceeding threshold triggers island detection."""
        det = IslandDetector(rocof_threshold=1.0, vector_shift_threshold=10.0)
        timestamps = [0.0, 0.25, 0.5, 0.75, 1.0]
        frequencies = [60.0, 59.5, 59.0, 58.5, 58.0]  # -2.0 Hz/s
        result = det.detect(
            timestamps=timestamps,
            frequencies=frequencies,
            voltage_angle_prev=0.0,
            voltage_angle_curr=0.0,
        )
        assert result.tripped is True
        assert result.rocof_tripped is True

    def test_trip_on_vector_shift(self):
        """Vector shift exceeding threshold triggers island detection."""
        det = IslandDetector(rocof_threshold=1.0, vector_shift_threshold=10.0)
        timestamps = [0.0, 0.5, 1.0]
        frequencies = [60.0, 60.0, 60.0]  # stable
        result = det.detect(
            timestamps=timestamps,
            frequencies=frequencies,
            voltage_angle_prev=0.0,
            voltage_angle_curr=15.0,  # 15 deg shift > 10 threshold
        )
        assert result.tripped is True
        assert result.vector_shift_tripped is True

    def test_no_trip_normal_conditions(self):
        """Normal grid conditions → no trip."""
        det = IslandDetector(rocof_threshold=1.0, vector_shift_threshold=10.0)
        timestamps = [0.0, 0.5, 1.0]
        frequencies = [60.0, 60.0, 60.0]
        result = det.detect(
            timestamps=timestamps,
            frequencies=frequencies,
            voltage_angle_prev=0.0,
            voltage_angle_curr=2.0,  # small shift
        )
        assert result.tripped is False
