"""Islanding detection — ROCOF and vector shift methods.

Implements rate-of-change-of-frequency (ROCOF) via linear regression
and voltage vector shift detection per IEEE 1547.4 §6.4.

References
----------
IEEE 1547.4-2011 §6.4 — Islanding detection requirements.
"""

from __future__ import annotations

from dataclasses import dataclass


@dataclass
class DetectionResult:
    """Result of islanding detection evaluation.

    Attributes
    ----------
    tripped : bool
        True if any detection method triggered.
    rocof_value : float
        Computed ROCOF [Hz/s].
    vector_shift_value : float
        Computed vector shift [degrees].
    rocof_tripped : bool
        True if |ROCOF| exceeded threshold.
    vector_shift_tripped : bool
        True if vector shift exceeded threshold.
    """

    tripped: bool
    rocof_value: float
    vector_shift_value: float
    rocof_tripped: bool
    vector_shift_tripped: bool


class IslandDetector:
    """Islanding detector using ROCOF and vector shift methods.

    Parameters
    ----------
    rocof_threshold : float
        ROCOF trip threshold [Hz/s]. Default 1.0 per IEEE 1547.4 §6.4.
    vector_shift_threshold : float
        Vector shift trip threshold [degrees]. Default 10.0.
    """

    def __init__(
        self,
        rocof_threshold: float = 1.0,
        vector_shift_threshold: float = 10.0,
    ) -> None:
        self.rocof_threshold = rocof_threshold
        self.vector_shift_threshold = vector_shift_threshold

    @staticmethod
    def compute_rocof(
        timestamps: list[float],
        frequencies: list[float],
    ) -> float:
        """Compute rate-of-change-of-frequency via linear regression.

        Fits f(t) = a + b*t using least-squares; ROCOF = b [Hz/s].

        Parameters
        ----------
        timestamps : list[float]
            Time values [seconds].
        frequencies : list[float]
            Frequency measurements [Hz].

        Returns
        -------
        float
            ROCOF slope [Hz/s].

        Raises
        ------
        ValueError
            If fewer than 2 samples provided.
        """
        n = len(timestamps)
        if n < 2:
            raise ValueError(f"Need at least 2 samples, got {n}")
        # Linear regression: slope = (n*Σxy - Σx*Σy) / (n*Σx² - (Σx)²)
        sum_t = sum(timestamps)
        sum_f = sum(frequencies)
        sum_tf = sum(t * f for t, f in zip(timestamps, frequencies))
        sum_t2 = sum(t * t for t in timestamps)
        denom = n * sum_t2 - sum_t * sum_t
        if denom == 0.0:
            return 0.0
        return (n * sum_tf - sum_t * sum_f) / denom

    @staticmethod
    def compute_vector_shift(
        angle_prev: float,
        angle_curr: float,
    ) -> float:
        """Compute voltage vector shift magnitude.

        Handles angular wrapping at 360 degrees, returning the
        minimum absolute shift [0, 180].

        Parameters
        ----------
        angle_prev : float
            Previous voltage angle [degrees].
        angle_curr : float
            Current voltage angle [degrees].

        Returns
        -------
        float
            Absolute angular shift [degrees], in [0, 180].
        """
        diff = (angle_curr - angle_prev) % 360.0
        if diff > 180.0:
            diff = 360.0 - diff
        return diff

    def detect(
        self,
        timestamps: list[float],
        frequencies: list[float],
        voltage_angle_prev: float,
        voltage_angle_curr: float,
    ) -> DetectionResult:
        """Run full islanding detection evaluation.

        Parameters
        ----------
        timestamps : list[float]
            Time values [seconds].
        frequencies : list[float]
            Frequency measurements [Hz].
        voltage_angle_prev : float
            Previous voltage angle [degrees].
        voltage_angle_curr : float
            Current voltage angle [degrees].

        Returns
        -------
        DetectionResult
            Detection outcome with per-method results.
        """
        rocof = self.compute_rocof(timestamps, frequencies)
        shift = self.compute_vector_shift(voltage_angle_prev, voltage_angle_curr)
        rocof_tripped = abs(rocof) >= self.rocof_threshold
        vs_tripped = shift >= self.vector_shift_threshold
        return DetectionResult(
            tripped=rocof_tripped or vs_tripped,
            rocof_value=rocof,
            vector_shift_value=shift,
            rocof_tripped=rocof_tripped,
            vector_shift_tripped=vs_tripped,
        )
