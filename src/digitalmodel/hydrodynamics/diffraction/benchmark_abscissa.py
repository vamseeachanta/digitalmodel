"""Fail-closed abscissa alignment for diffraction benchmarks."""

from dataclasses import dataclass

import numpy as np
from numpy.typing import ArrayLike, NDArray


DEFAULT_JUSTIFICATION = (
    "Physics-derived, not fitted to benchmark data: for the lowest expected "
    "damping ratio zeta=0.10 and limiting natural frequency omega_n=11 rad/s, "
    "the half-power bandwidth is approximately 2*zeta*omega_n=2.2 rad/s. "
    "Using two intervals across that bandwidth gives MAX_GAP <= "
    "zeta*omega_n=1.1 rad/s; MIN_SAMPLES=5 retains the peak and two samples "
    "on each flank, and MIN_COVERAGE=0.5 requires half the narrower source "
    "domain to be shared."
)


class AbscissaOrderError(ValueError):
    """Raised when comparison input is not strictly increasing."""


class AbscissaOverlapError(ValueError):
    """Raised when source abscissae do not have adequate interval coverage."""


class AbscissaGapError(ValueError):
    """Raised when a source cannot resolve the shared interval."""


@dataclass(frozen=True)
class AbscissaConfig:
    """Physics-based thresholds for an abscissa comparison."""

    min_samples: int = 5
    min_coverage: float = 0.5
    max_gap: float = 1.1
    justification: str = DEFAULT_JUSTIFICATION


@dataclass(frozen=True)
class OrderedSolverData:
    """Frequency-indexed solver values normalized at ingestion."""

    frequencies: NDArray[np.float64]
    raos: NDArray[np.generic]


@dataclass(frozen=True)
class ComplexResponse:
    """Magnitude and phase derived from a complex transfer function."""

    magnitude: NDArray[np.float64]
    phase_degrees: NDArray[np.float64]


@dataclass(frozen=True)
class AlignedResponses:
    """Two responses evaluated on the declared comparison grid."""

    frequencies: NDArray[np.float64]
    first: ComplexResponse
    second: ComplexResponse


@dataclass(frozen=True)
class InsufficientSampling:
    """A comparison refusal with no fabricated correlation."""

    sample_count: int
    correlation: None = None


AlignmentResult = AlignedResponses | InsufficientSampling


def order_solver_data(frequencies: ArrayLike, raos: ArrayLike) -> OrderedSolverData:
    """Sort frequencies and their RAO rows together at ingestion."""
    frequency_array = np.asarray(frequencies, dtype=float)
    rao_array = np.asarray(raos)
    if frequency_array.ndim != 1:
        raise ValueError("frequencies must be one-dimensional")
    if rao_array.ndim == 0 or rao_array.shape[0] != frequency_array.size:
        raise ValueError("RAO leading dimension must match frequencies")

    sort_index = np.argsort(frequency_array)
    return OrderedSolverData(
        frequencies=frequency_array[sort_index],
        raos=rao_array[sort_index, ...],
    )


def build_evaluation_grid(
    first: ArrayLike,
    second: ArrayLike,
    config: AbscissaConfig | None = None,
) -> NDArray[np.float64]:
    """Return the coarser source grid inside the adequate shared interval."""
    active_config = config or AbscissaConfig()
    first_array = _validated_abscissa(first, "first")
    second_array = _validated_abscissa(second, "second")
    lower = max(first_array[0], second_array[0])
    upper = min(first_array[-1], second_array[-1])
    if upper <= lower:
        raise AbscissaOverlapError("abscissae are disjoint")

    smaller_span = min(np.ptp(first_array), np.ptp(second_array))
    coverage = (upper - lower) / smaller_span
    if coverage < active_config.min_coverage:
        raise AbscissaOverlapError(
            f"shared-interval coverage {coverage:.6f} is below minimum "
            f"{active_config.min_coverage:.6f}"
        )

    _check_source_gap(first_array, lower, upper, "first", active_config.max_gap)
    _check_source_gap(second_array, lower, upper, "second", active_config.max_gap)
    first_grid = first_array[(first_array >= lower) & (first_array <= upper)]
    second_grid = second_array[(second_array >= lower) & (second_array <= upper)]
    return (
        first_grid.copy() if first_grid.size <= second_grid.size else second_grid.copy()
    )


def interpolate_complex_response(
    frequencies: ArrayLike,
    magnitude: ArrayLike,
    phase_degrees: ArrayLike,
    evaluation_grid: ArrayLike,
) -> ComplexResponse:
    """Interpolate real and imaginary transfer-function components."""
    source = _validated_abscissa(frequencies, "source")
    target = np.asarray(evaluation_grid, dtype=float)
    magnitude_array = np.asarray(magnitude, dtype=float)
    phase_array = np.asarray(phase_degrees, dtype=float)
    if (
        magnitude_array.shape != phase_array.shape
        or magnitude_array.shape[0] != source.size
    ):
        raise ValueError(
            "magnitude and phase must share the frequency leading dimension"
        )
    if target.ndim != 1 or np.any(target < source[0]) or np.any(target > source[-1]):
        raise ValueError("evaluation grid must remain inside the source abscissa")

    transfer = magnitude_array * np.exp(1j * np.deg2rad(phase_array))
    trailing_shape = transfer.shape[1:]
    flattened = transfer.reshape(source.size, -1)
    interpolated = np.empty((target.size, flattened.shape[1]), dtype=complex)
    for column in range(flattened.shape[1]):
        real = np.interp(target, source, flattened[:, column].real)
        imaginary = np.interp(target, source, flattened[:, column].imag)
        interpolated[:, column] = real + 1j * imaginary
    reshaped = interpolated.reshape((target.size, *trailing_shape))
    return ComplexResponse(np.abs(reshaped), np.rad2deg(np.angle(reshaped)))


def assess_sampling(
    first: ArrayLike,
    second: ArrayLike,
    config: AbscissaConfig | None = None,
) -> InsufficientSampling | None:
    """Refuse correlation when either compared array has too few samples."""
    active_config = config or AbscissaConfig()
    first_array = np.asarray(first)
    second_array = np.asarray(second)
    first_count = first_array.shape[0] if first_array.ndim else 0
    second_count = second_array.shape[0] if second_array.ndim else 0
    sample_count = min(first_count, second_count)
    if sample_count < active_config.min_samples:
        return InsufficientSampling(sample_count=sample_count, correlation=None)
    return None


def align_responses(
    first_frequencies: ArrayLike,
    first_magnitude: ArrayLike,
    first_phase_degrees: ArrayLike,
    second_frequencies: ArrayLike,
    second_magnitude: ArrayLike,
    second_phase_degrees: ArrayLike,
    config: AbscissaConfig | None = None,
) -> AlignmentResult:
    """Align two responses and always apply sampling adequacy afterward."""
    active_config = config or AbscissaConfig()
    first_abscissa = _validated_abscissa(first_frequencies, "first")
    second_abscissa = _validated_abscissa(second_frequencies, "second")
    grid = build_evaluation_grid(first_abscissa, second_abscissa, active_config)
    first_response = _response_on_grid(
        first_abscissa, first_magnitude, first_phase_degrees, grid
    )
    second_response = _response_on_grid(
        second_abscissa, second_magnitude, second_phase_degrees, grid
    )
    sampling = assess_sampling(
        first_response.magnitude, second_response.magnitude, active_config
    )
    if sampling is not None:
        return sampling
    return AlignedResponses(grid, first_response, second_response)


def _response_on_grid(
    frequencies: NDArray[np.float64],
    magnitude: ArrayLike,
    phase_degrees: ArrayLike,
    grid: NDArray[np.float64],
) -> ComplexResponse:
    if not np.array_equal(frequencies, grid):
        return interpolate_complex_response(frequencies, magnitude, phase_degrees, grid)

    magnitude_array = np.asarray(magnitude, dtype=float)
    phase_array = np.asarray(phase_degrees, dtype=float)
    if (
        magnitude_array.shape != phase_array.shape
        or magnitude_array.shape[0] != grid.size
    ):
        raise ValueError(
            "magnitude and phase must share the frequency leading dimension"
        )
    return ComplexResponse(magnitude_array.copy(), phase_array.copy())


def _validated_abscissa(values: ArrayLike, label: str) -> NDArray[np.float64]:
    array = np.asarray(values, dtype=float)
    if array.ndim != 1 or array.size < 2 or not np.all(np.isfinite(array)):
        raise AbscissaOrderError(f"{label} abscissa must be strictly increasing")
    if np.any(np.diff(array) <= 0.0):
        raise AbscissaOrderError(f"{label} abscissa must be strictly increasing")
    return array


def _check_source_gap(
    frequencies: NDArray[np.float64],
    lower: float,
    upper: float,
    label: str,
    maximum: float,
) -> None:
    gaps = np.diff(frequencies)
    intersects_shared = (frequencies[:-1] < upper) & (frequencies[1:] > lower)
    shared_gaps = gaps[intersects_shared]
    largest = float(np.max(shared_gaps)) if shared_gaps.size else 0.0
    if largest > maximum:
        raise AbscissaGapError(
            f"{label} source gap {largest:.6f} exceeds maximum {maximum:.6f}"
        )
