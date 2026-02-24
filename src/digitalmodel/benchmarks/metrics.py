"""
Benchmark metrics — time-domain statistics, seed equivalence, frequency-domain
comparison, pass/fail classification, and consolidated summary.

Constants (tolerance gates from WRK-126)
-----------------------------------------
    SEED_COV_THRESHOLD      0.10   (10% CoV max for seed equivalence)
    RAO_AMP_TOLERANCE_PCT   5.0    (5% RAO amplitude tolerance)
    RAO_PHASE_TOLERANCE_DEG 10.0   (10 deg RAO phase tolerance)
    TENSION_TOLERANCE_PCT   1.0    (1% statics tension tolerance)
    BENDING_TOLERANCE_PCT   5.0    (5% statics bending tolerance)
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Dict, List, Optional

import numpy as np

from .inventory import ModelCategory

# ---------------------------------------------------------------------------
# Tolerance constants (acceptance criteria from WRK-126 plan)
# ---------------------------------------------------------------------------
SEED_COV_THRESHOLD: float = 0.10
RAO_AMP_TOLERANCE_PCT: float = 5.0
RAO_PHASE_TOLERANCE_DEG: float = 10.0
TENSION_TOLERANCE_PCT: float = 1.0
BENDING_TOLERANCE_PCT: float = 5.0


# ---------------------------------------------------------------------------
# TimeDomainStats
# ---------------------------------------------------------------------------
@dataclass
class TimeDomainStats:
    """Statistics from one time-domain simulation run.

    Parameters
    ----------
    seed:   Random wave seed used for this run.
    mean, std, maximum, minimum, rms: Scalar statistics of the response.
    extra:  Optional named metrics (e.g. tension_max_kN).
    """
    seed: int
    mean: float
    std: float
    maximum: float
    minimum: float
    rms: float
    extra: Dict[str, float] = field(default_factory=dict)


def compute_time_domain_stats(
    signal: np.ndarray, seed: int
) -> TimeDomainStats:
    """Compute statistics from a 1-D time-domain response array.

    Parameters
    ----------
    signal: 1-D numpy array of the response time history.
    seed:   The seed label to attach to the result.

    Returns
    -------
    TimeDomainStats

    Raises
    ------
    ValueError: If *signal* is empty.
    """
    if signal.size == 0:
        raise ValueError("Cannot compute stats from empty signal")

    return TimeDomainStats(
        seed=seed,
        mean=float(np.mean(signal)),
        std=float(np.std(signal, ddof=0)),
        maximum=float(np.max(signal)),
        minimum=float(np.min(signal)),
        rms=float(np.sqrt(np.mean(signal ** 2))),
    )


# ---------------------------------------------------------------------------
# SeedEquivalenceResult
# ---------------------------------------------------------------------------
@dataclass
class SeedEquivalenceResult:
    """CoV / convergence summary across multiple seed runs.

    Parameters
    ----------
    model_name: Name of the model being analysed.
    n_seeds:    Number of seeds used.
    seeds:      List of seed values.
    cov_mean, cov_std, cov_max: CoV statistics across amplitude metrics.
    converged:  True when cov_max < SEED_COV_THRESHOLD.
    """
    model_name: str
    n_seeds: int
    seeds: List[int]
    cov_mean: float
    cov_std: float
    cov_max: float
    converged: bool


def check_seed_equivalence(
    model_name: str,
    stats_list: List[TimeDomainStats],
) -> SeedEquivalenceResult:
    """Analyse seed-to-seed statistical convergence.

    Uses amplitude-normalised CoV across std, rms, abs_max, abs_min.
    Process mean is excluded: for zero-mean wave responses it varies
    naturally across seeds and is not a meaningful convergence indicator.

    Parameters
    ----------
    model_name:  Identifier (used in the result record).
    stats_list:  Per-seed statistics (minimum 2 required).

    Raises
    ------
    ValueError: If fewer than 2 seeds are supplied.
    """
    if len(stats_list) < 2:
        raise ValueError(
            f"check_seed_equivalence requires at least 2 seeds; "
            f"got {len(stats_list)} for model '{model_name}'"
        )

    seeds = [s.seed for s in stats_list]

    # Amplitude reference: mean rms across seeds
    rms_arr = np.array([s.rms for s in stats_list])
    ref_scale = max(float(np.mean(rms_arr)), 1e-12)

    # Amplitude metrics (all positive and physically meaningful)
    metrics: Dict[str, np.ndarray] = {
        "std": np.array([s.std for s in stats_list]),
        "rms": rms_arr,
        "abs_max": np.abs([s.maximum for s in stats_list]),
        "abs_min": np.abs([s.minimum for s in stats_list]),
    }

    # CoV per metric = std_of_metric / ref_scale
    covs = [
        float(np.std(v, ddof=1)) / ref_scale
        for v in metrics.values()
    ]
    cov_arr = np.array(covs)

    return SeedEquivalenceResult(
        model_name=model_name,
        n_seeds=len(stats_list),
        seeds=seeds,
        cov_mean=float(np.mean(cov_arr)),
        cov_std=float(np.std(cov_arr, ddof=0)),
        cov_max=float(np.max(cov_arr)),
        converged=float(np.max(cov_arr)) < SEED_COV_THRESHOLD,
    )


# ---------------------------------------------------------------------------
# FrequencyDomainMetrics
# ---------------------------------------------------------------------------
@dataclass
class FrequencyDomainMetrics:
    """Comparison metrics between reference and computed frequency-domain RAOs.

    Parameters
    ----------
    model_name:         Model identifier.
    frequency_hz:       Frequency array (Hz).
    rao_reference:      Reference RAO amplitudes.
    rao_computed:       Computed RAO amplitudes.
    max_amp_diff_pct:   Maximum % amplitude difference across frequencies.
    mean_amp_diff_pct:  Mean % amplitude difference across frequencies.
    max_phase_diff_deg: Optional maximum phase difference in degrees.
    """
    model_name: str
    frequency_hz: np.ndarray
    rao_reference: np.ndarray
    rao_computed: np.ndarray
    max_amp_diff_pct: float
    mean_amp_diff_pct: float
    max_phase_diff_deg: Optional[float] = None


def compare_frequency_domain(
    model_name: str,
    frequency_hz: np.ndarray,
    rao_reference: np.ndarray,
    rao_computed: np.ndarray,
    phase_reference: Optional[np.ndarray] = None,
    phase_computed: Optional[np.ndarray] = None,
) -> FrequencyDomainMetrics:
    """Compare frequency-domain RAO amplitudes (and optionally phases).

    Parameters
    ----------
    model_name:        Model identifier for the result record.
    frequency_hz:      Frequency array (must match RAO array lengths).
    rao_reference:     Reference RAO amplitudes.
    rao_computed:      Computed RAO amplitudes.
    phase_reference, phase_computed: Optional phase arrays in degrees.

    Raises
    ------
    ValueError: If arrays are empty or shapes are incompatible.
    """
    if len(frequency_hz) == 0:
        raise ValueError(
            f"compare_frequency_domain: frequency array is empty "
            f"for model '{model_name}'"
        )
    if rao_reference.shape != rao_computed.shape:
        raise ValueError(
            f"compare_frequency_domain: rao_reference shape "
            f"{rao_reference.shape} does not match rao_computed shape "
            f"{rao_computed.shape} for model '{model_name}'"
        )
    if frequency_hz.shape != rao_reference.shape:
        raise ValueError(
            f"compare_frequency_domain: frequency_hz shape "
            f"{frequency_hz.shape} does not match rao_reference shape "
            f"{rao_reference.shape} for model '{model_name}'"
        )

    eps = 1e-12
    denom = np.maximum(np.abs(rao_reference), eps)
    amp_diff_pct = np.abs(rao_reference - rao_computed) / denom * 100.0

    max_phase: Optional[float] = None
    if phase_reference is not None and phase_computed is not None:
        phase_diff = np.abs(phase_reference - phase_computed)
        phase_diff = np.minimum(
            phase_diff % 360.0, 360.0 - phase_diff % 360.0)
        max_phase = float(np.max(phase_diff))

    return FrequencyDomainMetrics(
        model_name=model_name,
        frequency_hz=frequency_hz,
        rao_reference=rao_reference,
        rao_computed=rao_computed,
        max_amp_diff_pct=float(np.max(amp_diff_pct)),
        mean_amp_diff_pct=float(np.mean(amp_diff_pct)),
        max_phase_diff_deg=max_phase,
    )


# ---------------------------------------------------------------------------
# BenchmarkPassFail
# ---------------------------------------------------------------------------
class BenchmarkPassFail(str, Enum):
    """Benchmark result status."""
    PASS = "pass"
    WARN = "warn"
    FAIL = "fail"
    SKIP = "skip"


def classify_benchmark_result(
    cov: float,
    tolerance: float,
    warn_factor: float = 0.8,
) -> BenchmarkPassFail:
    """Map a scalar metric to a pass/warn/fail state.

    Parameters
    ----------
    cov:         The metric value to classify.
    tolerance:   The pass threshold.
    warn_factor: PASS when cov < warn_factor * tolerance; WARN when
                 warn_factor * tolerance <= cov <= tolerance; FAIL otherwise.
    """
    if cov < warn_factor * tolerance:
        return BenchmarkPassFail.PASS
    if cov <= tolerance:
        return BenchmarkPassFail.WARN
    return BenchmarkPassFail.FAIL


# ---------------------------------------------------------------------------
# BenchmarkSummary
# ---------------------------------------------------------------------------
@dataclass
class BenchmarkSummary:
    """Consolidated per-model benchmark result.

    Parameters
    ----------
    model_name:               Model identifier.
    category:                 Analysis domain category.
    statics_pass:             Whether static analysis converged.
    time_domain_status:       Time-domain benchmark outcome.
    frequency_domain_status:  Frequency-domain outcome (SKIP if N/A).
    seed_equivalence_status:  Seed equivalence outcome (SKIP if N/A).
    overall_status:           Worst-case aggregate status.
    seed_cov:                 Maximum CoV from seed equivalence.
    freq_max_diff_pct:        Maximum frequency-domain amplitude diff.
    notes:                    Optional human-readable notes.
    """
    model_name: str
    category: ModelCategory
    statics_pass: bool
    time_domain_status: BenchmarkPassFail
    frequency_domain_status: BenchmarkPassFail
    seed_equivalence_status: BenchmarkPassFail
    overall_status: BenchmarkPassFail
    seed_cov: Optional[float] = None
    freq_max_diff_pct: Optional[float] = None
    notes: Optional[str] = None


def _worst_status(*statuses: BenchmarkPassFail) -> BenchmarkPassFail:
    """Return the most severe status from a collection."""
    order = [BenchmarkPassFail.FAIL, BenchmarkPassFail.WARN,
             BenchmarkPassFail.PASS, BenchmarkPassFail.SKIP]
    for candidate in order:
        if candidate in statuses:
            return candidate
    return BenchmarkPassFail.SKIP


def build_benchmark_summary(
    model_name: str,
    category: ModelCategory,
    statics_pass: bool,
    seed_equiv_result: Optional[SeedEquivalenceResult],
    freq_metrics: Optional[FrequencyDomainMetrics],
) -> BenchmarkSummary:
    """Produce a consolidated BenchmarkSummary for one model.

    Parameters
    ----------
    model_name:        Model identifier.
    category:          ModelCategory classification.
    statics_pass:      Whether the statics check passed.
    seed_equiv_result: Output of check_seed_equivalence (None if N/A).
    freq_metrics:      Output of compare_frequency_domain (None if N/A).
    """
    if not statics_pass:
        return BenchmarkSummary(
            model_name=model_name,
            category=category,
            statics_pass=False,
            time_domain_status=BenchmarkPassFail.SKIP,
            frequency_domain_status=BenchmarkPassFail.SKIP,
            seed_equivalence_status=BenchmarkPassFail.SKIP,
            overall_status=BenchmarkPassFail.FAIL,
            notes="Statics check failed — downstream tests skipped",
        )

    td_status = BenchmarkPassFail.SKIP
    seed_status = BenchmarkPassFail.SKIP
    seed_cov: Optional[float] = None

    if category in (ModelCategory.TIME_DOMAIN, ModelCategory.BOTH):
        if seed_equiv_result is not None:
            seed_cov = seed_equiv_result.cov_max
            td_status = classify_benchmark_result(
                cov=seed_equiv_result.cov_max,
                tolerance=SEED_COV_THRESHOLD,
            )
            seed_status = td_status

    fd_status = BenchmarkPassFail.SKIP
    freq_max: Optional[float] = None

    if category in (ModelCategory.FREQUENCY_DOMAIN, ModelCategory.BOTH):
        if freq_metrics is not None:
            freq_max = freq_metrics.max_amp_diff_pct
            fd_status = classify_benchmark_result(
                cov=freq_metrics.max_amp_diff_pct / 100.0,
                tolerance=RAO_AMP_TOLERANCE_PCT / 100.0,
            )

    non_skip = [s for s in (td_status, fd_status, seed_status)
                if s != BenchmarkPassFail.SKIP]
    overall = _worst_status(*non_skip) if non_skip else BenchmarkPassFail.PASS

    return BenchmarkSummary(
        model_name=model_name,
        category=category,
        statics_pass=statics_pass,
        time_domain_status=td_status,
        frequency_domain_status=fd_status,
        seed_equivalence_status=seed_status,
        overall_status=overall,
        seed_cov=seed_cov,
        freq_max_diff_pct=freq_max,
    )
