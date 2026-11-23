#!/usr/bin/env python3
"""
ABOUTME: dm#1528 slice 3 - first fluid sloshing natural period (from the linear
tanh dispersion relation, provenance retained) plus deterministic sweep
generation over fill fraction x conduit capacity x T_roll/T_natural, with
refined sampling around the natural-period response region (ratio = 1) and
stable, reproducible case IDs / manifest hashes.
"""

from __future__ import annotations

import hashlib
import itertools
import json
from dataclasses import dataclass, field
from typing import Tuple

from loguru import logger

from .spectral_analysis import GRAVITY, prismatic_tank_natural_frequency

# Schema version for the generated manifest (bump on breaking field changes).
SWEEP_SCHEMA_VERSION = "1.0"

# Label + provenance for the analytical approximation used for the natural period.
_APPROXIMATION_LABEL = "linear_potential_tanh_dispersion"
_PROVENANCE = (
    "First fluid sloshing mode from the linear-potential prismatic-tank "
    "dispersion relation omega^2 = (n*pi*g/L)*tanh(n*pi*h/L) "
    "(spectral_analysis.prismatic_tank_natural_frequency); reference geometry "
    "is a prismatic tank; approximation is linear/inviscid and strictly valid "
    "for small-amplitude sloshing. See digitalmodel #1528 slice 3."
)

# Rounding used to normalize inputs before hashing (kills float noise so IDs are
# reproducible across platforms) and to snap generated ratios.
_ROUND = 6


# ============================================================================
# Natural period
# ============================================================================


@dataclass(frozen=True)
class NaturalPeriodResult:
    """First sloshing natural period for a prismatic reference tank.

    Attributes:
        fill_fraction: Nominal fill fraction (0, 1) exclusive.
        fill_depth: Still-water fill depth h = fill_fraction * tank_height (m).
        length: Tank length L in the sloshing direction (m).
        tank_height: Tank internal height (m).
        mode: Sloshing mode number n (>= 1); 1 = first/fundamental mode.
        gravity: Gravitational acceleration used (m/s^2).
        natural_frequency_hz: First-mode natural frequency f_n (Hz).
        natural_period_s: First-mode natural period T_n = 1/f_n (s).
        approximation: Short machine label for the approximation used.
        provenance: Human-readable provenance / validity note (retained).
    """

    fill_fraction: float
    fill_depth: float
    length: float
    tank_height: float
    mode: int
    gravity: float
    natural_frequency_hz: float
    natural_period_s: float
    approximation: str
    provenance: str


def first_sloshing_natural_period(
    length: float,
    tank_height: float,
    fill_fraction: float,
    *,
    mode: int = 1,
    gravity: float = GRAVITY,
) -> NaturalPeriodResult:
    """Compute the first fluid sloshing natural period for a prismatic tank.

    The still-water fill depth is ``h = fill_fraction * tank_height`` and the
    natural frequency comes from the linear tanh dispersion relation via
    :func:`spectral_analysis.prismatic_tank_natural_frequency` (built upon, not
    duplicated). The approximation label and provenance are retained on the
    result per #1528.

    Args:
        length: Tank length L in the sloshing direction (m).
        tank_height: Tank internal height H (m).
        fill_fraction: Nominal fill fraction in the open interval (0, 1).
        mode: Sloshing mode number n (>= 1).
        gravity: Gravitational acceleration (m/s^2).

    Returns:
        NaturalPeriodResult with frequency, period, and provenance.

    Raises:
        ValueError: If ``fill_fraction`` is not in (0, 1), or geometry / mode /
            gravity are non-physical.
    """
    if not (0.0 < fill_fraction < 1.0):
        raise ValueError(
            f"fill_fraction must be in the open interval (0, 1); got "
            f"{fill_fraction!r}. Use a nominal fill strictly between empty and full."
        )
    if length <= 0:
        raise ValueError(f"length must be positive; got {length!r}.")
    if tank_height <= 0:
        raise ValueError(f"tank_height must be positive; got {tank_height!r}.")

    fill_depth = fill_fraction * tank_height
    # Delegates dispersion + mode/gravity validation to the spectral module.
    f_n = prismatic_tank_natural_frequency(
        length, fill_depth, mode=mode, gravity=gravity
    )
    if f_n <= 0:  # pragma: no cover - guarded upstream, defensive only
        raise ValueError("Computed a non-positive natural frequency.")
    return NaturalPeriodResult(
        fill_fraction=float(fill_fraction),
        fill_depth=float(fill_depth),
        length=float(length),
        tank_height=float(tank_height),
        mode=int(mode),
        gravity=float(gravity),
        natural_frequency_hz=float(f_n),
        natural_period_s=float(1.0 / f_n),
        approximation=_APPROXIMATION_LABEL,
        provenance=_PROVENANCE,
    )


def period_ratio(roll_period: float, natural_period: float) -> float:
    """Return the resonance ratio T_roll / T_natural.

    A value near 1.0 indicates the imposed roll period is close to the fluid's
    first sloshing natural period (the response region refined by the sweep).

    Args:
        roll_period: Imposed vessel roll period T_roll (s).
        natural_period: Fluid first sloshing natural period T_natural (s).

    Returns:
        Dimensionless ratio T_roll / T_natural.

    Raises:
        ValueError: If either period is non-positive.
    """
    if roll_period <= 0:
        raise ValueError(f"roll_period must be positive; got {roll_period!r}.")
    if natural_period <= 0:
        raise ValueError(
            f"natural_period must be positive; got {natural_period!r}."
        )
    return roll_period / natural_period


# ============================================================================
# Sweep configuration
# ============================================================================


@dataclass(frozen=True)
class SweepConfig:
    """Deterministic sweep specification for the reference prismatic tank.

    The sweep is a full factorial over ``fill_fractions`` x
    ``conduit_capacities`` x effective period ratios, where the effective ratio
    set augments ``base_period_ratios`` with a refined cluster of
    ``resonance_points`` samples inside +/- ``resonance_band`` of
    ``resonance_center`` (default 1.0, the natural-period response region).

    Attributes:
        length: Tank length L in the sloshing direction (m).
        tank_height: Tank internal height H (m).
        study_name: Prefix for generated case IDs (also part of the ID hash).
        mode: Sloshing mode number n for the natural period (>= 1).
        gravity: Gravitational acceleration (m/s^2).
        fill_fractions: Nominal fills; default covers the required set.
        conduit_capacities: Dimensionless conduit-capacity levels (>= 0);
            ``0.0`` is the no-flow control. Units documented in the manifest.
        base_period_ratios: Base T_roll/T_natural sampling spanning the
            operating range.
        resonance_center: Ratio around which sampling is refined (default 1.0).
        resonance_band: Half-width of the refinement/near-resonance band.
        resonance_points: Number of refined samples across the band (>= 1).
    """

    length: float
    tank_height: float
    study_name: str = ""
    mode: int = 1
    gravity: float = GRAVITY
    fill_fractions: Tuple[float, ...] = (0.10, 0.25, 0.40, 0.50, 0.65, 0.80)
    conduit_capacities: Tuple[float, ...] = (0.0, 0.05, 0.10)
    base_period_ratios: Tuple[float, ...] = (0.7, 0.85, 1.0, 1.2, 1.5)
    resonance_center: float = 1.0
    resonance_band: float = 0.15
    resonance_points: int = 5

    # Units recorded in the manifest for the (dimensionless) conduit capacity.
    conduit_capacity_units: str = "fraction_tank_volume_per_half_cycle"

    def effective_period_ratios(self) -> Tuple[float, ...]:
        """Return the sorted, de-duplicated ratio set (base + refined band).

        The refined samples are a deterministic linear spacing across
        ``[center - band, center + band]``; values are rounded so the union is
        reproducible.

        Returns:
            Ascending tuple of unique T_roll/T_natural ratios.
        """
        refined = _linspace(
            self.resonance_center - self.resonance_band,
            self.resonance_center + self.resonance_band,
            self.resonance_points,
        )
        merged = {round(r, _ROUND) for r in self.base_period_ratios}
        merged.update(round(r, _ROUND) for r in refined)
        return tuple(sorted(merged))

    def schema_key(self) -> dict:
        """Manifest-level identity (excludes the per-case sweep axes)."""
        return {
            "version": SWEEP_SCHEMA_VERSION,
            "study": self.study_name,
            "length_m": round(self.length, _ROUND),
            "tank_height_m": round(self.tank_height, _ROUND),
            "mode": int(self.mode),
            "gravity": round(self.gravity, _ROUND),
        }


# ============================================================================
# Sweep case + manifest
# ============================================================================


@dataclass(frozen=True)
class SweepCase:
    """One deterministic sweep case (a matrix row).

    Attributes:
        case_id: Stable ID = ``<study>-<12 hex>`` (or bare hash if no study),
            hashed from the normalized case-defining inputs.
        fill_fraction: Nominal fill fraction.
        fill_depth: Still-water fill depth (m).
        conduit_capacity: Dimensionless conduit-capacity level.
        period_ratio: Target T_roll / T_natural for this case.
        natural_frequency_hz: First-mode natural frequency (Hz).
        natural_period_s: First-mode natural period (s).
        roll_frequency_hz: Imposed roll frequency (Hz) = 1 / roll_period_s.
        roll_period_s: Imposed roll period = period_ratio * natural_period_s (s).
        near_resonance: True when the ratio is within the resonance band.
        approximation: Natural-period approximation label (retained).
    """

    case_id: str
    fill_fraction: float
    fill_depth: float
    conduit_capacity: float
    period_ratio: float
    natural_frequency_hz: float
    natural_period_s: float
    roll_frequency_hz: float
    roll_period_s: float
    near_resonance: bool
    approximation: str


@dataclass(frozen=True)
class SweepManifest:
    """Deterministic, reproducible manifest for a generated sweep.

    Attributes:
        schema_version: Manifest schema version.
        study_name: Study prefix.
        provenance: Natural-period approximation provenance (retained).
        length: Tank length L (m).
        tank_height: Tank internal height H (m).
        mode: Sloshing mode number.
        gravity: Gravitational acceleration (m/s^2).
        conduit_capacity_units: Units label for conduit_capacity values.
        cases: Ordered tuple of SweepCase rows.
        content_hash: 16-hex digest over the ordered case IDs + schema version;
            identical inputs always yield the identical hash.
    """

    schema_version: str
    study_name: str
    provenance: str
    length: float
    tank_height: float
    mode: int
    gravity: float
    conduit_capacity_units: str
    cases: Tuple[SweepCase, ...]
    content_hash: str


def generate_sweep(config: SweepConfig) -> SweepManifest:
    """Generate the deterministic fill x conduit x period-ratio sweep.

    Cases are produced as a full factorial in a fixed, sorted order (fill, then
    conduit capacity, then ratio) so the output - including case IDs and the
    manifest content hash - is byte-for-byte reproducible.

    Args:
        config: The sweep specification.

    Returns:
        SweepManifest with one SweepCase per combination.

    Raises:
        ValueError: If any fill/conduit/ratio value is non-physical.
    """
    if not config.fill_fractions:
        raise ValueError("At least one fill fraction is required.")
    for cap in config.conduit_capacities:
        if cap < 0:
            raise ValueError(
                f"conduit_capacity must be >= 0; got {cap!r} "
                "(0.0 is the no-flow control)."
            )
    if not config.conduit_capacities:
        raise ValueError("At least one conduit capacity is required.")

    ratios = config.effective_period_ratios()
    if not ratios:
        raise ValueError("At least one period ratio is required.")
    for r in ratios:
        if r <= 0:
            raise ValueError(
                f"period ratio must be positive; got {r!r}."
            )

    # Natural period depends only on fill -> compute once per fill (validates).
    natural_by_fill = {
        fill: first_sloshing_natural_period(
            config.length, config.tank_height, fill,
            mode=config.mode, gravity=config.gravity,
        )
        for fill in config.fill_fractions
    }

    cases: list[SweepCase] = []
    n_control = 0
    for fill, cap, ratio in itertools.product(
        sorted(config.fill_fractions),
        sorted(config.conduit_capacities),
        ratios,
    ):
        nat = natural_by_fill[fill]
        roll_period = ratio * nat.natural_period_s
        near = abs(ratio - config.resonance_center) <= config.resonance_band
        if cap == 0.0:
            n_control += 1
        case_id = _make_case_id(config, fill, cap, ratio)
        cases.append(
            SweepCase(
                case_id=case_id,
                fill_fraction=round(fill, _ROUND),
                fill_depth=round(nat.fill_depth, _ROUND),
                conduit_capacity=round(cap, _ROUND),
                period_ratio=round(ratio, _ROUND),
                natural_frequency_hz=nat.natural_frequency_hz,
                natural_period_s=nat.natural_period_s,
                roll_frequency_hz=1.0 / roll_period,
                roll_period_s=roll_period,
                near_resonance=bool(near),
                approximation=nat.approximation,
            )
        )

    content_hash = _content_hash(config.schema_key(), [c.case_id for c in cases])
    logger.info(
        "Generated dm#1528 sweep '{}': {} cases ({} controls), hash {}",
        config.study_name or "(unnamed)", len(cases), n_control, content_hash,
    )
    return SweepManifest(
        schema_version=SWEEP_SCHEMA_VERSION,
        study_name=config.study_name,
        provenance=_PROVENANCE,
        length=float(config.length),
        tank_height=float(config.tank_height),
        mode=int(config.mode),
        gravity=float(config.gravity),
        conduit_capacity_units=config.conduit_capacity_units,
        cases=tuple(cases),
        content_hash=content_hash,
    )


# ============================================================================
# Private helpers
# ============================================================================


def _linspace(a: float, b: float, n: int) -> list[float]:
    """Deterministic inclusive linear spacing (pure Python, no numpy noise)."""
    if n < 1:
        raise ValueError(f"resonance_points must be >= 1; got {n!r}.")
    if n == 1:
        return [(a + b) / 2.0]
    return [a + (b - a) * i / (n - 1) for i in range(n)]


def _canonical(obj: dict) -> str:
    """Deterministic JSON encoding (sorted keys, compact) for hashing."""
    return json.dumps(obj, sort_keys=True, separators=(",", ":"))


def _make_case_id(
    config: SweepConfig, fill: float, cap: float, ratio: float
) -> str:
    """Stable case ID from normalized case-defining inputs."""
    payload = {
        "study": config.study_name,
        "length_m": round(config.length, _ROUND),
        "tank_height_m": round(config.tank_height, _ROUND),
        "mode": int(config.mode),
        "gravity": round(config.gravity, _ROUND),
        "fill_fraction": round(fill, _ROUND),
        "conduit_capacity": round(cap, _ROUND),
        "period_ratio": round(ratio, _ROUND),
    }
    digest = hashlib.sha256(_canonical(payload).encode("utf-8")).hexdigest()[:12]
    return f"{config.study_name}-{digest}" if config.study_name else digest


def _content_hash(schema_key: dict, case_ids: list[str]) -> str:
    """16-hex digest over the schema key and the ordered case IDs."""
    payload = _canonical({"schema": schema_key, "cases": case_ids})
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()[:16]


