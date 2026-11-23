#!/usr/bin/env python3
"""ABOUTME: Synchronized CFD time-history output extraction for sloshing/marine
interFoam cases (#1528, slice 5). Assembles a common-time-base bundle of
provenance-stamped series - free-surface elevation (spatial max/min/mean +
probe transect), wall pressure vs elevation and time, liquid mass / volume /
fill fraction with a mass-balance residual, liquid centre of mass, inlet/outlet
flow rate and integrated flow volume, and imposed roll angle / angular velocity
/ phase plus the roll-to-response phase lag. Every series carries units, a case
id, its source CFD field and a provenance note; a synchronized-time check fails
on mismatched / unsorted / ragged / non-finite time vectors. No OpenFOAM
install is required - the extractor operates on already-parsed arrays, and the
tests drive it with independently constructed synthetic (de-identified) signals.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Dict, Mapping, Optional, Tuple

import numpy as np
from numpy.typing import NDArray
from loguru import logger
from scipy.signal import hilbert

from .results_models import ProbeTimeSeries
from .pressure_taps import PressureTapStatistics, compute_tap_statistics
from .spectral_analysis import extract_natural_frequency

__all__ = [
    "TimeSeriesChannel",
    "ValidationFlags",
    "SynchronizedTimeHistory",
    "RawCFDOutputs",
    "ExtractionConfig",
    "extract_time_history",
    "validate_synchronized_time",
    "phase_lag_deg",
]


# ============================================================================
# Synchronized-time validation
# ============================================================================


def validate_synchronized_time(time: NDArray[np.float64]) -> NDArray[np.float64]:
    """Validate a common time base for a synchronized bundle.

    A valid time base is a 1-D, finite, strictly increasing vector of at least
    two samples. This is the synchronized-time gate: it rejects unsorted,
    ragged (non-1-D) and non-finite time vectors so downstream series cannot be
    silently misaligned.

    Args:
        time: Candidate time vector (s).

    Returns:
        The time vector as a contiguous ``float64`` array.

    Raises:
        ValueError: If the vector is not 1-D, has < 2 samples, contains
            non-finite entries, or is not strictly increasing (unsorted).
    """
    arr = np.asarray(time, dtype=np.float64)
    if arr.ndim != 1:
        raise ValueError(
            f"time base must be 1-D; got shape {arr.shape}. "
            "Ragged/multi-dim time vectors cannot be synchronized."
        )
    if arr.size < 2:
        raise ValueError("time base must contain at least two samples.")
    if not np.all(np.isfinite(arr)):
        raise ValueError("time base contains non-finite (nan/inf) samples.")
    if np.any(np.diff(arr) <= 0.0):
        raise ValueError(
            "time base must be strictly increasing (sorted, no duplicates)."
        )
    return arr


# ============================================================================
# TimeSeriesChannel: the per-series envelope (units + provenance)
# ============================================================================


@dataclass(frozen=True, eq=False)
class TimeSeriesChannel:
    """One extracted CFD series with explicit units and provenance.

    The envelope carries everything needed to interpret and audit a series: its
    ``times`` (the timestamps, s), ``values`` (1-D scalar or 2-D multichannel
    ``(n_times, n_locations)``), ``units``, the originating ``case_id``, the
    ``source_field`` (the CFD field / function object it derives from) and a
    free-form ``provenance`` note describing how it was computed. Optional
    ``locations`` give the probe coordinates for a multichannel series (e.g. the
    elevation transect or the pressure probes).

    Attributes:
        name: Channel key (e.g. ``"free_surface_elevation"``).
        times: Timestamps (s), 1-D, strictly increasing.
        values: Series values; shape ``(n_times,)`` or ``(n_times, n_loc)``.
        units: Physical units string (e.g. ``"m"``, ``"Pa"``, ``"m^3/s"``,
            ``"1"`` for dimensionless).
        case_id: Originating case identifier.
        source_field: CFD field / function object the series derives from.
        provenance: How the series was computed (audit note).
        locations: Optional probe coordinates ``(n_loc, 3)`` for a multichannel
            series.
        location_labels: Optional per-location labels aligned with columns.
    """

    name: str
    times: NDArray[np.float64]
    values: NDArray[np.float64]
    units: str
    case_id: str
    source_field: str
    provenance: str
    locations: Optional[NDArray[np.float64]] = None
    location_labels: Optional[Tuple[str, ...]] = None

    def __post_init__(self) -> None:
        for attr in ("name", "units", "case_id", "source_field", "provenance"):
            val = getattr(self, attr)
            if not isinstance(val, str) or not val.strip():
                raise ValueError(
                    f"TimeSeriesChannel.{attr} must be a non-empty string "
                    f"(got {val!r}); every series must carry units and provenance."
                )
        times = np.asarray(self.times, dtype=np.float64)
        values = np.asarray(self.values, dtype=np.float64)
        object.__setattr__(self, "times", times)
        object.__setattr__(self, "values", values)

        if times.ndim != 1:
            raise ValueError(
                f"{self.name}: times must be 1-D (got shape {times.shape})."
            )
        if times.size < 1:
            raise ValueError(f"{self.name}: times must be non-empty.")
        if values.shape[0] != times.shape[0]:
            raise ValueError(
                f"{self.name}: ragged series - values leading dim "
                f"{values.shape[0]} != len(times) {times.shape[0]}."
            )
        if times.size >= 2 and np.any(np.diff(times) <= 0.0):
            raise ValueError(
                f"{self.name}: times must be strictly increasing (sorted)."
            )
        if not np.all(np.isfinite(times)):
            raise ValueError(f"{self.name}: times contain non-finite values.")
        if self.locations is not None:
            loc = np.asarray(self.locations, dtype=np.float64)
            object.__setattr__(self, "locations", loc)
            if loc.ndim != 2 or loc.shape[1] != 3:
                raise ValueError(
                    f"{self.name}: locations must be (n_loc, 3); got {loc.shape}."
                )
            if values.ndim == 2 and loc.shape[0] != values.shape[1]:
                raise ValueError(
                    f"{self.name}: locations rows {loc.shape[0]} != value "
                    f"columns {values.shape[1]}."
                )

    @property
    def n_samples(self) -> int:
        """Number of time samples."""
        return int(self.times.shape[0])

    @property
    def is_multichannel(self) -> bool:
        """True when the series has a spatial (location) axis."""
        return self.values.ndim == 2

    @property
    def peak(self) -> float:
        """Maximum value over all samples (and locations)."""
        return float(np.max(self.values))

    @property
    def trough(self) -> float:
        """Minimum value over all samples (and locations)."""
        return float(np.min(self.values))

    @property
    def mean(self) -> float:
        """Mean value over all samples (and locations)."""
        return float(np.mean(self.values))

    @property
    def spatial_max(self) -> NDArray[np.float64]:
        """Per-timestep maximum across locations (multichannel only)."""
        self._require_multichannel("spatial_max")
        return np.max(self.values, axis=1)

    @property
    def spatial_min(self) -> NDArray[np.float64]:
        """Per-timestep minimum across locations (multichannel only)."""
        self._require_multichannel("spatial_min")
        return np.min(self.values, axis=1)

    @property
    def spatial_mean(self) -> NDArray[np.float64]:
        """Per-timestep mean across locations (multichannel only)."""
        self._require_multichannel("spatial_mean")
        return np.mean(self.values, axis=1)

    def _require_multichannel(self, what: str) -> None:
        if not self.is_multichannel:
            raise ValueError(
                f"{self.name}: {what} requires a multichannel (2-D) series."
            )

    def to_provenance(self) -> Dict[str, object]:
        """Scalar provenance record (drops arrays) for logging/serialisation."""
        return {
            "name": self.name,
            "units": self.units,
            "case_id": self.case_id,
            "source_field": self.source_field,
            "provenance": self.provenance,
            "n_samples": self.n_samples,
            "multichannel": self.is_multichannel,
        }


# ============================================================================
# ValidationFlags
# ============================================================================


@dataclass(frozen=True)
class ValidationFlags:
    """Boolean/scalar validation summary for an extracted bundle.

    Attributes:
        synchronized_time: True when every series shares the common time base.
        mass_balance_residual: Relative mass-balance residual
            ``max|m - m_expected| / max|m|`` (dimensionless).
        mass_balance_ok: True when ``mass_balance_residual`` is within tolerance.
        dry_out: True if the fill fraction fell below the dry-out threshold.
        overflow: True if the fill fraction exceeded the overflow threshold.
        impact: True if any wall-pressure sample exceeded the impact threshold.
        mass_balance_rtol: The tolerance used for ``mass_balance_ok``.
        dry_out_fill_fraction: The dry-out fill-fraction threshold used.
        overflow_fill_fraction: The overflow fill-fraction threshold used.
        impact_pressure: The impact pressure threshold used (Pa).
    """

    synchronized_time: bool
    mass_balance_residual: float
    mass_balance_ok: bool
    dry_out: bool
    overflow: bool
    impact: bool
    mass_balance_rtol: float = 1.0e-3
    dry_out_fill_fraction: float = 0.02
    overflow_fill_fraction: float = 0.98
    impact_pressure: float = float("inf")

    def to_dict(self) -> Dict[str, object]:
        """Plain-dict view for logging / serialisation."""
        return {
            "synchronized_time": self.synchronized_time,
            "mass_balance_residual": self.mass_balance_residual,
            "mass_balance_ok": self.mass_balance_ok,
            "dry_out": self.dry_out,
            "overflow": self.overflow,
            "impact": self.impact,
            "mass_balance_rtol": self.mass_balance_rtol,
            "dry_out_fill_fraction": self.dry_out_fill_fraction,
            "overflow_fill_fraction": self.overflow_fill_fraction,
            "impact_pressure": self.impact_pressure,
        }


# ============================================================================
# SynchronizedTimeHistory bundle
# ============================================================================


@dataclass(frozen=True, eq=False)
class SynchronizedTimeHistory:
    """A bundle of provenance-stamped series sharing one common time base.

    Attributes:
        case_id: Originating case identifier.
        time: The common time base (s) every channel is aligned to.
        channels: Mapping ``name -> TimeSeriesChannel``.
        validation: The validation-flag summary.
        metadata: Free-form provenance metadata (density, tank volume, source).
    """

    case_id: str
    time: NDArray[np.float64]
    channels: Mapping[str, TimeSeriesChannel]
    validation: ValidationFlags
    metadata: Mapping[str, object] = field(default_factory=dict)

    @classmethod
    def from_channels(
        cls,
        *,
        case_id: str,
        channels: Mapping[str, TimeSeriesChannel],
        validation: ValidationFlags,
        metadata: Optional[Mapping[str, object]] = None,
    ) -> "SynchronizedTimeHistory":
        """Assemble a bundle, enforcing a single shared time base.

        Args:
            case_id: Case identifier.
            channels: The series to bundle (must be non-empty).
            validation: Validation-flag summary.
            metadata: Optional provenance metadata.

        Returns:
            The synchronized bundle.

        Raises:
            ValueError: If ``channels`` is empty or any channel's ``times`` do
                not match the common base (mismatched / ragged synchronization).
        """
        if not channels:
            raise ValueError("from_channels requires at least one channel.")
        first = next(iter(channels.values()))
        base = validate_synchronized_time(first.times)
        for name, ch in channels.items():
            if ch.times.shape != base.shape or not np.array_equal(ch.times, base):
                raise ValueError(
                    f"channel {name!r} is not synchronized to the common time "
                    f"base (time base mismatch)."
                )
        return cls(
            case_id=case_id,
            time=base,
            channels=dict(channels),
            validation=validation,
            metadata=dict(metadata or {}),
        )

    def pressure_vs_elevation(
        self, time_index: int, channel: str = "wall_pressure"
    ) -> Tuple[NDArray[np.float64], NDArray[np.float64]]:
        """Return ``(elevations, pressures)`` at one instant, sorted by z.

        Args:
            time_index: Index into the common time base.
            channel: Multichannel pressure channel name.

        Returns:
            Tuple of the probe elevations (m, ascending) and the aligned
            pressures (Pa) at ``time_index``.

        Raises:
            ValueError: If the channel is missing, not multichannel, or has no
                locations.
        """
        ch = self.channels.get(channel)
        if ch is None or not ch.is_multichannel or ch.locations is None:
            raise ValueError(
                f"pressure_vs_elevation needs a multichannel channel with "
                f"locations; {channel!r} is unavailable."
            )
        z = ch.locations[:, 2]
        order = np.argsort(z)
        return z[order], ch.values[time_index, order]

    def pressure_tap_statistics(
        self, channel: str = "wall_pressure", **kwargs: object
    ) -> Dict[str, PressureTapStatistics]:
        """Per-probe pressure statistics, reusing ``pressure_taps``.

        Wraps the multichannel pressure channel in a
        :class:`~digitalmodel.solvers.openfoam.results_models.ProbeTimeSeries`
        and delegates to
        :func:`~digitalmodel.solvers.openfoam.pressure_taps.compute_tap_statistics`.

        Args:
            channel: Multichannel pressure channel name.
            **kwargs: Forwarded to ``compute_tap_statistics``.

        Returns:
            Mapping ``probe_name -> PressureTapStatistics``.
        """
        ch = self.channels.get(channel)
        if ch is None or not ch.is_multichannel:
            raise ValueError(f"{channel!r} is not a multichannel pressure channel.")
        coords = (
            ch.locations.tolist()
            if ch.locations is not None
            else [[0.0, 0.0, 0.0]] * ch.values.shape[1]
        )
        series = ProbeTimeSeries(
            times=ch.times,
            probe_coords=coords,
            values=ch.values,
            field_name=ch.source_field,
        )
        names = list(ch.location_labels) if ch.location_labels else None
        return compute_tap_statistics(series, names, **kwargs)  # type: ignore[arg-type]

    def phase_lag_deg(
        self, response_channel: str, reference_channel: str = "roll_angle"
    ) -> float:
        """Phase lag (deg) of a response channel behind a reference channel.

        Both channels must be scalar (1-D). See :func:`phase_lag_deg`.
        """
        ref = self.channels[reference_channel]
        resp = self.channels[response_channel]
        if ref.is_multichannel or resp.is_multichannel:
            raise ValueError("phase_lag_deg needs scalar (1-D) channels.")
        return phase_lag_deg(ref.values, resp.values, times=self.time)


# ============================================================================
# Phase lag helper (reuses spectral_analysis for the dominant frequency)
# ============================================================================


def phase_lag_deg(
    reference: NDArray[np.float64],
    response: NDArray[np.float64],
    *,
    times: Optional[NDArray[np.float64]] = None,
    sample_rate: Optional[float] = None,
    frequency: Optional[float] = None,
) -> float:
    """Phase lag (deg) of ``response`` behind ``reference`` at their tone.

    The dominant frequency of ``reference`` is found via
    :func:`~digitalmodel.solvers.openfoam.spectral_analysis.extract_natural_frequency`
    (unless ``frequency`` is given), and the phase difference of the complex
    FFT bins nearest that frequency is returned wrapped to ``(-180, 180]``. A
    positive value means the response lags the reference.

    Args:
        reference: Reference signal (e.g. imposed roll).
        response: Response signal (e.g. elevation, force, CoM).
        times: Uniform time vector (s) used to infer ``sample_rate``.
        sample_rate: Explicit sampling frequency (Hz); overrides ``times``.
        frequency: Optional explicit frequency (Hz) to evaluate the phase at.

    Returns:
        Phase lag in degrees, positive when ``response`` lags ``reference``.

    Raises:
        ValueError: If neither ``times`` nor ``sample_rate`` is given, or the
            signals differ in length.
    """
    ref = np.asarray(reference, dtype=np.float64).ravel()
    resp = np.asarray(response, dtype=np.float64).ravel()
    if ref.size != resp.size:
        raise ValueError("reference and response must have equal length.")
    if sample_rate is None:
        if times is None:
            raise ValueError("Provide either 'times' or 'sample_rate'.")
        t = validate_synchronized_time(times)
        sample_rate = float(1.0 / np.mean(np.diff(t)))

    if frequency is None:
        spec = extract_natural_frequency(
            ref, sample_rate=sample_rate, method="fft", min_frequency=0.0
        )
        frequency = spec.dominant_frequency

    n = ref.size
    freqs = np.fft.rfftfreq(n, d=1.0 / sample_rate)
    k = int(np.argmin(np.abs(freqs - frequency)))

    ref_c = np.fft.rfft(ref - np.mean(ref))
    resp_c = np.fft.rfft(resp - np.mean(resp))
    lag_rad = np.angle(ref_c[k]) - np.angle(resp_c[k])
    # wrap to (-pi, pi]
    lag_rad = (lag_rad + np.pi) % (2.0 * np.pi) - np.pi
    return float(np.rad2deg(lag_rad))


# ============================================================================
# Raw inputs + extraction config
# ============================================================================


@dataclass(frozen=True, eq=False)
class RawCFDOutputs:
    """Already-parsed CFD outputs on a common time base for extraction.

    All time-varying arrays share ``time`` as their leading axis; the extractor
    never re-times them, so this container is where the raw synchronization is
    validated (ragged / unsorted inputs are rejected here).

    Attributes:
        case_id: Case identifier stamped onto every extracted channel.
        time: Common time base (s).
        elevation: Free-surface elevation above nominal ``(n_t, n_pos)`` (m).
        elevation_positions: Elevation-probe coordinates ``(n_pos, 3)`` (m).
        pressure: Wall pressure ``(n_t, n_p)`` (Pa).
        pressure_locations: Pressure-probe coordinates ``(n_p, 3)`` (m).
        liquid_volume: Instantaneous liquid volume ``(n_t,)`` (m^3).
        com: Liquid centre of mass ``(n_t, 3)`` (m).
        inlet_flow_rate: Inlet volumetric flow rate ``(n_t,)`` (m^3/s).
        outlet_flow_rate: Outlet volumetric flow rate ``(n_t,)`` (m^3/s).
        roll_angle: Imposed roll angle ``(n_t,)`` (rad).
        density: Liquid density (kg/m^3).
        tank_volume: Total tank volume (m^3) for the fill fraction.
        roll_rate: Optional angular velocity ``(n_t,)`` (rad/s); derived by
            gradient of ``roll_angle`` when omitted.
        source: Provenance base string (e.g. solver / function-object family).
    """

    case_id: str
    time: NDArray[np.float64]
    elevation: NDArray[np.float64]
    elevation_positions: NDArray[np.float64]
    pressure: NDArray[np.float64]
    pressure_locations: NDArray[np.float64]
    liquid_volume: NDArray[np.float64]
    com: NDArray[np.float64]
    inlet_flow_rate: NDArray[np.float64]
    outlet_flow_rate: NDArray[np.float64]
    roll_angle: NDArray[np.float64]
    density: float = 1025.0
    tank_volume: float = 0.0
    roll_rate: Optional[NDArray[np.float64]] = None
    source: str = "interFoam"

    def __post_init__(self) -> None:
        if not self.case_id or not self.case_id.strip():
            raise ValueError("case_id must be a non-empty string.")
        t = validate_synchronized_time(self.time)
        object.__setattr__(self, "time", t)
        n = t.shape[0]

        def _arr(name: str, expect_cols: Optional[int] = None) -> NDArray[np.float64]:
            a = np.asarray(getattr(self, name), dtype=np.float64)
            if a.shape[0] != n:
                raise ValueError(
                    f"{name}: ragged input - leading dim {a.shape[0]} != "
                    f"len(time) {n}; all series must share the time base."
                )
            if expect_cols is not None and (a.ndim != 2 or a.shape[1] != expect_cols):
                raise ValueError(
                    f"{name}: expected shape (n_t, {expect_cols}); got {a.shape}."
                )
            object.__setattr__(self, name, a)
            return a

        _arr("elevation")
        _arr("pressure")
        _arr("liquid_volume")
        _arr("com", expect_cols=3)
        _arr("inlet_flow_rate")
        _arr("outlet_flow_rate")
        _arr("roll_angle")
        if self.roll_rate is not None:
            _arr("roll_rate")

        pos = np.asarray(self.elevation_positions, dtype=np.float64)
        if pos.ndim != 2 or pos.shape[1] != 3:
            raise ValueError("elevation_positions must be shape (n_pos, 3).")
        object.__setattr__(self, "elevation_positions", pos)
        if self.elevation.ndim == 2 and self.elevation.shape[1] != pos.shape[0]:
            raise ValueError("elevation columns must match elevation_positions rows.")

        plocs = np.asarray(self.pressure_locations, dtype=np.float64)
        if plocs.ndim != 2 or plocs.shape[1] != 3:
            raise ValueError("pressure_locations must be shape (n_p, 3).")
        object.__setattr__(self, "pressure_locations", plocs)
        if self.pressure.ndim == 2 and self.pressure.shape[1] != plocs.shape[0]:
            raise ValueError("pressure columns must match pressure_locations rows.")

        if self.density <= 0.0:
            raise ValueError("density must be positive.")
        if self.tank_volume <= 0.0:
            raise ValueError("tank_volume must be positive.")


@dataclass(frozen=True)
class ExtractionConfig:
    """Thresholds for the extraction validation flags.

    Attributes:
        mass_balance_rtol: Relative tolerance for the mass-balance residual.
        dry_out_fill_fraction: Fill fraction below which dry-out is flagged.
        overflow_fill_fraction: Fill fraction above which overflow is flagged.
        impact_pressure: Wall pressure (Pa) above which an impact is flagged
            (default ``inf`` = disabled unless configured).
    """

    mass_balance_rtol: float = 1.0e-3
    dry_out_fill_fraction: float = 0.02
    overflow_fill_fraction: float = 0.98
    impact_pressure: float = float("inf")


# ============================================================================
# Extraction
# ============================================================================


def _cumulative_trapezoid(
    y: NDArray[np.float64], x: NDArray[np.float64]
) -> NDArray[np.float64]:
    """Cumulative trapezoidal integral of ``y`` over ``x``, starting at 0."""
    dt = np.diff(x)
    increments = 0.5 * (y[1:] + y[:-1]) * dt
    return np.concatenate([[0.0], np.cumsum(increments)])


def extract_time_history(
    raw: RawCFDOutputs, *, config: Optional[ExtractionConfig] = None
) -> SynchronizedTimeHistory:
    """Extract a synchronized, provenance-stamped time-history bundle.

    Builds every scope channel on ``raw.time`` (so the bundle is synchronized by
    construction), computes the derived spatial/temporal/integral series, and
    evaluates the validation flags (mass-balance residual vs the flow integral,
    dry-out / overflow on the fill fraction, and wall-pressure impact).

    Args:
        raw: Parsed CFD outputs on a common time base.
        config: Validation thresholds (defaults used when omitted).

    Returns:
        The :class:`SynchronizedTimeHistory` bundle.
    """
    cfg = config or ExtractionConfig()
    t = raw.time
    cid = raw.case_id
    src = raw.source

    def ch(
        name: str,
        values: NDArray[np.float64],
        units: str,
        source_field: str,
        provenance: str,
        *,
        locations: Optional[NDArray[np.float64]] = None,
        location_labels: Optional[Tuple[str, ...]] = None,
    ) -> TimeSeriesChannel:
        return TimeSeriesChannel(
            name=name,
            times=t,
            values=values,
            units=units,
            case_id=cid,
            source_field=source_field,
            provenance=f"[{src}] {provenance}",
            locations=locations,
            location_labels=location_labels,
        )

    channels: Dict[str, TimeSeriesChannel] = {}

    # --- free-surface elevation (transect + spatial reductions) ---
    elev_labels = tuple(f"eta_probe_{i}" for i in range(raw.elevation.shape[1]))
    channels["free_surface_elevation"] = ch(
        "free_surface_elevation",
        raw.elevation,
        "m",
        "alpha.water",
        "interface elevation above nominal at probe transect",
        locations=raw.elevation_positions,
        location_labels=elev_labels,
    )
    channels["elevation_max"] = ch(
        "elevation_max",
        np.max(raw.elevation, axis=1),
        "m",
        "alpha.water",
        "spatial max of free-surface elevation across probes",
    )
    channels["elevation_min"] = ch(
        "elevation_min",
        np.min(raw.elevation, axis=1),
        "m",
        "alpha.water",
        "spatial min of free-surface elevation across probes",
    )
    channels["elevation_mean"] = ch(
        "elevation_mean",
        np.mean(raw.elevation, axis=1),
        "m",
        "alpha.water",
        "spatial mean of free-surface elevation across probes",
    )

    # --- wall pressure (vs elevation and time) ---
    p_labels = tuple(
        f"p_z{loc[2]:.3g}" for loc in raw.pressure_locations
    )
    channels["wall_pressure"] = ch(
        "wall_pressure",
        raw.pressure,
        "Pa",
        "p",
        "wall pressure at lower/middle/upper elevation probes",
        locations=raw.pressure_locations,
        location_labels=p_labels,
    )

    # --- liquid volume / mass / fill fraction ---
    channels["liquid_volume"] = ch(
        "liquid_volume",
        raw.liquid_volume,
        "m^3",
        "alpha.water",
        "instantaneous liquid volume (alpha integral over cells)",
    )
    liquid_mass = raw.density * raw.liquid_volume
    channels["liquid_mass"] = ch(
        "liquid_mass",
        liquid_mass,
        "kg",
        "alpha.water",
        f"total liquid mass = density({raw.density}) * liquid volume",
    )
    fill_fraction = raw.liquid_volume / raw.tank_volume
    channels["fill_fraction"] = ch(
        "fill_fraction",
        fill_fraction,
        "1",
        "alpha.water",
        f"fill fraction = liquid volume / tank volume ({raw.tank_volume} m^3)",
    )

    # --- inlet / outlet flow + integrated volumes ---
    channels["inlet_flow_rate"] = ch(
        "inlet_flow_rate",
        raw.inlet_flow_rate,
        "m^3/s",
        "phi",
        "inlet volumetric flow rate (patch flux)",
    )
    channels["outlet_flow_rate"] = ch(
        "outlet_flow_rate",
        raw.outlet_flow_rate,
        "m^3/s",
        "phi",
        "outlet volumetric flow rate (patch flux)",
    )
    inlet_volume = _cumulative_trapezoid(raw.inlet_flow_rate, t)
    outlet_volume = _cumulative_trapezoid(raw.outlet_flow_rate, t)
    channels["inlet_volume"] = ch(
        "inlet_volume",
        inlet_volume,
        "m^3",
        "phi",
        "integrated inlet volume (cumulative trapezoid of inlet flow)",
    )
    channels["outlet_volume"] = ch(
        "outlet_volume",
        outlet_volume,
        "m^3",
        "phi",
        "integrated outlet volume (cumulative trapezoid of outlet flow)",
    )

    # --- mass-balance residual vs the flow integral ---
    net_volume_in = inlet_volume - outlet_volume
    expected_mass = liquid_mass[0] + raw.density * net_volume_in
    residual_series = liquid_mass - expected_mass
    mass_ref = float(np.max(np.abs(liquid_mass)))
    rel_residual = (
        float(np.max(np.abs(residual_series)) / mass_ref) if mass_ref > 0 else 0.0
    )
    channels["mass_balance_residual"] = ch(
        "mass_balance_residual",
        residual_series,
        "kg",
        "alpha.water",
        "mass - (mass[0] + density * integral(Qin - Qout))",
    )

    # --- centre of mass ---
    for axis, key in enumerate(("com_x", "com_y", "com_z")):
        channels[key] = ch(
            key,
            raw.com[:, axis],
            "m",
            "alpha.water",
            f"liquid centre-of-mass {key[-1]}-coordinate (alpha-weighted)",
        )

    # --- roll angle / rate / phase ---
    channels["roll_angle"] = ch(
        "roll_angle",
        raw.roll_angle,
        "rad",
        "sixDoFRigidBodyMotion",
        "imposed roll angle",
    )
    if raw.roll_rate is not None:
        roll_rate = raw.roll_rate
        rate_prov = "imposed roll angular velocity"
    else:
        roll_rate = np.gradient(raw.roll_angle, t)
        rate_prov = "roll angular velocity (gradient of roll angle)"
    channels["roll_rate"] = ch(
        "roll_rate", roll_rate, "rad/s", "sixDoFRigidBodyMotion", rate_prov
    )
    roll_phase = np.unwrap(np.angle(hilbert(raw.roll_angle - np.mean(raw.roll_angle))))
    channels["roll_phase"] = ch(
        "roll_phase",
        roll_phase,
        "rad",
        "sixDoFRigidBodyMotion",
        "imposed-motion instantaneous phase (Hilbert analytic signal)",
    )

    # --- validation flags ---
    dry_out = bool(np.any(fill_fraction < cfg.dry_out_fill_fraction))
    overflow = bool(np.any(fill_fraction > cfg.overflow_fill_fraction))
    impact = bool(np.any(raw.pressure > cfg.impact_pressure))
    mass_ok = rel_residual <= cfg.mass_balance_rtol

    if not mass_ok:
        logger.warning(
            "case {} mass-balance residual {:.3e} exceeds rtol {:.3e}",
            cid,
            rel_residual,
            cfg.mass_balance_rtol,
        )

    validation = ValidationFlags(
        synchronized_time=True,
        mass_balance_residual=rel_residual,
        mass_balance_ok=mass_ok,
        dry_out=dry_out,
        overflow=overflow,
        impact=impact,
        mass_balance_rtol=cfg.mass_balance_rtol,
        dry_out_fill_fraction=cfg.dry_out_fill_fraction,
        overflow_fill_fraction=cfg.overflow_fill_fraction,
        impact_pressure=cfg.impact_pressure,
    )

    metadata: Dict[str, object] = {
        "density_kg_m3": raw.density,
        "tank_volume_m3": raw.tank_volume,
        "source": src,
        "n_elevation_probes": int(raw.elevation.shape[1]),
        "n_pressure_probes": int(raw.pressure.shape[1]),
    }

    return SynchronizedTimeHistory.from_channels(
        case_id=cid,
        channels=channels,
        validation=validation,
        metadata=metadata,
    )
