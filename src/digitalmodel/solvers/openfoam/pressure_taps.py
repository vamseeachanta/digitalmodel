#!/usr/bin/env python3
"""
ABOUTME: Named wall pressure taps for OpenFOAM sloshing/impact cases (dm#661,
ACMA B1546 ballast-tank sloshing CFD). Given a list of named taps - a point
location, a named wall patch/surface, or a point snapped onto a named patch -
this module renders the corresponding OpenFOAM ``functions{}`` entries
(``probes`` for point pressure, ``patchProbes`` for a point on a wall, and
``surfaceFieldValue`` for a whole wall patch), writing the pressure field ``p``
(and ``p_rgh`` when relevant) time histories. It also provides post-processing
that reads the tap output back and computes, per named tap: peak, envelope
(min/max), the full time-history handle, spectral content (via
``spectral_analysis.compute_fft_spectrum``) and a design-equivalent pressure.

This generalises the proven Kleefsman impact-sensor pattern
(``validation/kleefsman.py``) so callers can name their own taps. The B1546
interest points supported by name are: the tank top near the centreline, the
longitudinal floors, and the side-shell longitudinals - see
``b1546_default_taps``.

Design-equivalent pressure
--------------------------
The *design-equivalent pressure* is defined here as a high-percentile value of
the tap pressure time history (default: the 99th percentile). Sloshing impact
traces are spiky and a single-sample numerical over/under-shoot is not a sound
design basis; a high percentile is a robust near-peak statistic that captures
the sustained impact maxima while discarding isolated one-sample spikes. The
percentile is configurable, and the raw ``peak`` (true maximum) is always
reported alongside it.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

import numpy as np
from numpy.typing import NDArray

from .post_processing import OpenFOAMPostProcessor
from .results_models import ProbeTimeSeries
from .spectral_analysis import compute_fft_spectrum

# Fields written for a multiphase (interFoam) sloshing tap: dynamic pressure p
# and the buoyant/hydrostatic-reduced pressure p_rgh.
DEFAULT_MULTIPHASE_FIELDS: Tuple[str, ...] = ("p", "p_rgh")


def _fmt(value: float) -> str:
    """Format a float for an OpenFOAM dict (6 significant figures)."""
    return "{:.6g}".format(value)


# ============================================================================
# PressureTap
# ============================================================================


@dataclass
class PressureTap:
    """A single named wall pressure tap.

    A tap is one of three kinds, inferred from which of ``location`` / ``patch``
    are set:

    - point tap (``location`` only) -> emitted as a ``probes`` entry that
      interpolates the field to the point;
    - patch-point tap (``location`` **and** ``patch``) -> emitted as a
      ``patchProbes`` entry that snaps the point onto the named wall patch (so
      the sample sits exactly on the boundary face rather than half a cell off);
    - surface tap (``patch`` only) -> emitted as a ``surfaceFieldValue`` entry
      that reduces the field over the whole named wall patch.

    Attributes:
        name: Unique, human-readable tap name (e.g. ``"tank_top_centreline"``).
        location: ``(x, y, z)`` sample point in metres, or ``None`` for a
            whole-patch surface tap.
        patch: Named wall patch/surface, or ``None`` for a free point tap.
        fields: Fields to sample (default ``("p",)``; use ``("p", "p_rgh")``
            for a multiphase VOF case).
        operation: Reduction for a surface tap (``surfaceFieldValue``
            ``operation``), e.g. ``"areaAverage"`` or ``"max"``. Ignored for
            point / patch-point taps.
    """

    name: str
    location: Optional[Tuple[float, float, float]] = None
    patch: Optional[str] = None
    fields: Tuple[str, ...] = ("p",)
    operation: str = "areaAverage"

    def __post_init__(self) -> None:
        if not self.name or not self.name.strip():
            raise ValueError("PressureTap.name must be a non-empty string.")
        if self.location is None and self.patch is None:
            raise ValueError(
                f"Tap {self.name!r}: provide a location, a patch, or both."
            )
        if self.location is not None and len(tuple(self.location)) != 3:
            raise ValueError(
                f"Tap {self.name!r}: location must be an (x, y, z) triple."
            )
        if not self.fields:
            raise ValueError(f"Tap {self.name!r}: at least one field required.")

    @property
    def kind(self) -> str:
        """One of ``'point'``, ``'patch_point'`` or ``'surface'``."""
        if self.patch is None:
            return "point"
        if self.location is None:
            return "surface"
        return "patch_point"


# ============================================================================
# Function-object rendering
# ============================================================================


def _union_fields(taps: Sequence[PressureTap]) -> Tuple[str, ...]:
    """Ordered union of the fields requested across ``taps`` (p first)."""
    ordered: List[str] = []
    for tap in taps:
        for fld in tap.fields:
            if fld not in ordered:
                ordered.append(fld)
    # Keep 'p' leading for readability if present.
    if "p" in ordered:
        ordered = ["p"] + [f for f in ordered if f != "p"]
    return tuple(ordered)


def render_probes_entry(
    taps: Sequence[PressureTap],
    *,
    object_name: str = "pressureTaps",
    write_control: str = "timeStep",
    write_interval: int = 1,
) -> str:
    """Render a single ``probes`` function object for point taps.

    Columns in the output file follow ``taps`` order; the tap names are written
    as a comment so the mapping back to named taps is auditable.

    Args:
        taps: Point taps (``location`` set, ``patch`` unset).
        object_name: Function-object key in ``functions{}``.
        write_control: OpenFOAM ``writeControl`` (default ``"timeStep"``).
        write_interval: ``writeInterval`` in the chosen control units.

    Returns:
        The dictionary block text (indented for embedding in ``functions{}``).
    """
    if not taps:
        raise ValueError("render_probes_entry requires at least one tap.")
    fields = " ".join(_union_fields(taps))
    loc_lines = []
    for tap in taps:
        assert tap.location is not None  # guaranteed by caller
        x, y, z = tap.location
        loc_lines.append(
            f"            ({_fmt(x)} {_fmt(y)} {_fmt(z)})   // {tap.name}"
        )
    locs = "\n".join(loc_lines)
    names = ", ".join(t.name for t in taps)
    return f"""\
    {object_name}
    {{
        type            probes;
        libs            (sampling);
        writeControl    {write_control};
        writeInterval   {write_interval};
        fields          ({fields});
        // named taps (column order): {names}
        probeLocations
        (
{locs}
        );
    }}
"""


def render_patch_probes_entry(
    taps: Sequence[PressureTap],
    patch: str,
    *,
    object_name: Optional[str] = None,
    write_control: str = "timeStep",
    write_interval: int = 1,
) -> str:
    """Render a ``patchProbes`` function object for point-on-wall taps.

    ``patchProbes`` snaps each point to the nearest face of ``patch`` so the
    sample sits exactly on the wall - the correct choice for a wall pressure
    tap on a named boundary.

    Args:
        taps: Patch-point taps that all reference ``patch``.
        patch: The wall patch the points are snapped onto.
        object_name: Function-object key (default ``"pressureTaps_<patch>"``).
        write_control: OpenFOAM ``writeControl``.
        write_interval: ``writeInterval``.
    """
    if not taps:
        raise ValueError("render_patch_probes_entry requires at least one tap.")
    object_name = object_name or f"pressureTaps_{patch}"
    fields = " ".join(_union_fields(taps))
    loc_lines = []
    for tap in taps:
        assert tap.location is not None
        x, y, z = tap.location
        loc_lines.append(
            f"            ({_fmt(x)} {_fmt(y)} {_fmt(z)})   // {tap.name}"
        )
    locs = "\n".join(loc_lines)
    names = ", ".join(t.name for t in taps)
    return f"""\
    {object_name}
    {{
        type            patchProbes;
        libs            (sampling);
        patch           {patch};
        writeControl    {write_control};
        writeInterval   {write_interval};
        fields          ({fields});
        // named taps (column order): {names}
        probeLocations
        (
{locs}
        );
    }}
"""


def render_surface_entry(
    tap: PressureTap,
    *,
    write_control: str = "timeStep",
    write_interval: int = 1,
) -> str:
    """Render a ``surfaceFieldValue`` function object for a whole-patch tap.

    Reduces ``tap.fields`` over the named wall patch with ``tap.operation``
    (e.g. area-average or max pressure across the patch).
    """
    if tap.kind != "surface":
        raise ValueError(
            f"render_surface_entry expects a surface tap; {tap.name!r} is "
            f"a {tap.kind} tap."
        )
    fields = " ".join(tap.fields)
    return f"""\
    {tap.name}
    {{
        type            surfaceFieldValue;
        libs            (fieldFunctionObjects);
        regionType      patch;
        name            {tap.patch};
        operation       {tap.operation};
        fields          ({fields});
        writeControl    {write_control};
        writeInterval   {write_interval};
        writeFields     false;
        log             false;
    }}
"""


def render_pressure_tap_functions(
    taps: Sequence[PressureTap],
    *,
    write_control: str = "timeStep",
    write_interval: int = 1,
) -> str:
    """Render the full ``functions{}`` block for a list of named taps.

    Point taps are grouped into one ``probes`` object; patch-point taps are
    grouped into one ``patchProbes`` object per patch; each surface tap becomes
    its own ``surfaceFieldValue`` object. Returns ``""`` for an empty tap list
    so the caller can stay additive (no taps -> no output).

    Args:
        taps: The named taps.
        write_control: OpenFOAM ``writeControl`` for every emitted object.
        write_interval: ``writeInterval`` for every emitted object.

    Returns:
        A ``functions\\n{ ... }\\n`` block, or ``""`` when ``taps`` is empty.
    """
    taps = list(taps)
    if not taps:
        return ""

    _validate_unique_names(taps)

    point_taps = [t for t in taps if t.kind == "point"]
    patch_point_taps = [t for t in taps if t.kind == "patch_point"]
    surface_taps = [t for t in taps if t.kind == "surface"]

    entries: List[str] = []
    if point_taps:
        entries.append(
            render_probes_entry(
                point_taps,
                write_control=write_control,
                write_interval=write_interval,
            )
        )

    # Group patch-point taps by their patch (deterministic order of first use).
    by_patch: Dict[str, List[PressureTap]] = {}
    for tap in patch_point_taps:
        by_patch.setdefault(tap.patch, []).append(tap)  # type: ignore[arg-type]
    for patch, group in by_patch.items():
        entries.append(
            render_patch_probes_entry(
                group,
                patch,
                write_control=write_control,
                write_interval=write_interval,
            )
        )

    for tap in surface_taps:
        entries.append(
            render_surface_entry(
                tap,
                write_control=write_control,
                write_interval=write_interval,
            )
        )

    body = "".join(entries)
    return f"functions\n{{\n    // Named wall pressure taps (dm#661)\n{body}}}\n"


def _validate_unique_names(taps: Sequence[PressureTap]) -> None:
    seen: set[str] = set()
    for tap in taps:
        if tap.name in seen:
            raise ValueError(f"Duplicate pressure-tap name: {tap.name!r}")
        seen.add(tap.name)


def point_tap_names(taps: Sequence[PressureTap]) -> List[str]:
    """Names of the point taps in the order their probe columns are written.

    Use this to align a parsed ``probes`` output file (columns follow this
    order) with the tap names when computing statistics.
    """
    return [t.name for t in taps if t.kind == "point"]


# ============================================================================
# B1546 default taps
# ============================================================================


def b1546_default_taps(
    length: float,
    width: float,
    height: float,
    *,
    fields: Tuple[str, ...] = DEFAULT_MULTIPHASE_FIELDS,
    n_floor: int = 3,
    n_side: int = 3,
    inset: float = 0.02,
) -> List[PressureTap]:
    """Named B1546 ballast-tank pressure taps for a box tank of ``L x W x H``.

    Produces the three families of interest called out in dm#661:

    - **tank top near centreline** - one tap just below the tank top at
      mid-length / mid-width (``tank_top_centreline``);
    - **longitudinal floors** - ``n_floor`` taps along the tank bottom at the
      spanwise centreline (``floor_long_1..n``), where slosh run-up loads the
      floor;
    - **side-shell longitudinals** - ``n_side`` taps up each side shell at
      mid-length (``side_shell_port_1..n`` / ``side_shell_stbd_1..n``), where
      the travelling bore impacts the side.

    Coordinates assume a tank occupying ``[0, L] x [0, W] x [0, H]`` (x =
    length, y = width/beam, z = height). Points are inset by ``inset`` metres
    from the walls so free point probes sample a fluid cell; for probes that
    must sit exactly on a wall, attach a ``patch`` and they become
    ``patchProbes`` instead.

    Args:
        length: Tank length L (m, x-direction).
        width: Tank width/beam W (m, y-direction).
        height: Tank height H (m, z-direction).
        fields: Fields to sample at each tap.
        n_floor: Number of longitudinal-floor taps.
        n_side: Number of side-shell taps up each side.
        inset: Wall inset for the sample points (m).

    Returns:
        A list of named :class:`PressureTap`.
    """
    if length <= 0 or width <= 0 or height <= 0:
        raise ValueError("length, width and height must be positive.")
    if n_floor < 1 or n_side < 1:
        raise ValueError("n_floor and n_side must be >= 1.")

    taps: List[PressureTap] = []

    # Tank top near centreline (just below the top).
    taps.append(
        PressureTap(
            name="tank_top_centreline",
            location=(length / 2.0, width / 2.0, height - inset),
            fields=fields,
        )
    )

    # Longitudinal-floor taps along the bottom at the spanwise centreline.
    for i in range(n_floor):
        # Spread across the interior length (avoid the end walls).
        frac = (i + 1) / (n_floor + 1)
        x = length * frac
        taps.append(
            PressureTap(
                name=f"floor_long_{i + 1}",
                location=(x, width / 2.0, inset),
                fields=fields,
            )
        )

    # Side-shell longitudinals up each side at mid-length.
    for i in range(n_side):
        frac = (i + 1) / (n_side + 1)
        z = height * frac
        taps.append(
            PressureTap(
                name=f"side_shell_port_{i + 1}",
                location=(length / 2.0, inset, z),
                fields=fields,
            )
        )
        taps.append(
            PressureTap(
                name=f"side_shell_stbd_{i + 1}",
                location=(length / 2.0, width - inset, z),
                fields=fields,
            )
        )

    return taps


# ============================================================================
# Post-processing: per-tap statistics
# ============================================================================


@dataclass
class PressureTapStatistics:
    """Summary statistics for one named pressure tap.

    Attributes:
        name: Tap name.
        times: Time vector of the tap trace (s).
        pressure: The full pressure time history (the time-history handle).
        peak: True maximum pressure over the record.
        min: Minimum pressure (lower envelope).
        max: Maximum pressure (upper envelope) - equals ``peak``.
        mean: Time-mean pressure.
        design_equivalent: High-percentile design-equivalent pressure (see the
            module docstring; default is the 99th percentile).
        design_percentile: The percentile used for ``design_equivalent``.
        frequencies: FFT frequency bins (Hz).
        spectrum: One-sided amplitude spectrum aligned with ``frequencies``.
        dominant_frequency: Frequency of the largest spectral peak (Hz).
    """

    name: str
    times: NDArray[np.float64]
    pressure: NDArray[np.float64]
    peak: float
    min: float
    max: float
    mean: float
    design_equivalent: float
    design_percentile: float
    frequencies: NDArray[np.float64]
    spectrum: NDArray[np.float64]
    dominant_frequency: float

    @property
    def envelope(self) -> Tuple[float, float]:
        """The (min, max) pressure envelope."""
        return (self.min, self.max)

    def to_dict(self) -> Dict[str, float]:
        """Scalar summary (drops the arrays) for logging / provenance."""
        return {
            "name": self.name,  # type: ignore[dict-item]
            "peak": self.peak,
            "min": self.min,
            "max": self.max,
            "mean": self.mean,
            "design_equivalent": self.design_equivalent,
            "design_percentile": self.design_percentile,
            "dominant_frequency": self.dominant_frequency,
        }


def compute_tap_statistics(
    probe_series: ProbeTimeSeries,
    tap_names: Optional[Sequence[str]] = None,
    *,
    design_percentile: float = 99.0,
    min_frequency: float = 0.0,
    detrend: str = "constant",
) -> Dict[str, PressureTapStatistics]:
    """Compute per-tap statistics from a parsed pressure ``probes`` series.

    Columns of ``probe_series.values`` are taken in order; ``tap_names`` (if
    given) labels them - it must match the ordered point-tap names used to
    build the case (see :func:`point_tap_names`). Without ``tap_names``, taps
    are labelled ``tap_0``, ``tap_1``, ...

    For each tap the peak, envelope (min/max), mean, the full time-history
    handle, a design-equivalent pressure (``design_percentile``-th percentile),
    and the FFT amplitude spectrum (via
    :func:`spectral_analysis.compute_fft_spectrum`) with its dominant frequency
    are returned.

    Args:
        probe_series: Parsed probe series (from
            :meth:`OpenFOAMPostProcessor.parse_probe_file`).
        tap_names: Optional names aligned with the value columns.
        design_percentile: Percentile for the design-equivalent pressure.
        min_frequency: Ignore spectral peaks below this frequency (Hz) when
            picking the dominant frequency (excludes the DC band).
        detrend: Detrend mode passed to ``compute_fft_spectrum``.

    Returns:
        Ordered mapping ``name -> PressureTapStatistics``.

    Raises:
        ValueError: If ``tap_names`` length does not match the column count, or
            if the percentile is outside ``[0, 100]``.
    """
    if not 0.0 <= design_percentile <= 100.0:
        raise ValueError("design_percentile must be in [0, 100].")

    values = np.asarray(probe_series.values, dtype=np.float64)
    if values.ndim == 1:
        values = values.reshape(-1, 1)
    n_cols = values.shape[1]

    if tap_names is not None:
        tap_names = list(tap_names)
        if len(tap_names) != n_cols:
            raise ValueError(
                f"tap_names has {len(tap_names)} entries but the series has "
                f"{n_cols} columns."
            )
    else:
        tap_names = [f"tap_{i}" for i in range(n_cols)]

    times = np.asarray(probe_series.times, dtype=np.float64)
    sample_rate = _infer_sample_rate(times)

    results: Dict[str, PressureTapStatistics] = {}
    for col, name in enumerate(tap_names):
        signal = values[:, col]
        freqs, spectrum = compute_fft_spectrum(
            signal, sample_rate, detrend=detrend  # type: ignore[arg-type]
        )
        band = freqs >= min_frequency
        if np.any(band):
            dom = float(freqs[band][int(np.argmax(spectrum[band]))])
        else:
            dom = float("nan")

        results[name] = PressureTapStatistics(
            name=name,
            times=times,
            pressure=signal,
            peak=float(np.max(signal)),
            min=float(np.min(signal)),
            max=float(np.max(signal)),
            mean=float(np.mean(signal)),
            design_equivalent=float(np.percentile(signal, design_percentile)),
            design_percentile=design_percentile,
            frequencies=freqs,
            spectrum=spectrum,
            dominant_frequency=dom,
        )
    return results


def read_tap_statistics(
    probe_file: Path | str,
    tap_names: Optional[Sequence[str]] = None,
    *,
    field_name: str = "p",
    design_percentile: float = 99.0,
    min_frequency: float = 0.0,
) -> Dict[str, PressureTapStatistics]:
    """Read one probe output file and compute per-tap statistics.

    Convenience wrapper that parses ``probe_file`` with
    :meth:`OpenFOAMPostProcessor.parse_probe_file` and forwards to
    :func:`compute_tap_statistics`.
    """
    probe_file = Path(probe_file)
    pp = OpenFOAMPostProcessor(case_dir=probe_file.parent)
    series = pp.parse_probe_file(probe_file, field_name=field_name)
    return compute_tap_statistics(
        series,
        tap_names,
        design_percentile=design_percentile,
        min_frequency=min_frequency,
    )


def _infer_sample_rate(times: NDArray[np.float64]) -> float:
    """Sampling frequency (Hz) from a (near-)uniform time vector."""
    times = np.asarray(times, dtype=np.float64)
    if times.size < 2:
        raise ValueError("At least two time samples are required.")
    dt = np.diff(times)
    if np.any(dt <= 0):
        raise ValueError("Time vector must be strictly increasing.")
    return float(1.0 / np.mean(dt))
