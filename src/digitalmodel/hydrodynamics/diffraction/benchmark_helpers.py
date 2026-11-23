"""Shared constants and utility functions for benchmark comparison modules.

ABOUTME: Pure module-level constants and helper functions used by
benchmark_plotter.py and its extracted sub-modules.
Extracted from benchmark_plotter.py as part of WRK-592 God Object split.

No imports from other benchmark_* modules — this is the leaf dependency.
REFUSAL_QUALITIES is imported from multi_solver_comparator (not a benchmark_*
module, and it does not import this one) rather than copied, because a second
copy of that set is how a refusal quality silently stops reading as one.
"""
from __future__ import annotations

import html
from pathlib import Path
from typing import Any, Dict, List, Mapping, Optional, Tuple

from digitalmodel.hydrodynamics.diffraction.multi_solver_comparator import (
    REFUSAL_QUALITIES,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import DOF

DOF_ORDER = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]


def optional_float(value: Optional[float]) -> Optional[float]:
    """Return a JSON-safe float without fabricating a missing measurement."""
    return None if value is None else float(value)


def optional_round(value: Optional[float], digits: int) -> Optional[float]:
    """Round a present measurement and preserve absence as ``None``."""
    return None if value is None else round(float(value), digits)


# Two different facts wear the same word "unavailable" in a report:
#   - nothing was there to compare (a symmetric body has no off-diagonal
#     coupling, a response is below the null floor), and
#   - something was there and the comparison could not be made.
# Only the second is a refusal. Rendering them identically is what let 168 of
# 216 uncompared coefficient cells pass for ordinary blanks (#1633).
_ABSENCE_STRUCTURAL = (
    "absence-structural",
    "color:#7f8c8d;font-style:italic;",
    "Nothing to compare",
    "Not compared",
)
_ABSENCE_REFUSAL = (
    "absence-refusal",
    "color:#c0392b;font-weight:600;",
    "Comparison refused",
    "Unavailable",
)


def is_refusal_quality(quality: Optional[str]) -> bool:
    """Return True when an absent value means "could not compare".

    An unknown quality is treated as a refusal: an absence with no recorded
    provenance has not been shown to be benign.
    """
    return quality is None or quality in REFUSAL_QUALITIES


def format_absence(detail: Optional[str], refused: bool) -> str:
    """Render a missing value, distinguishing absence from refusal."""
    css_class, style, title, lead = (
        _ABSENCE_REFUSAL if refused else _ABSENCE_STRUCTURAL
    )
    suffix = f" ({detail})" if detail else ""
    return (
        f'<span class="{css_class}" style="{style}" title="{title}">'
        f"{lead}{suffix}</span>"
    )


def format_quality_distribution(counts: Mapping[str, int]) -> str:
    """Render a per-cell quality distribution for a group of cells.

    The group reads as a refusal when any single quality in it is one, so a
    handful of refused cells is never hidden behind a majority of structurally
    absent ones.
    """
    refused = any(is_refusal_quality(quality) for quality in counts)
    detail = ", ".join(
        f"{quality}: {count}" for quality, count in sorted(counts.items())
    )
    return format_absence(detail or None, refused)


def format_optional_correlation(
    value: Optional[float],
    quality: Optional[str] = None,
    digits: int = 4,
) -> str:
    """Format a correlation, or render the provenance of its absence.

    Present values render as a bare number. Absent values render as HTML
    markup that separates a structural absence from a refusal; every caller
    of this function builds HTML.
    """
    if value is not None:
        return f"{value:.{digits}f}"
    return format_absence(quality, is_refusal_quality(quality))


def coefficient_coverage(
    qualities: Mapping[Tuple[int, int], str],
    correlations: Mapping[Tuple[int, int], Optional[float]],
) -> Dict[str, Any]:
    """Report how much of a coefficient matrix carries a real comparison.

    A cell counts as compared when it produced a correlation. That is the
    ground truth of "did this cell yield evidence", read off the data rather
    than declared by a list of qualities that would then have to be kept in
    step with the taxonomy: IDENTICAL cells were compared and found bit-equal,
    while NOT_APPLICABLE, ABSENT_DIAGONAL and the refusals yielded nothing.

    Deliberately threshold-free. Whether a given coverage is acceptable is a
    human call; the failure case of an entirely zeroed matrix is already
    caught by the ABSENT_DIAGONAL refusal (#1633).
    """
    counts: Dict[str, int] = {}
    uncompared: Dict[str, int] = {}
    compared_cells = 0
    for cell, quality in qualities.items():
        counts[quality] = counts.get(quality, 0) + 1
        if correlations.get(cell) is None:
            uncompared[quality] = uncompared.get(quality, 0) + 1
        else:
            compared_cells += 1
    return {
        "compared_cells": compared_cells,
        "total_cells": len(qualities),
        "quality_counts": dict(sorted(counts.items())),
        "uncompared_quality_counts": dict(sorted(uncompared.items())),
    }


def format_coverage_summary(coverage: Mapping[str, Any]) -> str:
    """Render a coverage record as one plain-text sentence fragment."""
    compared = coverage["compared_cells"]
    total = coverage["total_cells"]
    remainder = coverage["uncompared_quality_counts"]
    summary = f"{compared} of {total} cells compared"
    if remainder:
        detail = ", ".join(
            f"{quality}: {count}"
            for quality, count in sorted(remainder.items())
        )
        summary += f" ({detail})"
    return summary

_AMPLITUDE_UNITS: Dict[DOF, str] = {
    DOF.SURGE: "m/m",
    DOF.SWAY: "m/m",
    DOF.HEAVE: "m/m",
    DOF.ROLL: "deg/m",
    DOF.PITCH: "deg/m",
    DOF.YAW: "deg/m",
}

_SOLVER_STYLES = {
    0: {"dash": "solid", "color_base": "#1f77b4"},
    1: {"dash": "dash", "color_base": "#ff7f0e"},
    2: {"dash": "dot", "color_base": "#2ca02c"},
    3: {"dash": "dashdot", "color_base": "#d62728"},
}

# Threshold: amplitude below 5% of peak is considered negligible for
# phase interpretation. Phase is physically meaningless when the signal
# is near zero (e.g. yaw on a symmetric body, or off-axis DOFs).
_NEGLIGIBLE_AMPLITUDE_RATIO = 0.05


def _is_phase_at_negligible_amplitude(
    mag_at_phase_diff: float,
    peak_mag: float,
) -> bool:
    """Return True if the amplitude where max phase diff occurs is negligible.

    Phase values are undefined / meaningless when the underlying signal
    amplitude is near zero. This helper lets the commentary and plot
    annotations communicate this clearly.
    """
    if peak_mag <= 0:
        return True  # entirely zero signal -- phase is meaningless
    return mag_at_phase_diff / peak_mag < _NEGLIGIBLE_AMPLITUDE_RATIO


def _parse_fdf_panels(fdf_path: Path) -> list[list[list[float]]]:
    """Parse a WAMIT .fdf free-surface panel file.

    Each data row contains 8 values encoding 4 panel corner coordinates in the
    horizontal plane (z=0): x1,x2,x3,x4,y1,y2,y3,y4 (all x-coords then all
    y-coords, for the XZ-symmetry quadrant y>=0).

    Returns a list of panels; each panel is a list of 4 [x, y, 0.0] vertices.
    Returns an empty list on any read failure.
    """
    try:
        panels: list[list[list[float]]] = []
        with fdf_path.open(encoding="utf-8", errors="replace") as fh:
            lines = fh.readlines()
        # First 4 lines are header (title, RINNER, NPF/NTCL, NAL params)
        for raw in lines[4:]:
            raw = raw.strip()
            if not raw:
                continue
            try:
                vals = [float(v) for v in raw.split()]
            except ValueError:
                continue
            if len(vals) != 8:
                continue
            # x1,x2,x3,x4 then y1,y2,y3,y4
            xs = vals[:4]
            ys = vals[4:]
            panels.append([[xs[i], ys[i], 0.0] for i in range(4)])
        return panels
    except Exception:
        return []


# Descriptive labels for solver input files
_FILE_DESCRIPTIONS: Dict[str, str] = {
    "OrcaWave (.owd)": (
        "OrcaWave input configuration exported from the original .owd "
        "project via SaveData(). This is the ground truth -- the exact "
        "parameters used by the manually-configured OrcaWave project."
    ),
    "OrcaWave (spec.yml)": (
        "OrcaWave input configuration exported from the spec.yml "
        "pipeline via SaveData(). This is the auto-generated project -- "
        "built by OrcaWaveRunner from the declarative spec."
    ),
    "AQWA": (
        "AQWA solver input listing (.LIS) showing the full solver "
        "configuration including element data, boundary conditions, "
        "and analysis parameters."
    ),
}


def generate_dof_observations(
    dof_name: str,
    consensus: str,
    mag_corr: float,
    phase_corr: float,
    max_mag_diff: float,
    max_phase_diff: float,
    unit: str,
    magnitude_at_max_phase_diff: float = 0.0,
    peak_magnitude: float = 0.0,
    phase_diff_at_visible_heading: bool = True,
    refusal_reason: Optional[str] = None,
) -> str:
    """Generate human-readable observation text for a DOF.

    Commentary must only describe data visible in the plot.
    When the max phase diff occurs at a hidden heading (filtered
    due to negligible response), the text acknowledges this rather
    than alarming the reader about invisible discrepancies.
    """
    lines: List[str] = []

    if consensus == "FULL":
        lines.append(
            f"<p>Solvers show <strong>full agreement</strong> on "
            f"{dof_name} response.</p>"
        )
    elif consensus == "MAJORITY":
        lines.append(
            f"<p>Solvers show <strong>majority agreement</strong> on "
            f"{dof_name}; minor outlier detected.</p>"
        )
    elif consensus == "REFUSED":
        reason = (
            f" Reason: {html.escape(refusal_reason)}."
            if refusal_reason else ""
        )
        lines.append(
            f"<p><strong>Comparison was refused</strong> for {dof_name}; "
            f"no verdict was produced.{reason}</p>"
        )
        return "\n".join(lines)
    else:
        lines.append(
            f"<p>Solvers show <strong>no consensus</strong> on "
            f"{dof_name} response -- review recommended.</p>"
        )

    if mag_corr > 0.999:
        lines.append(
            "<p>Amplitude curves are virtually identical "
            f"(r={mag_corr:.4f}).</p>"
        )
    elif mag_corr > 0.99:
        lines.append(
            f"<p>Amplitude agreement is excellent (r={mag_corr:.4f}), "
            f"with max diff of {max_mag_diff:.4g} {unit}.</p>"
        )
    elif mag_corr > 0.95:
        lines.append(
            f"<p>Amplitude correlation is good (r={mag_corr:.4f}) "
            f"but max diff reaches {max_mag_diff:.4g} {unit}.</p>"
        )
    else:
        lines.append(
            f"<p>Amplitude correlation is moderate (r={mag_corr:.4f}); "
            f"max diff of {max_mag_diff:.4g} {unit} warrants "
            "investigation.</p>"
        )

    # Phase commentary -- only describe what the reader can see.
    if not phase_diff_at_visible_heading and max_phase_diff > 20:
        lines.append(
            f"<p>Max phase difference of {max_phase_diff:.1f}&deg; "
            "occurs at a heading omitted from the plot (negligible "
            "response). Displayed headings show good phase "
            "agreement.</p>"
        )
    elif max_phase_diff > 90:
        phase_at_negligible = _is_phase_at_negligible_amplitude(
            magnitude_at_max_phase_diff, peak_magnitude,
        )
        if phase_at_negligible:
            lines.append(
                f"<p>Phase difference of {max_phase_diff:.1f}&deg; "
                "occurs where amplitude is insignificant "
                f"({magnitude_at_max_phase_diff:.2e} {unit}). "
                "Phase angle is physically undefined at near-zero "
                "magnitude and <strong>can be ignored</strong>.</p>"
            )
        else:
            lines.append(
                f"<p>Phase difference reaches {max_phase_diff:.1f}&deg; "
                f"at significant amplitude "
                f"({magnitude_at_max_phase_diff:.4g} {unit}) "
                "-- check phase convention or resonance behavior.</p>"
            )
    elif max_phase_diff > 20:
        lines.append(
            f"<p>Phase difference up to {max_phase_diff:.1f}&deg; "
            "near resonance -- typical for sharp peaks.</p>"
        )
    else:
        lines.append(
            f"<p>Phase agreement within {max_phase_diff:.1f}&deg;.</p>"
        )

    return "\n".join(lines)
