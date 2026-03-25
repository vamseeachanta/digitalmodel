"""Shared constants and utility functions for benchmark comparison modules.

ABOUTME: Pure module-level constants and helper functions used by
benchmark_plotter.py and its extracted sub-modules.
Extracted from benchmark_plotter.py as part of WRK-592 God Object split.

No imports from other benchmark_* modules — this is the leaf dependency.
"""
from __future__ import annotations

from pathlib import Path
from typing import Dict, List

from digitalmodel.hydrodynamics.diffraction.output_schemas import DOF

DOF_ORDER = [DOF.SURGE, DOF.SWAY, DOF.HEAVE, DOF.ROLL, DOF.PITCH, DOF.YAW]

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
