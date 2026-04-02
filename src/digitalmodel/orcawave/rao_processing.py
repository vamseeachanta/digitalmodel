"""
ABOUTME: RAO data processing for OrcaWave diffraction analysis results.
Read RAO tables from CSV/YAML, interpolate between headings and periods,
convert between amplitude/phase and real/imaginary formats, combine RAOs
for multi-body systems, and compare RAOs between vessels.
"""

import csv
import io
from typing import Dict, List, Optional, Tuple, Union

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class RAOEntry(BaseModel):
    """Single RAO value at a given period and heading."""

    period: float = Field(..., description="Wave period in seconds", gt=0)
    heading_deg: float = Field(..., description="Wave heading in degrees")
    amplitude: float = Field(..., description="RAO amplitude (e.g. m/m for heave)")
    phase_deg: float = Field(..., description="Phase angle in degrees")


class RAOTable(BaseModel):
    """Complete RAO table for one degree of freedom."""

    dof: str = Field(..., description="Degree of freedom label, e.g. 'Heave'")
    unit: str = Field(default="m/m", description="RAO unit string")
    periods: List[float] = Field(..., description="Wave periods in seconds")
    headings_deg: List[float] = Field(..., description="Wave headings in degrees")
    amplitudes: List[List[float]] = Field(
        ...,
        description="Amplitude matrix [n_headings x n_periods]",
    )
    phases_deg: List[List[float]] = Field(
        ...,
        description="Phase matrix [n_headings x n_periods] in degrees",
    )


class RAOComparisonResult(BaseModel):
    """Result of comparing two RAO tables."""

    dof: str
    max_amplitude_diff: float
    mean_amplitude_diff: float
    rms_amplitude_diff: float
    max_phase_diff_deg: float
    common_periods: List[float]
    common_headings_deg: List[float]


# ---------------------------------------------------------------------------
# Core functions
# ---------------------------------------------------------------------------

def amplitude_phase_to_complex(
    amplitude: np.ndarray,
    phase_deg: np.ndarray,
) -> np.ndarray:
    """Convert amplitude/phase representation to complex (real + j*imag).

    Args:
        amplitude: RAO amplitudes, any shape.
        phase_deg: Phase angles in degrees, same shape as *amplitude*.

    Returns:
        Complex-valued array of same shape.
    """
    phase_rad = np.deg2rad(phase_deg)
    return amplitude * (np.cos(phase_rad) + 1j * np.sin(phase_rad))


def complex_to_amplitude_phase(
    z: np.ndarray,
) -> Tuple[np.ndarray, np.ndarray]:
    """Convert complex representation to amplitude and phase (degrees).

    Args:
        z: Complex-valued RAO array.

    Returns:
        Tuple of (amplitude, phase_deg) arrays.
    """
    amplitude = np.abs(z)
    phase_deg = np.rad2deg(np.angle(z))
    return amplitude, phase_deg


def read_rao_csv(text: str) -> RAOTable:
    """Parse an OrcaWave-style CSV RAO export.

    Expected CSV layout::

        DOF,Heave
        Unit,m/m
        Period\\Heading,0,45,90,135,180
        3.0,0.10/5.0,0.12/6.0,...
        4.0,...

    Each cell is ``amplitude/phase_deg``.

    Args:
        text: CSV string content.

    Returns:
        Populated RAOTable.
    """
    reader = csv.reader(io.StringIO(text))
    rows = list(reader)

    dof = rows[0][1].strip() if len(rows[0]) > 1 else "Unknown"
    unit = rows[1][1].strip() if len(rows[1]) > 1 else "m/m"

    header_row = rows[2]
    headings_deg = [float(h) for h in header_row[1:]]

    periods: List[float] = []
    amplitudes: List[List[float]] = []
    phases_deg: List[List[float]] = []

    # Transpose: we read period-rows but store [heading x period]
    amp_by_heading: Dict[int, List[float]] = {i: [] for i in range(len(headings_deg))}
    pha_by_heading: Dict[int, List[float]] = {i: [] for i in range(len(headings_deg))}

    for row in rows[3:]:
        if not row or not row[0].strip():
            continue
        periods.append(float(row[0]))
        for hi, cell in enumerate(row[1:]):
            parts = cell.strip().split("/")
            amp_by_heading[hi].append(float(parts[0]))
            pha_by_heading[hi].append(float(parts[1]) if len(parts) > 1 else 0.0)

    amplitudes = [amp_by_heading[i] for i in range(len(headings_deg))]
    phases_deg = [pha_by_heading[i] for i in range(len(headings_deg))]

    return RAOTable(
        dof=dof,
        unit=unit,
        periods=periods,
        headings_deg=headings_deg,
        amplitudes=amplitudes,
        phases_deg=phases_deg,
    )


def interpolate_rao(
    table: RAOTable,
    target_period: float,
    target_heading_deg: float,
) -> Tuple[float, float]:
    """Bilinear interpolation of RAO at arbitrary period and heading.

    Args:
        table: Source RAO table.
        target_period: Desired wave period (s).
        target_heading_deg: Desired wave heading (deg).

    Returns:
        (amplitude, phase_deg) at the interpolated point.
    """
    periods = np.array(table.periods)
    headings = np.array(table.headings_deg)
    amp = np.array(table.amplitudes)   # [n_headings, n_periods]
    pha = np.array(table.phases_deg)

    # Convert to complex for smooth interpolation across phase wrapping
    z = amplitude_phase_to_complex(amp, pha)

    # Clamp to bounds
    tp = np.clip(target_period, periods.min(), periods.max())
    th = np.clip(target_heading_deg, headings.min(), headings.max())

    # Find bracketing indices for period
    ip = np.searchsorted(np.sort(periods), tp) - 1
    ip = np.clip(ip, 0, len(periods) - 2)
    sorted_periods = np.sort(periods)
    t_frac = (tp - sorted_periods[ip]) / max(sorted_periods[ip + 1] - sorted_periods[ip], 1e-12)

    # Find bracketing indices for heading
    sorted_headings = np.sort(headings)
    ih = np.searchsorted(sorted_headings, th) - 1
    ih = np.clip(ih, 0, len(headings) - 2)
    h_frac = (th - sorted_headings[ih]) / max(sorted_headings[ih + 1] - sorted_headings[ih], 1e-12)

    # Sort z to match sorted axes
    period_order = np.argsort(periods)
    heading_order = np.argsort(headings)
    z_sorted = z[np.ix_(heading_order, period_order)]

    # Bilinear interpolation in complex domain
    z00 = z_sorted[ih, ip]
    z01 = z_sorted[ih, ip + 1]
    z10 = z_sorted[ih + 1, ip]
    z11 = z_sorted[ih + 1, ip + 1]

    z_interp = (
        z00 * (1 - t_frac) * (1 - h_frac)
        + z01 * t_frac * (1 - h_frac)
        + z10 * (1 - t_frac) * h_frac
        + z11 * t_frac * h_frac
    )

    amp_out, pha_out = complex_to_amplitude_phase(np.array([z_interp]))
    return float(amp_out[0]), float(pha_out[0])


def combine_raos_multi_body(
    rao_a: RAOTable,
    rao_b: RAOTable,
    coupling_factor: float = 0.0,
) -> RAOTable:
    """Combine RAOs from two bodies with optional coupling.

    For coupled multi-body analysis the combined response can be estimated
    as a weighted superposition.  With ``coupling_factor == 0`` the result
    is a simple vector sum of the complex RAOs.

    Args:
        rao_a: RAO table for body A.
        rao_b: RAO table for body B (must share the same period/heading grid).
        coupling_factor: Coupling coefficient in [0, 1]. 0 = independent sum.

    Returns:
        Combined RAOTable.
    """
    amp_a = np.array(rao_a.amplitudes)
    pha_a = np.array(rao_a.phases_deg)
    amp_b = np.array(rao_b.amplitudes)
    pha_b = np.array(rao_b.phases_deg)

    za = amplitude_phase_to_complex(amp_a, pha_a)
    zb = amplitude_phase_to_complex(amp_b, pha_b)

    z_combined = za + zb * (1.0 - coupling_factor)

    amp_out, pha_out = complex_to_amplitude_phase(z_combined)

    return RAOTable(
        dof=f"{rao_a.dof}+{rao_b.dof}",
        unit=rao_a.unit,
        periods=list(rao_a.periods),
        headings_deg=list(rao_a.headings_deg),
        amplitudes=amp_out.tolist(),
        phases_deg=pha_out.tolist(),
    )


def compare_raos(
    table_a: RAOTable,
    table_b: RAOTable,
) -> RAOComparisonResult:
    """Compare two RAO tables on their common period/heading grid.

    Args:
        table_a: First RAO table.
        table_b: Second RAO table.

    Returns:
        RAOComparisonResult with difference metrics.
    """
    common_periods = sorted(set(table_a.periods) & set(table_b.periods))
    common_headings = sorted(set(table_a.headings_deg) & set(table_b.headings_deg))

    if not common_periods or not common_headings:
        return RAOComparisonResult(
            dof=table_a.dof,
            max_amplitude_diff=float("nan"),
            mean_amplitude_diff=float("nan"),
            rms_amplitude_diff=float("nan"),
            max_phase_diff_deg=float("nan"),
            common_periods=common_periods,
            common_headings_deg=common_headings,
        )

    amp_a = np.array(table_a.amplitudes)
    amp_b = np.array(table_b.amplitudes)
    pha_a = np.array(table_a.phases_deg)
    pha_b = np.array(table_b.phases_deg)

    # Build index maps
    pa_idx = {p: i for i, p in enumerate(table_a.periods)}
    pb_idx = {p: i for i, p in enumerate(table_b.periods)}
    ha_idx = {h: i for i, h in enumerate(table_a.headings_deg)}
    hb_idx = {h: i for i, h in enumerate(table_b.headings_deg)}

    amp_diffs = []
    pha_diffs = []
    for h in common_headings:
        for p in common_periods:
            da = amp_a[ha_idx[h], pa_idx[p]] - amp_b[hb_idx[h], pb_idx[p]]
            dp = pha_a[ha_idx[h], pa_idx[p]] - pha_b[hb_idx[h], pb_idx[p]]
            # Wrap phase difference to [-180, 180]
            dp = (dp + 180) % 360 - 180
            amp_diffs.append(abs(da))
            pha_diffs.append(abs(dp))

    amp_diffs_arr = np.array(amp_diffs)
    pha_diffs_arr = np.array(pha_diffs)

    return RAOComparisonResult(
        dof=table_a.dof,
        max_amplitude_diff=float(amp_diffs_arr.max()),
        mean_amplitude_diff=float(amp_diffs_arr.mean()),
        rms_amplitude_diff=float(np.sqrt((amp_diffs_arr**2).mean())),
        max_phase_diff_deg=float(pha_diffs_arr.max()),
        common_periods=common_periods,
        common_headings_deg=common_headings,
    )


def rao_table_to_csv(table: RAOTable) -> str:
    """Serialize an RAOTable back to OrcaWave CSV format.

    Args:
        table: The RAO table to serialize.

    Returns:
        CSV string.
    """
    lines = []
    lines.append(f"DOF,{table.dof}")
    lines.append(f"Unit,{table.unit}")

    header = "Period\\Heading," + ",".join(str(h) for h in table.headings_deg)
    lines.append(header)

    amp = np.array(table.amplitudes)   # [n_headings, n_periods]
    pha = np.array(table.phases_deg)

    for pi, period in enumerate(table.periods):
        cells = []
        for hi in range(len(table.headings_deg)):
            cells.append(f"{amp[hi, pi]:.6g}/{pha[hi, pi]:.2f}")
        lines.append(f"{period}," + ",".join(cells))

    return "\n".join(lines) + "\n"
