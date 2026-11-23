"""
ABOUTME: Hydrodynamic coefficient handling for OrcaWave frequency-domain results.
Added mass, radiation damping, and excitation force storage with frequency-dependent
interpolation.  Format conversion helpers for WAMIT, AQWA, and Nemoh outputs.
"""

import io
import re
from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, ConfigDict, Field, model_validator


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------


class HydroMatrix6x6(BaseModel):
    """A single 6x6 hydrodynamic matrix at a specific frequency."""

    frequency_rad_s: float = Field(..., description="Wave frequency in rad/s", ge=0)
    values: List[List[float]] = Field(
        ...,
        description="6x6 matrix values (row-major)",
    )

    model_config = ConfigDict(allow_inf_nan=False)

    @model_validator(mode="after")
    def finite_matrix(self):
        values = np.asarray(self.values)
        if values.shape != (6, 6) or not np.all(np.isfinite(values)):
            raise ValueError("Require a finite 6 by 6 matrix")
        # Preserve solver discretisation differences for the reciprocity gate.
        return self

    def to_numpy(self) -> np.ndarray:
        """Return values as a 6x6 numpy array."""
        return np.array(self.values)


class ExcitationForceVector(BaseModel):
    """Excitation force/moment vector at one frequency and heading."""

    frequency_rad_s: float = Field(..., ge=0)
    heading_deg: float
    amplitudes: List[float] = Field(..., description="6 DOF amplitudes")
    phases_deg: List[float] = Field(..., description="6 DOF phases in degrees")


class HydroCoefficients(BaseModel):
    """Full set of frequency-dependent hydrodynamic coefficients for one body."""

    body_name: str = Field(default="Body1")
    mass_kg: float = Field(default=0.0, description="Body mass in kg")
    cog: List[float] = Field(
        default=[0.0, 0.0, 0.0],
        description="Centre of gravity [x, y, z] in metres",
    )
    frequencies_rad_s: List[float] = Field(
        default_factory=list,
        description="Frequency vector in rad/s",
    )
    added_mass: List[HydroMatrix6x6] = Field(
        default_factory=list,
        description="Added mass matrices, one per frequency",
    )
    radiation_damping: List[HydroMatrix6x6] = Field(
        default_factory=list,
        description="Radiation damping matrices, one per frequency",
    )
    excitation_forces: List[ExcitationForceVector] = Field(
        default_factory=list,
        description="Excitation force vectors per frequency and heading",
    )
    hydrostatic_restoring: Optional[List[List[float]]] = Field(
        default=None,
        description="6x6 hydrostatic restoring matrix",
    )


# ---------------------------------------------------------------------------
# Interpolation helpers
# ---------------------------------------------------------------------------


def interpolate_matrix_at_frequency(
    matrices: List[HydroMatrix6x6],
    target_freq: float,
) -> np.ndarray:
    """Linearly interpolate a list of frequency-dependent 6x6 matrices.

    Args:
        matrices: Sorted list of HydroMatrix6x6 at increasing frequencies.
        target_freq: Desired frequency in rad/s.

    Returns:
        Interpolated 6x6 numpy array.
    """
    if not matrices:
        raise ValueError("Empty matrix list")

    freqs = np.array([m.frequency_rad_s for m in matrices])
    if target_freq <= freqs[0]:
        return matrices[0].to_numpy()
    if target_freq >= freqs[-1]:
        return matrices[-1].to_numpy()

    idx = int(np.searchsorted(freqs, target_freq)) - 1
    idx = max(0, min(idx, len(freqs) - 2))

    f0, f1 = freqs[idx], freqs[idx + 1]
    frac = (target_freq - f0) / (f1 - f0) if f1 != f0 else 0.0

    m0 = matrices[idx].to_numpy()
    m1 = matrices[idx + 1].to_numpy()
    return m0 + frac * (m1 - m0)


def interpolate_excitation_at_frequency(
    forces: List[ExcitationForceVector],
    heading_deg: float,
    target_freq: float,
) -> Tuple[np.ndarray, np.ndarray]:
    """Interpolate excitation forces at a given heading and frequency.

    Args:
        forces: List of ExcitationForceVector entries.
        heading_deg: Target heading in degrees.
        target_freq: Target frequency in rad/s.

    Returns:
        Tuple of (amplitudes_6, phases_deg_6).
    """
    # Filter to matching heading
    tol = 0.5  # degrees
    subset = [f for f in forces if abs(f.heading_deg - heading_deg) < tol]
    if not subset:
        raise ValueError(f"No excitation data for heading {heading_deg} deg")

    subset.sort(key=lambda f: f.frequency_rad_s)
    freqs = np.array([f.frequency_rad_s for f in subset])

    if target_freq <= freqs[0]:
        s = subset[0]
        return np.array(s.amplitudes), np.array(s.phases_deg)
    if target_freq >= freqs[-1]:
        s = subset[-1]
        return np.array(s.amplitudes), np.array(s.phases_deg)

    idx = int(np.searchsorted(freqs, target_freq)) - 1
    idx = max(0, min(idx, len(freqs) - 2))
    frac = (target_freq - freqs[idx]) / (freqs[idx + 1] - freqs[idx])

    a0 = np.array(subset[idx].amplitudes)
    a1 = np.array(subset[idx + 1].amplitudes)
    p0 = np.array(subset[idx].phases_deg)
    p1 = np.array(subset[idx + 1].phases_deg)

    # Interpolate in complex domain to handle phase wrapping
    z0 = a0 * np.exp(1j * np.deg2rad(p0))
    z1 = a1 * np.exp(1j * np.deg2rad(p1))
    z_interp = z0 + frac * (z1 - z0)

    return np.abs(z_interp), np.rad2deg(np.angle(z_interp))


# ---------------------------------------------------------------------------
# Native dimensional coefficient interchange (NOT WAMIT)
# ---------------------------------------------------------------------------


def to_native_added_mass(coeffs: HydroCoefficients) -> str:
    """Export native dimensional rows: omega(rad/s), i, j, A(SI).

    This is not WAMIT .1. See WAMIT 7.5 manual sections 3.2 and 5.2.
    """
    lines = []
    for mat in coeffs.added_mass:
        omega = mat.frequency_rad_s
        arr = mat.to_numpy()
        for i in range(6):
            for j in range(6):
                if abs(arr[i, j]) > 1e-30:
                    lines.append(f" {omega:12.6E}  {i + 1}  {j + 1}  {arr[i, j]:14.6E}")
    return "\n".join(lines) + "\n"


def to_native_damping(coeffs: HydroCoefficients) -> str:
    """Export native dimensional rows: omega(rad/s), i, j, B(SI).

    WAMIT .3 is exciting force output, not damping (manual section 5.2).
    """
    lines = []
    for mat in coeffs.radiation_damping:
        omega = mat.frequency_rad_s
        arr = mat.to_numpy()
        for i in range(6):
            for j in range(6):
                if abs(arr[i, j]) > 1e-30:
                    lines.append(f" {omega:12.6E}  {i + 1}  {j + 1}  {arr[i, j]:14.6E}")
    return "\n".join(lines) + "\n"


def from_native_added_mass(text: str) -> List[HydroMatrix6x6]:
    """Parse native dimensional added-mass rows (omega, i, j, A).

    Args:
        text: Native dimensional text; never pass a WAMIT file.

    Returns:
        List of HydroMatrix6x6 sorted by frequency.
    """
    data: Dict[float, np.ndarray] = {}
    for line_number, line in enumerate(text.splitlines(), start=1):
        parts = line.split()
        if not parts:
            continue
        context = f"Native coefficient line {line_number}: "
        if len(parts) != 4:
            raise ValueError(context + "expected four columns: omega, i, j, value")
        try:
            omega = float(parts[0])
            i, j = int(parts[1]) - 1, int(parts[2]) - 1
            val = float(parts[3])
        except ValueError as exc:
            raise ValueError(context + "invalid numeric field") from exc
        if not np.isfinite(omega) or omega < 0 or not np.isfinite(val):
            raise ValueError(context + "finite nonnegative frequency and finite value required")
        if not (0 <= i < 6 and 0 <= j < 6):
            raise ValueError(context + "indices must be integers in 1..6")
        if omega not in data:
            data[omega] = np.zeros((6, 6))
        data[omega][i, j] = val

    matrices = []
    for omega in sorted(data.keys()):
        matrices.append(
            HydroMatrix6x6(
                frequency_rad_s=omega,
                values=data[omega].tolist(),
            )
        )
    return matrices


def to_wamit_added_mass(*args, **kwargs):
    """Removed mislabeled format. Use to_native_added_mass for native SI rows."""
    raise ValueError("Not a WAMIT writer; use to_native_added_mass for native SI rows")


def to_wamit_damping(*args, **kwargs):
    """Removed mislabeled format. Use to_native_damping for native SI rows."""
    raise ValueError("Not a WAMIT writer; use to_native_damping for native SI rows")


def from_wamit_added_mass(*args, **kwargs):
    """Refuse WAMIT data rather than silently treating period as frequency."""
    raise ValueError(
        "WAMIT import unsupported; from_native_added_mass accepts native SI rows only"
    )


def create_hydrostatic_restoring(
    displaced_volume_m3: float,
    waterplane_area_m2: float,
    gm_t: float,
    gm_l: float,
    rho: float = 1025.0,
    g: float = 9.81,
) -> List[List[float]]:
    """Build diagonal restoring about the centre of flotation, in SI units.

    WAMIT 7.5 manual 3.1: C33=rho*g*Awp; C44/55=rho*g*V*GM.
    Assumes principal horizontal axes and coincident reference/centre of
    flotation. C35/C53 and all other off-diagonal coupling are omitted;
    transform a full matrix externally for an offset reference.


    Args:
        displaced_volume_m3: Displaced volume.
        waterplane_area_m2: Waterplane area.
        gm_t: Transverse metacentric height (m).
        gm_l: Longitudinal metacentric height (m).
        rho: Water density (kg/m^3).
        g: Gravitational acceleration (m/s^2).

    Returns:
        6x6 matrix as nested lists.
    """
    c = np.zeros((6, 6))
    mass = rho * displaced_volume_m3
    c[2, 2] = rho * g * waterplane_area_m2  # heave
    c[3, 3] = mass * g * gm_t  # roll
    c[4, 4] = mass * g * gm_l  # pitch
    return c.tolist()
