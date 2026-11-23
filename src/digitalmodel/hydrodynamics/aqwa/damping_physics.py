"""Offline harmonic damping helpers; governing equations: docs/damping-basis.md."""
from hashlib import sha256
from pathlib import Path

import numpy as np
from scipy.optimize import brentq


def citation_sidecar():
    """Cite the shipped, independently derived E1-E5; fail if basis is missing.

    This is a derivation citation, not a claim of licensed-standard compliance.
    No private wiki or commercial document is distributed with this calculation.
    """
    path = Path(__file__).with_name("docs") / "damping-basis.md"
    content = path.read_bytes()
    return {"citations": [{"type": "Citation", "code_id": "AQWA-HARMONIC-1",
            "publisher": "digitalmodel", "revision": "1", "section": "E1-E3",
            "source_sibling": "generic", "source_project": None,
            "source": "docs/damping-basis.md", "sha256": sha256(content).hexdigest()}]}


def positive(value, name):
    value = float(value)
    if not np.isfinite(value) or value <= 0:
        raise ValueError(f"{name} must be finite and positive")
    return value


def frequency_grid(values):
    omega = np.asarray(values, dtype=float)
    if omega.ndim != 1 or not omega.size or not np.all(np.isfinite(omega)):
        raise ValueError("Frequency grid must be a nonempty finite vector")
    if np.any(omega <= 0) or np.any(np.diff(omega) <= 0):
        raise ValueError("Frequency grid must be positive and strictly increasing")
    return omega


def motion_amplitudes(raos, dof, length, wave_amplitude, required):
    entry = raos.get(dof)
    if entry is None and not required:
        return np.zeros(length)
    if not isinstance(entry, dict):
        raise ValueError(f"{dof} RAO must declare amplitude and unit")
    unit = entry.get("unit")
    allowed = {"m/m": 1.0} if dof == "heave" else {"rad/m": 1.0, "deg/m": np.pi / 180}
    if unit not in allowed:
        raise ValueError(f"{dof} RAO unit must be one of {tuple(allowed)}")
    values = np.asarray(entry.get("amplitude"), dtype=float)
    if values.ndim == 1:
        values = values[:, None]
    if values.ndim != 2 or values.shape[0] != length or not values.shape[1]:
        raise ValueError(f"{dof} RAO shape must match frequencies and headings")
    if not np.all(np.isfinite(values)) or np.any(values < 0):
        raise ValueError(f"{dof} RAO amplitudes must be finite and nonnegative")
    # Heading quadrature is a declared representative harmonic amplitude, not sea RMS.
    return np.sqrt(np.mean(values ** 2, axis=1)) * allowed[unit] * wave_amplitude


def resonance(omega, effective_mass, stiffness, dof):
    """Solve E4 using piecewise-linear A(omega); reject ambiguous roots."""
    roots = []
    # On each segment A is linear; omega**2 * (I+A) - C is cubic.
    # Subdivide at the one possible positive stationary point to avoid missed roots.
    for index in range(len(omega) - 1):
        lo, hi = omega[index:index + 2]
        slope = (effective_mass[index + 1] - effective_mass[index]) / (hi - lo)
        intercept = effective_mass[index] - slope * lo
        points = [lo, hi]
        turning = -2 * intercept / (3 * slope) if slope else -1
        if lo < turning < hi:
            points.insert(1, turning)
        function = lambda w: w*w*(intercept + slope*w) - stiffness
        for left, right in zip(points, points[1:]):
            if np.isclose(function(left), 0, atol=stiffness*1e-12, rtol=0):
                roots.append(left)
            if function(left) * function(right) < 0:
                roots.append(brentq(function, left, right))
            if np.isclose(function(right), 0, atol=stiffness*1e-12, rtol=0):
                roots.append(right)
    if len(omega) == 1 and np.isclose(omega[0]**2 * effective_mass[0], stiffness):
        roots.append(omega[0])
    unique = []
    for root in sorted(roots):
        if not unique or not np.isclose(root, unique[-1], rtol=1e-9, atol=1e-12):
            unique.append(root)
    if len(unique) != 1:
        raise ValueError(f"{dof}: expected one resonance inside frequency grid; found {len(unique)}")
    return unique[0]


def critical_dof(omega, damping, inertia, stiffness, added_mass, dof):
    """E4-E5: evaluate damping at the solved natural frequency."""
    inertia = positive(inertia, f"{dof} inertia")
    stiffness = positive(stiffness, f"{dof} restoring")
    added_mass = np.broadcast_to(np.asarray(added_mass, dtype=float), omega.shape)
    mass = inertia + added_mass
    if not np.all(np.isfinite(mass)) or np.any(mass <= 0):
        raise ValueError(f"{dof}: effective mass must be finite and positive")
    damping = np.asarray(damping, dtype=float)
    if damping.shape != omega.shape or not np.all(np.isfinite(damping)) or np.any(damping < 0):
        raise ValueError(f"{dof}: damping must be finite, nonnegative and match grid")
    natural = resonance(omega, mass, stiffness, dof)
    resonant_mass = float(np.interp(natural, omega, mass))
    resonant_damping = float(np.interp(natural, omega, damping))
    critical = 2 * resonant_mass * natural
    return {"critical_damping_ratio": damping / critical,
            "zeta_at_natural_frequency": resonant_damping / critical,
            "natural_frequency_rad_s": natural,
            "natural_period_s": 2*np.pi / natural,
            "damping_at_natural_frequency": resonant_damping,
            "damping": damping, "denominator": resonant_mass}
