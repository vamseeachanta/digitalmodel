# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Linear RAO response without a diffraction solver or plotting dependencies.

Basis: Orcina, 'Vessel theory: RAOs and phases', displacement RAOs,
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Vesseltheory,RAOsandphases.htm
The spectral identity follows by applying that linear transfer to independent
Fourier components. See also Waves in Ocean Engineering (1991), pp107-108,
for the wave spectral basis reproduced in Orcina's 'Waves: Wave spectra'.
"""

from dataclasses import dataclass

import numpy as np

from ..seakeeping import compute_response_spectrum, significant_amplitude, spectral_moments


@dataclass(frozen=True)
class SpectralResponse:
    """One-sided response density per rad/s and integrated response statistics.

    m0 has squared response units; rms and significant_amplitude have response
    units. The latter is 2*sqrt(m0), a significant single-amplitude measure,
    not the crest-to-trough height 4*sqrt(m0). Interpreting it as a mean of the
    highest third of peaks requires a narrow-band Gaussian process.
    """

    spectrum: np.ndarray
    m0: float
    rms: float
    significant_amplitude: float


def _vector(values, name, *, size=None, complex_values=False):
    """Reject implicit broadcasting, complex physical inputs and nonfinite data."""
    raw = np.asarray(values)
    if not complex_values and np.iscomplexobj(raw):
        raise ValueError(f"{name} must be real")
    data = np.asarray(raw, dtype=complex if complex_values else float)
    if data.ndim != 1 or not data.size or not np.all(np.isfinite(data)):
        raise ValueError(f"{name} must be a finite nonempty one-dimensional array")
    if size is not None and len(data) != size:
        raise ValueError(f"{name} length must match frequency length")
    return data


def _omega(axis_values, axis, *, minimum_size=1):
    values = _vector(axis_values, "frequency axis")
    if np.any(values <= 0) or len(values) < minimum_size:
        raise ValueError("frequency axis must be positive and sufficiently long")
    if axis == "omega":
        omega = values
    elif axis == "frequency":
        omega = 2 * np.pi * values
    elif axis == "period":
        omega = 2 * np.pi / values
    else:
        raise ValueError("frequency axis must be omega, frequency or period")
    if not np.all(np.isfinite(omega)) or np.any(np.diff(omega) <= 0):
        raise ValueError("angular frequency must be strictly increasing")
    return omega


def regular_wave_response(
    axis_values, rao_amplitudes, wave_amplitudes, times, *,
    rao_phases=None, wave_phases=None, axis="omega", phase_unit="rad",
    phase_convention="lag",
):
    """Return sum R_j*a_j*cos(omega_j*t + theta_j - phi_j) for lag phases.

    ``axis`` is 'omega' (rad/s), 'frequency' (Hz), or 'period' (seconds).
    Angular frequencies must ascend; period axes must therefore descend.
    All component arrays have the same length; there is no interpolation,
    extrapolation, sorting or scalar broadcasting. Times are seconds.

    RAO and wave amplitudes are nonnegative. Translational RAOs in m/m with
    wave amplitudes in m return m; rotational RAOs in rad/m or deg/m return
    rad or deg respectively. Response units follow RAO amplitude units; the
    ``phase_unit`` argument converts phases only, never rotational amplitudes.

    Both phase arrays use ``phase_unit`` ('rad' or 'deg'). Wave phase theta
    defines eta=a*cos(omega*t+theta). A positive RAO 'lag' delays the crest;
    'lead' uses +phi instead. The complex transfer for lag is R*exp(-i*phi)
    when time dependence is exp(+i*omega*t). Omitted phases are zero.
    These are explicit local conventions, not inferred file conventions.
    """
    omega = _omega(axis_values, axis)
    n = len(omega)
    rao = _vector(rao_amplitudes, "RAO amplitudes", size=n)
    wave = _vector(wave_amplitudes, "wave amplitudes", size=n)
    time = _vector(times, "times")
    if np.any(rao < 0) or np.any(wave < 0):
        raise ValueError("amplitudes must be nonnegative; express sign as phase")
    phi = np.zeros(n) if rao_phases is None else _vector(rao_phases, "RAO phases", size=n)
    theta = np.zeros(n) if wave_phases is None else _vector(wave_phases, "wave phases", size=n)
    if phase_unit == "deg":
        phi, theta = np.deg2rad(phi), np.deg2rad(theta)
    elif phase_unit != "rad":
        raise ValueError("phase_unit must be rad or deg")
    if phase_convention not in ("lag", "lead"):
        raise ValueError("phase_convention must be lag or lead")
    sign = -1 if phase_convention == "lag" else 1
    argument = omega[:, None] * time[None, :] + theta[:, None] + sign * phi[:, None]
    return np.sum((rao * wave)[:, None] * np.cos(argument), axis=0)


def spectral_response(omega, rao, wave_spectrum):
    """Return S_r=|RAO|²*S_w, m0=integral(S_r dw), RMS and 2*sqrt(m0).

    ``omega`` is strictly ascending positive angular frequency (rad/s), with
    at least two points. Both densities are one-sided, per rad/s; wave density
    is m²/(rad/s). Complex RAOs are accepted, with response units per metre.
    Frequency/period densities must be transformed with their Jacobian before
    calling; merely relabelling an axis is invalid. No tail is extrapolated,
    renormalized or presumed negligible. m0 covers the supplied band only.
    Mismatched lengths, nonfinite data and negative densities raise ValueError.
    """
    omega = _omega(omega, "omega", minimum_size=2)
    transfer = _vector(rao, "RAO", size=len(omega), complex_values=True)
    density = _vector(wave_spectrum, "wave spectrum", size=len(omega))
    if np.any(density < 0):
        raise ValueError("wave spectrum must be nonnegative")
    response = compute_response_spectrum(np.abs(transfer), density)
    m0 = spectral_moments(omega, response, orders=[0])[0]
    if not np.isfinite(m0):
        raise ValueError("response variance must be finite")
    return SpectralResponse(response, m0, float(np.sqrt(m0)), significant_amplitude(m0))
