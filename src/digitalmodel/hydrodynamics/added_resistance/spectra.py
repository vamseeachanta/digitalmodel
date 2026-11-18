# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit-band, long-crested spectral integration without response extrapolation."""

from dataclasses import dataclass
from math import exp, expm1, pi
from typing import Sequence

import numpy as np

from ..wave_spectra import WaveSpectra
from .models import ResistanceRow, Status, finite

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


def frequency_grid(values: Sequence[float]) -> np.ndarray:
    omega = np.asarray(values, dtype=float)
    if (
        omega.ndim != 1
        or len(omega) < 2
        or not np.all(np.isfinite(omega))
        or np.any(omega <= 0)
        or np.any(np.diff(omega) <= 0)
    ):
        raise ValueError(
            "wave frequencies must be finite, positive, strictly increasing and have at least two samples"
        )
    return omega


def nonnegative_samples(
    values: Sequence[float], omega: np.ndarray, name: str
) -> np.ndarray:
    samples = np.asarray(values, dtype=float)
    if (
        samples.shape != omega.shape
        or not np.all(np.isfinite(samples))
        or np.any(samples < 0)
    ):
        raise ValueError(
            f"{name} must be finite, nonnegative and match the frequency grid"
        )
    return samples


def check_band(omega_min: float, omega_max: float) -> None:
    finite(omega_min, "lower frequency cutoff", positive=True)
    finite(omega_max, "upper frequency cutoff", positive=True)
    if omega_max <= omega_min:
        raise ValueError("upper cutoff must exceed lower cutoff")


def mean_added_resistance(
    omega_rad_s: Sequence[float],
    density_m2_s: Sequence[float],
    resistance_per_amplitude_squared: Sequence[float],
    *,
    omega_min: float,
    omega_max: float,
) -> float:
    """Return 2*integral S(omega)*[R_regular(omega)/a²] d(omega), in N.

    Source: ITTC 7.5-02-07-02.2 (2024, revision 07), equation (19),
    https://ittc.info/media/11890/75-02-07-022.pdf, long-crested reduction.
    Frequencies are absolute wave frequencies, not encounter frequencies;
    no Doppler Jacobian is appropriate here. S is one-sided elevation-variance
    density in m²/(rad/s). The factor two relates sinusoidal amplitude squared
    to variance. Trapezoidal quadrature uses the supplied product samples.
    Cutoffs must equal the grid endpoints: truncation is explicit, not hidden.
    """
    omega = frequency_grid(omega_rad_s)
    spectrum = nonnegative_samples(density_m2_s, omega, "spectrum")
    response = nonnegative_samples(
        resistance_per_amplitude_squared, omega, "resistance response"
    )
    check_band(omega_min, omega_max)
    if omega[0] != omega_min or omega[-1] != omega_max:
        raise ValueError("cutoffs must match the supplied frequency grid endpoints")
    result = float(2 * _trapezoid(spectrum * response, omega))
    finite(result, "integrated resistance")
    return result


def integrate_regular_table(
    rows: Sequence[ResistanceRow],
    omega_rad_s: Sequence[float],
    density_m2_s: Sequence[float],
    *,
    omega_min: float,
    omega_max: float,
) -> float:
    """Linearly interpolate R/a² on wave frequency; reject gaps and extrapolation.

    All rows must be evaluated. The caller is responsible for providing one
    method, speed, heading and applicability context, and enough resolution.
    """
    if len(rows) < 2:
        raise ValueError("at least two regular-wave rows are required")
    if any(row.status != Status.EVALUATED or row.resistance_n is None for row in rows):
        raise ValueError("NOT_EVALUATED rows cannot enter a spectral mean")
    if len({row.method for row in rows}) != 1:
        raise ValueError("one method is required per integration")
    ordered = sorted(rows, key=lambda row: row.omega_rad_s)
    source_omega = frequency_grid([row.omega_rad_s for row in ordered])
    for row in ordered:
        finite(row.amplitude_m, "regular-wave amplitude", positive=True)
    response = nonnegative_samples(
        [row.resistance_n / row.amplitude_m**2 for row in ordered],
        source_omega,
        "regular resistance response",
    )
    omega = frequency_grid(omega_rad_s)
    if omega[0] < source_omega[0] or omega[-1] > source_omega[-1]:
        raise ValueError(
            "regular response coverage does not span the spectrum; no extrapolation"
        )
    return mean_added_resistance(
        omega,
        density_m2_s,
        np.interp(omega, source_omega, response),
        omega_min=omega_min,
        omega_max=omega_max,
    )


@dataclass(frozen=True)
class SpectrumBand:
    omega_rad_s: np.ndarray
    density_m2_s: np.ndarray
    variance_m2: float
    omitted_variance_fraction: float
    omega_min: float
    omega_max: float
    normalized_to_hs: bool


def pm_spectrum(
    hs_m: float,
    tp_s: float,
    *,
    omega_min: float,
    omega_max: float,
    n_points: int,
) -> SpectrumBand:
    """Reuse the repository's Hs/Tp two-parameter spectrum with mandatory explicit cutoffs.

    The existing generator normalizes finite-band m0 to Hs²/16; this does not
    recover the tail (issue #2613), nor establish convergence of force moments.
    The omitted *unrenormalized* variance fraction is reported independently.
    Source for 5/4: repository wave_spectra.py, WaveSpectra.pierson_moskowitz,
    documented spectrum S=A*omega^-5*exp[-(5/4)*(omega_p/omega)^4]. Analytic
    substitution y=(5/4)*(omega_p/omega)^4 gives cumulative variance below w:
    F(w)=exp[-(5/4)*(omega_p/w)^4]. Omitted fraction=F(w_min)+1-F(w_max).
    This is the two-parameter spectrum shape already used by this repository.
    No default band, tail-acceptance threshold or new empirical constant is used.
    """
    finite(hs_m, "significant height")
    finite(tp_s, "peak period", positive=True)
    check_band(omega_min, omega_max)
    if not isinstance(n_points, int) or isinstance(n_points, bool) or n_points < 2:
        raise ValueError(
            "number of frequency samples must be an integer of at least two"
        )
    omega, spectrum = WaveSpectra().pierson_moskowitz(
        hs=hs_m,
        tp=tp_s,
        freq_min=omega_min,
        freq_max=omega_max,
        n_points=n_points,
    )
    nonnegative_samples(spectrum, omega, "generated spectrum")
    variance = float(_trapezoid(spectrum, omega))
    if hs_m > 0 and variance == 0:
        raise ValueError(
            "spectrum band has no representable energy; change the cutoffs"
        )
    wp = 2 * pi / tp_s
    beta = 5 / 4  # Source and analytic derivation are given in this docstring.
    omitted = exp(-beta * (wp / omega_min) ** 4) - expm1(-beta * (wp / omega_max) ** 4)
    omega.setflags(write=False)
    spectrum.setflags(write=False)
    return SpectrumBand(omega, spectrum, variance, omitted, omega_min, omega_max, True)
