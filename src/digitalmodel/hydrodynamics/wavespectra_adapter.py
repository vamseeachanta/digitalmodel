# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Convert supplied angular-frequency spectra to optional wavespectra arrays.

One cycle is two pi radians: f = omega / (2*pi), and conservation of
variance S_f df = S_omega d_omega gives S_f = 2*pi*S_omega. This linear
change of variables preserves the trapezoidal zeroth moment on the supplied
grid without sorting, interpolation, normalization or tail extrapolation.
Other integration rules used by wavespectra accessors can give different
moments on a finite grid.

The DataArray contract follows wavespectra's installed
``core.attributes`` definitions (checked against release 4.9.0): ``ef`` on
``freq``, frequency in Hz and omnidirectional density in m2 s.

Example:
    With wavespectra and xarray installed, use a synthetic two-point spectrum
    with angular frequencies of one and two cycles per second:

    >>> from math import pi
    >>> spectrum = to_wavespectra([2*pi, 4*pi], [1/(2*pi), 0.0])
    >>> spectrum.freq.values.tolist()
    [1.0, 2.0]
    >>> spectrum.values.tolist()
    [1.0, 0.0]
    >>> spectrum.attrs["units"]
    'm2 s'
    >>> spectrum.spec.oned().dims
    ('freq',)
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Sequence

import numpy as np

if TYPE_CHECKING:
    from xarray import DataArray


class MissingWavespectraDependencyError(ImportError):
    """The optional wavespectra or xarray package cannot be imported."""


def to_wavespectra(
    omega: Sequence[float] | np.ndarray,
    spectral_density: Sequence[float] | np.ndarray,
) -> DataArray:
    """Return a wavespectra-compatible omnidirectional frequency DataArray.

    Args:
        omega: Strictly increasing positive angular frequencies in rad/s.
            Supply a one-dimensional real numeric array with at least two
            points so that trapezoidal integration is defined.
        spectral_density: Matching finite nonnegative densities in m2 s/rad.

    Returns:
        A new DataArray named ``ef`` with dimension ``freq`` in Hz and
        density in m2 s. Importing wavespectra registers its ``spec`` accessor.

    Raises:
        ValueError: Arrays have invalid shapes, types, frequencies or densities,
            or the converted values cannot be represented as finite floats
            with strictly increasing positive frequencies.
        MissingWavespectraDependencyError: Install wavespectra and xarray to
            use this adapter. Core imports do not require either package.
    """
    try:
        angular = np.asarray(omega)
        density = np.asarray(spectral_density)
    except (TypeError, ValueError) as exc:
        raise ValueError("Supply one-dimensional real numeric arrays.") from exc

    if (
        angular.ndim != 1
        or density.ndim != 1
        or angular.size < 2
        or angular.shape != density.shape
    ):
        raise ValueError("Arrays must be one-dimensional with matching lengths >= 2.")
    if angular.dtype.kind not in "iuf" or density.dtype.kind not in "iuf":
        raise ValueError("Arrays must contain real numeric values, excluding booleans.")

    angular = angular.astype(float)
    density = density.astype(float)
    if (
        not np.all(np.isfinite(angular))
        or np.any(angular <= 0)
        or np.any(angular[1:] <= angular[:-1])
    ):
        raise ValueError("Angular frequencies must be finite, positive and increasing.")
    if not np.all(np.isfinite(density)) or np.any(density < 0):
        raise ValueError("Spectral densities must be finite and nonnegative.")

    # Definition of angular measure: one cycle = two pi radians.
    two_pi = 2 * np.pi
    with np.errstate(over="ignore", under="ignore"):
        frequency = angular / two_pi
        frequency_density = density * two_pi
    if (
        not np.all(np.isfinite(frequency_density))
        or np.any(frequency <= 0)
        or np.any(frequency[1:] <= frequency[:-1])
    ):
        raise ValueError("Converted spectrum cannot be represented on a valid grid.")

    try:
        from xarray import DataArray
        # Importing wavespectra.core also initializes the package and registers
        # the DataArray accessor; no optional imports occur at module load.
        from wavespectra.core.attributes import attrs
    except ImportError as exc:
        raise MissingWavespectraDependencyError(
            "This adapter requires wavespectra and xarray; "
            "install them with 'pip install wavespectra xarray'."
        ) from exc

    return DataArray(
        frequency_density,
        dims=(attrs.FREQNAME,),
        coords={
            attrs.FREQNAME: (
                attrs.FREQNAME,
                frequency,
                dict(attrs.ATTRS[attrs.FREQNAME]),
            ),
        },
        name=attrs.SPEC1DNAME,
        attrs=dict(attrs.ATTRS[attrs.SPEC1DNAME]),
    )
