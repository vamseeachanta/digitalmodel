# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Fresh-process fixture for a constant spectrum with no legacy NumPy API."""

import sys

import numpy as np


def main():
    """Remove the legacy API before importing and evaluating wave_spectra."""
    module_name = "digitalmodel.hydrodynamics.wave_spectra"
    assert module_name not in sys.modules, "wave_spectra was imported too early"

    # NumPy 1.x has only the historical spelling. Expose that implementation
    # under the supported spelling to exercise the same API-absence scenario.
    if not hasattr(np, "trapezoid"):
        setattr(np, "trapezoid", getattr(np, "trapz"))
    if hasattr(np, "trapz"):
        delattr(np, "trapz")
    # Keep NumPy 1.x's export list consistent with the removed attribute:
    # unrelated dependency imports may execute `from numpy import *`.
    if "trapz" in np.__all__:
        np.__all__.remove("trapz")
    assert not hasattr(np, "trapz")
    assert callable(np.trapezoid)

    from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra

    # Synthetic nonuniform grid and S=2: the independent hand integrals are
    # integral(2 dw, 1, 4)=6 and integral(2w dw, 1, 4)=4**2-1**2=15.
    # The interior nodes 1.5 and 3 distinguish unequal interval widths.
    frequencies = np.array([1.0, 1.5, 3.0, 4.0])
    spectrum = np.full_like(frequencies, 2.0)
    print(WaveSpectra().spectral_moment(frequencies, spectrum, n=int(sys.argv[1])))


if __name__ == "__main__":
    main()
