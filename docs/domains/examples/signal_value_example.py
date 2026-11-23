# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run the synthetic single-tone example without files or solver dependencies.

Sources: NumPy reference 1.26, Discrete Fourier Transform; SciPy user guide
1.14.1 (2024), Spectral Analysis. Links and hand derivations are in
docs/domains/signal_processing/synthetic-value-checks.md.
"""

import numpy as np

from digitalmodel.signal_processing.signal_analysis.core.spectral import (
    SpectralAnalyzer,
)


def run_example():
    """Return measured values for A=4, f=16 Hz, fs=256 Hz, N=256.

    T=N/fs=1 second and df=1/T=1 Hz. DFT orthogonality leaves
    coefficients of magnitude N*A/2=512 at the two tone bins.
    One-sided amplitude is 2*512/256=4, and mean-square=A²/2=8.
    A rectangular-window density has one nonzero bin of area 8.
    This example's constants define synthetic inputs, not a material limit.
    """
    samples = 4 * np.sin(2 * np.pi * 16 * np.arange(256) / 256)
    analyzer = SpectralAnalyzer(sampling_rate=256)
    spectrum = analyzer.compute_spectrum(samples, detrend="none")
    density = analyzer.compute_psd(samples, method="periodogram", window="boxcar")
    peak_index = spectrum.magnitude.idxmax()
    df = density.frequency.iloc[1] - density.frequency.iloc[0]
    return {
        "frequency_hz": float(spectrum.loc[peak_index, "frequency"]),
        "amplitude": float(spectrum.loc[peak_index, "magnitude"]),
        "resolution_hz": float(df),
        "mean_square": float(np.mean(samples**2)),
        "psd_area": float(density.psd.sum() * df),
    }


if __name__ == "__main__":
    for quantity, value in run_example().items():
        print(f"{quantity}: {value:.1f}")
