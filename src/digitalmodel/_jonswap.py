# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Frequency-grid-independent JONSWAP energy normalisation.

Shape coefficients: SIMA public documentation, SIMO / Modelling / Environment /
Waves / JONSWAP Spectra, section 1 (accessed 2026-10-22):
https://simasite.azurewebsites.net/docs/latest/simo/modelling/environment/waves/Jonswap.html

Derivation and numerical bounds: docs/domains/installation/jonswap-normalisation.md.
"""

from functools import lru_cache
import math

import numpy as np

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


@lru_cache
def normalising_factor(gamma: float) -> float:
    """Return the multiplier that preserves Hs²/16 for the full spectrum.

    For x=omega/wp, q(x)=5*x^-5*exp(-5/(4*x^4)) has integral 1.
    Hence C_gamma = 1 / (1 + integral(q*(gamma**a-1))). Integrating
    only the enhancement excess preserves the analytical unenhanced tail exactly.
    The caller's frequency grid is never used to normalise the spectrum.

    Numerical qualification covers 1 <= gamma <= 7 (issue #2543).
    """
    # Numerical mesh: step=1/2048, with the sigma transition at a node.
    # On [0.5, 2], discarded enhancement is below 2e-11 for gamma<=7;
    # see the derivation and independent Simpson check in the basis document.
    x = np.linspace(0.5, 2.0, 3073)
    # SIMA section 1: sigma_a=0.07, sigma_b=0.09, beta=5/4.
    sigma = np.where(x <= 1.0, 0.07, 0.09)
    a = np.exp(-0.5 * ((x - 1.0) / sigma) ** 2)
    q = 5.0 * x**-5 * np.exp(-1.25 / x**4)
    excess = q * np.expm1(math.log(gamma) * a)
    return float(1.0 / (1.0 + _trapezoid(excess, x)))
