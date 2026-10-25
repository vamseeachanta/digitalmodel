# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Worked energy example; run with PYTHONPATH=src python <this file>.

Inputs are synthetic. Method, source and hand-calculated answers:
docs/domains/installation/jonswap-normalisation.md.
"""

import numpy as np

from digitalmodel.marine_ops.installation.crane_tip_motion import (
    _jonswap_spectrum,
    crane_tip_significant_motion,
)
from digitalmodel.subsea.pipeline.free_span.wave_velocity import jonswap_spectrum

_trapezoid = getattr(np, "trapezoid", None) or np.trapz


def run_example() -> list[dict[str, float]]:
    """Return both variances and unit-RAO motion for each requested gamma."""
    # Synthetic inputs: Hs=4 m, Tp=8 s, unit displacement RAO.
    # By hand: m0=Hs²/16=1 m² and significant single amplitude=Hs/2=2 m.
    hs, tp = 4.0, 8.0
    wp = 2 * np.pi / tp
    # Issue #2543 gamma cases and the qualified grid from the basis document.
    omega = wp * np.geomspace(0.01, 100.0, 20001)
    rows = []
    for gamma in (1.0, 3.3, 7.0):
        crane = _jonswap_spectrum(omega, hs, wp, gamma)
        free_span = jonswap_spectrum(omega, hs, tp, gamma)
        rows.append(
            {
                "gamma": gamma,
                "crane_variance_m2": float(_trapezoid(crane, omega)),
                "free_span_variance_m2": float(_trapezoid(free_span, omega)),
                "motion_m": float(
                    crane_tip_significant_motion(
                        np.ones_like(omega), hs, tp, omega, gamma=gamma
                    )
                ),
            }
        )
    return rows


if __name__ == "__main__":
    for row in run_example():
        print(
            f"gamma={row['gamma']:.1f}: "
            f"crane m0={row['crane_variance_m2']:.1f} m², "
            f"free-span m0={row['free_span_variance_m2']:.1f} m², "
            f"significant single-amplitude motion={row['motion_m']:.1f} m"
        )
