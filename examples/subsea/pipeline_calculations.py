# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic wave and current examples; run with the repository source on PYTHONPATH.

Methods, units, sources and independent hand answers are recorded in
docs/domains/pipelines/calculation-test-basis.html. These examples exercise
arithmetic; no site-specific design or return-period conversion is implied.
"""

import json
import math

import numpy as np

from digitalmodel.subsea.pipeline.free_span.wave_velocity import (
    solve_dispersion,
    velocity_transfer,
)
from digitalmodel.subsea.pipeline.free_span.weibull_current import extrapolate_current


def worked_example() -> dict:
    """Return answers for three independently checked synthetic cases.

    Dispersion: h=10 m and omega=sqrt(g/10) rad/s give x*tanh(x)=1,
    x=k*h; hand bisection gives k=0.119968 /m to six decimal places.
    Transfer: omega=2 rad/s, k=.1 /m, h=10 m, z'=5 m give 1.9190347513 /s.
    Weibull: shape 2, scale 1.5 m/s; survival pairs (exp(-.25),.75) and
    (exp(-4),3) give speeds .1503770450 and 3.2189490394 m/s at CDF .01,.99.
    References: Coastal Dynamics (2021), Appendix A, Table A.2; NIST
    e-Handbook section 1.3.6.6.8, inverse survival function.
    """
    g = 9.80665  # m/s²; NIST SP 811 (2008), Appendix B.8, standard free fall.
    wave_number = solve_dispersion(np.array([math.sqrt(g / 10.0)]), 10.0)[0]
    transfer = velocity_transfer(np.array([2.0]), np.array([0.1]), 10.0, 5.0)[0]
    currents = extrapolate_current(
        [math.exp(-0.25), math.exp(-4.0)], [0.75, 3.0], [0.99, 0.01]
    )
    return {
        "wave_number_per_m": float(wave_number),
        "velocity_transfer_per_s": float(transfer),
        "current_speed_m_s": currents,
    }


if __name__ == "__main__":
    print(json.dumps(worked_example(), indent=2))
