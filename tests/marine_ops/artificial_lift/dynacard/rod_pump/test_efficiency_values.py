# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent synthetic volume-balance hand check for pump efficiency.

All numerical inputs are synthetic choices. A day contains 24 hours by
definition. Expected displacement is supplied directly, without a pumping
correlation or a production conversion constant.
"""

from digitalmodel.marine_ops.artificial_lift.dynacard.rod_pump import (
    volumetric_efficiency,
)


def test_efficiency_accounts_for_runtime_and_volume_factor():
    """Balance produced reservoir volume against duty-adjusted displacement.

    Choose surface production 40 bbl/day, theoretical displacement
    200 bbl/day, runtime 12 h/day and reservoir-volume factor 1.25.
    Produced reservoir volume is 40*1.25=50 bbl/day; duty-adjusted
    displacement is 200*(12/24)=100 bbl/day, giving 50/100=0.5.
    Full-day runtime gives 50/200=0.25. These binary-exact fractions need
    no rounding tolerance. Missing runtime or volume factor is undetermined
    under the interface; zero or synthetic -1 runtime/displacement cannot
    provide a positive denominator and must also return None.
    """
    assert volumetric_efficiency(40.0, 200.0, 12.0, 1.25) == 0.5
    assert volumetric_efficiency(40.0, 200.0, 24.0, 1.25) == 0.25

    assert volumetric_efficiency(40.0, 200.0) is None
    assert volumetric_efficiency(40.0, 200.0, formation_volume_factor=1.25) is None
    assert volumetric_efficiency(40.0, 200.0, runtime_hours_per_day=12.0) is None
    for displacement, runtime, volume_factor in [
        (200.0, None, 1.25),
        (200.0, 12.0, None),
        (200.0, 0.0, 1.25),
        (200.0, -1.0, 1.25),
        (0.0, 12.0, 1.25),
        (-1.0, 12.0, 1.25),
    ]:
        assert (
            volumetric_efficiency(40.0, displacement, runtime, volume_factor) is None
        )
