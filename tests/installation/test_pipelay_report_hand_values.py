# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic controls specified in the pipelay report issue.

Source: the pipelay module-report worked-example specification and the independent
derivations below. All inputs are synthetic, not standard criteria.
The relative tolerance is 32 binary64 epsilons: these positive, small calculations
require fewer than ten elementary operations without cancellation. No absolute
tolerance is needed for the nonzero reference values.
"""

import doctest
import sys
from dataclasses import asdict

import pytest

from digitalmodel.installation.pipelay import integrity, stress

ROUND_OFF = 32 * sys.float_info.epsilon


def test_combined_stress_matches_dimensional_hand_values():
    """Derive every field independently in N, mm and N/mm².

    From the synthetic issue example: epsilon = (200 mm / 2) / 100000 mm
    = 1/1000; sigma_b = 200000 N/mm² * 1/1000 = 200 N/mm².
    sigma_a = 20000 N / 1000 mm² = 20 N/mm²; total = 220 N/mm².
    Supplied strength 400 N/mm² times supplied fraction 3/4 gives 300 N/mm².
    Utilisation = 220/300 = 11/15; strength percentage = 220/400 * 100 = 55.
    The supplied fraction is an arithmetic control, not a standard allowable.
    """
    assert integrity.bending_strain_from_radius(200, 100000) == pytest.approx(
        1 / 1000, rel=ROUND_OFF, abs=0
    )
    assert stress.bending_stress_from_radius(200, 100000, 200000) == pytest.approx(
        200, rel=ROUND_OFF, abs=0
    )
    assert stress.axial_stress(20000, 1000) == pytest.approx(
        20, rel=ROUND_OFF, abs=0
    )
    result = stress.smys_stress_check(
        200, 100000, 200000, 400, 3 / 4, axial_force=20000, steel_area=1000
    )
    values = asdict(result)
    assert values.pop("passes") is True  # 220 N/mm² <= 300 N/mm².
    assert values == pytest.approx(
        {"longitudinal_stress": 220, "allowable_stress": 300,
         "utilisation": 11 / 15, "percent_smys": 55},
        rel=ROUND_OFF, abs=0,
    )


def test_route_radius_matches_force_equilibrium():
    """F/R = mu*w: 100000 N / ((1/2)*1000 N/m) = 200 m.

    Source: the separate synthetic force-equilibrium example in the issue
    specification. N/(N/m) reduces to m; the stress example's mm are not reused.
    The same roundoff bound applies to the two products/divisions here.
    """
    assert integrity.minimum_route_radius(100000, 1 / 2, 1000) == pytest.approx(
        200, rel=ROUND_OFF, abs=0
    )


def test_stress_documentation_executes_consistent_unit_examples():
    """Require executable N/mm² and N/m² examples instead of mixed units.

    The example inputs and results are the independent synthetic stress control
    above. The SI length relation (m = 1000 mm) implies m² = 1000000 mm².
    """
    result = doctest.testmod(stress)
    assert result.failed == 0
    assert result.attempted >= 2
    assert "N/mm^2" in stress.__doc__
    assert "N/m^2" in stress.__doc__
