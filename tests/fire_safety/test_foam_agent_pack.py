# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Hand anchors: public FSS resolution MSC.98(73) (2000), Ch.14 §2.2.1.

The selected library row records the 2015 consolidated edition, §2.2.2.2.
Only the original resolution's rate and inert-gas duration were read publicly.
These are arithmetic regressions, not approval of that later edition.
"""

import pytest

from digitalmodel.foam_system.criteria_library import get_criterion
from digitalmodel.foam_system.sizing import (
    ProtectedArea,
    concentrate_quantity,
    foam_demand,
)


def _area():
    # Synthetic area; rate/duration: cited FSS §2.2.1.1.2 / §2.2.1.2.
    return ProtectedArea(
        "synthetic", 100, get_criterion("fss_deck_foam_largest_tank_section")
    )


def test_library_solution_rate():
    """100 m² × 6 L/(min m²) = 600 L/min, FSS §2.2.1.1.2 (2000)."""
    assert _area().solution_flow_lpm == pytest.approx(600)


def test_library_solution_volume():
    """600 L/min × 20 min = 12000 L, inert-gas case, FSS §2.2.1.2 (2000)."""
    assert _area().solution_volume_l == pytest.approx(12000)


def test_synthetic_concentrate_policy():
    """12000 × (3/100) × (1+50/100) = 540 L; 600 × 3/100 = 18 L/min.

    Concentration and reserve are synthetic caller inputs, not standard limits.
    Volume basis: FSS §2.2.1.1.2 / §2.2.1.2 (2000), as above.
    """
    result = concentrate_quantity(foam_demand([_area()]), 3, 50)
    assert result.base_concentrate_l == pytest.approx(360)
    assert result.total_concentrate_l == pytest.approx(540)
    assert result.injection_rate_lpm == pytest.approx(18)
