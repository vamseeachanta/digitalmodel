# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic basis from issue 3308, synthetic examples only.

Expected results are hand addition, an interval average and state enumeration.
Constructor controls are synthetic software controls, not a design basis.
"""

from dataclasses import replace

import pytest

from digitalmodel.structural.offshore_resilience.installation_checklist import (
    ChecklistItem,
    ChecklistStatus,
    InstallationChecklist,
)
from digitalmodel.structural.offshore_resilience.minimum_facility import (
    InstallationMethod,
    MinimumFacilityPlatform,
)

# Absolute allowance for one floating-point division; counts remain exact.
# IEEE binary64 roundoff is far below this dimensionless arithmetic allowance.
RATIO_ABS_TOLERANCE = 1e-15


def test_facility_hand_addition_midpoint_and_inclusive_capacity():
    """Hand basis: 120 + 80 = 200 t; (100 + 300) / 2 = 200 ft."""
    facility = MinimumFacilityPlatform(
        platform_type="wellhead",  # Synthetic constructor control.
        topsides_weight_tonnes=120,
        jacket_weight_tonnes=80,
        water_depth_range_ft=(100, 300),
        installation_method=InstallationMethod.CRANE_BARGE,  # Control only.
        max_crane_capacity_tonnes=200,
        well_slots=1,  # Synthetic constructor control, not a design basis.
        design_life_years=1,  # Synthetic constructor control, not a design basis.
        expansion_capability=False,  # Synthetic constructor control.
    )
    assert facility.total_weight_tonnes == 200
    assert facility.water_depth_midpoint_ft == 200
    assert facility.crane_installable is True  # Inclusive: 200 <= 200.
    assert replace(facility, max_crane_capacity_tonnes=199).crane_installable is False


def test_checklist_hand_enumeration_and_passed_fraction():
    """Hand basis: [pass, fail, pending]; passed fraction is 1/3, not 2/3."""
    items = [
        ChecklistItem("synthetic", key, "Synthetic interval control", 2, 4, "m")
        for key in ("check_a", "check_b", "check_c")
    ]
    checklist = InstallationChecklist("Synthetic interval checks", items)
    items[0].set_value(2)
    items[1].set_value(5)
    assert [item.status for item in items] == [
        ChecklistStatus.PASS, ChecklistStatus.FAIL, ChecklistStatus.PENDING,
    ]
    assert checklist.summary() == {"total": 3, "pass": 1, "fail": 1, "pending": 1}
    assert (checklist.pass_count, checklist.fail_count, checklist.pending_count) == (1, 1, 1)
    assert checklist.completion_ratio == pytest.approx(
        1 / 3, rel=0, abs=RATIO_ABS_TOLERANCE,
    )
    assert checklist.is_complete is False

    # Both supplied bounds are inclusive: 2 <= 2, 3, 4 <= 4.
    for item, value in zip(items, (2, 3, 4), strict=True):
        item.set_value(value)
    assert [item.status for item in items] == [ChecklistStatus.PASS] * 3
    assert checklist.summary() == {"total": 3, "pass": 3, "fail": 0, "pending": 0}
    assert (checklist.pass_count, checklist.fail_count, checklist.pending_count) == (3, 0, 0)
    assert checklist.completion_ratio == 1
    assert checklist.is_complete is True
