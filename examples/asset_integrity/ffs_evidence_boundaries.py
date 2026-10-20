# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic FFS evidence example; no external or measured-component data.

All inputs are chosen for hand arithmetic. Cell length = row count * pitch;
centroid spacing assumes equal axial and circumferential pitch. The existing
pressure-scale relation is identified by ffs_decision.reduced_mawp as
API 579-1/ASME FFS-1 (2021), section 2.4.2.2. No new standard coefficient or
acceptance limit is introduced. This is a software example, not qualification.
"""
import json

import pandas as pd

from digitalmodel.asset_integrity.assessment.ffs_decision import decide, reduced_mawp
from digitalmodel.asset_integrity.assessment.pitting import (
    assess_pitting_level2_equivalent_lta,
    characterize_pit_field,
)
from digitalmodel.units import Q_


def run_example():
    """Return observations and arithmetic answers, keeping absent life null."""
    pit_free = characterize_pit_field(pd.DataFrame([[0.48, 0.49]]), 0.5)
    single_row = characterize_pit_field(
        pd.DataFrame([[0.3, 0.5, 0.3]]), 0.5, row_spacing_in=0.4)
    equivalent = assess_pitting_level2_equivalent_lta(
        single_row, nominal_od_in=16, nominal_wt_in=0.5, t_min_in=0.35)
    # Independent hand arithmetic: 1000 * (0.72 / 0.9) = 800 psi.
    # RSF and allowable here are caller inputs, not derived from the pit grid.
    pressure = decide(0.72, 0.9, None, "pressure",
                      derate=reduced_mawp(Q_(1000, "psi"))).to_dict()
    return {
        "pit_free_observed_min_in": pit_free.measured_min_in,
        "pit_free_pit_min_in": pit_free.t_min_pit_in,
        "axial_extent_in": equivalent["flaw_length_in"],
        "pit_spacing_in": single_row.pit_spacing_min_in,
        "calculated_rating_psi": pressure["rerated_mawp_psi"],
        "remaining_life_yr": pressure["remaining_life_yr"],
    }


if __name__ == "__main__":
    print(json.dumps(run_example(), indent=2, allow_nan=False))
