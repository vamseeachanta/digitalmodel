# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Anonymized real riser-inspection fixtures exercise the canonical FFS chain (#1293).

The grids are unmodified UT C-scan measurements (mm) from a real drilling-riser
baseline inspection; the registers are the campaign's per-scan GML results and the
weld flaw-register schema. See test_data/real_inspection/README.md for provenance.
"""

from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
    FFSComponent,
    assess_component,
)
from digitalmodel.asset_integrity.assessment.grid_parser import GridParser

DATA = Path(__file__).parent / "test_data" / "real_inspection"

NOMINAL_OD_IN = 21.25
NOMINAL_WT_IN = 0.875  # 22.225 mm

GRIDS = {
    "severe_ds8": ("ut_grid_RJ-101-boxend_ds8.csv", 375),
    "severe_worstzone": ("ut_grid_RJ-101-boxend_worstzone_fullres.csv", 500),
    "moderate_ds8": ("ut_grid_RJ-102-boxend_ds8.csv", 407),
    "mild_ds8": ("ut_grid_RJ-103-pinend_ds8.csv", 375),
}


@pytest.mark.parametrize("name", GRIDS)
def test_grid_parses_to_physical_thickness(name):
    # A03: parsing cannot silently exclude gaps in the original measurement grid.
    fname, n_axial = GRIDS[name]
    raw = pd.read_csv(DATA / fname, header=None)
    assert raw.shape == (n_axial, 64)
    assert raw.isna().to_numpy().any()
    with pytest.raises(ValueError, match="complete coverage"):
        GridParser.from_csv(str(DATA / fname), input_units="mm")


def test_severe_worstzone_runs_canonical_ffs_chain():
    # A03: missing readings withhold component clearance before routing.
    fname, _ = GRIDS["severe_worstzone"]
    component = FFSComponent("component", "B31.8", NOMINAL_OD_IN, NOMINAL_WT_IN,
                             500.0, 80000.0, corrosion_rate_in_per_yr=0.25 / 25.4)
    with pytest.raises(ValueError, match="complete coverage"):
        assess_component(component, DATA / fname, input_units="mm")


def test_gml_results_register_schema_and_ordering():
    reg = pd.read_csv(DATA / "gml_results_register.csv")
    assert len(reg) == 142
    assert set(reg["component"]) == {"Main", "Choke", "Kill", "MB"}
    assert reg["joint_id"].str.match(r"RJ-\d+").all()
    # severity ordering of the three grid joints (box/pin scans picked per README)
    main = reg[reg["component"] == "Main"]
    severe = main[(main["joint_id"] == "RJ-101") & (main["scan_location"] == "Box End")]
    mild = main[(main["joint_id"] == "RJ-103") & (main["scan_location"] == "Pin End")]
    assert severe["min_life_years"].min() < 5.0
    assert mild["min_life_years"].min() > 25.0
    # required-minimum WT is a constant collapse limit per component
    assert main["min_required_wt_mm"].nunique() == 1


def test_flaw_register_schema():
    flaws = pd.read_csv(DATA / "flaw_register.csv")
    assert list(flaws.columns) == [
        "joint_id", "weld", "flaw_length_mm", "flaw_depth_mm", "flaw_type", "zones",
    ]
    assert len(flaws) == 3
    assert set(flaws["flaw_type"]) == {"ext", "emb", "int"}
    assert (flaws["flaw_depth_mm"] < flaws["flaw_length_mm"]).all()
