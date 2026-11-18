"""Shared constants and the synthetic flexible variant for the fowt model-type tests."""
from __future__ import annotations

import copy
from pathlib import Path

ROOT = Path(__file__).resolve().parents[4]
EXAMPLES = ROOT / "docs" / "domains" / "orcaflex" / "examples"
SPEC_FILE = EXAMPLES / "fowt" / "iea15-volturnus-s" / "spec.yml"
GOLDEN = Path(__file__).parent / "goldens" / "fowt_iea15_lc0"
K03 = EXAMPLES / "modular" / "K03" / "K03 15MW semi-sub FOWT" / "includes"
H01 = EXAMPLES / "modular" / "H01" / "H01 Chinese lantern" / "includes"


def _flexible(spec_dict: dict) -> dict:
    """Synthetic flexible variant: two tower sections and a lumped RNA.

    Section values are illustrative for routing tests only; they are not
    reference-design values.
    """
    fowt = copy.deepcopy(spec_dict["fowt"])
    fowt["representation"] = "flexible"
    fowt["hull"]["mass"] = 17838.0
    fowt["hull"]["centre_of_mass"] = [0.0, 0.0, -14.4]
    fowt["tower"] = {"base_elevation": 15.0, "target_segment_length": 5.0, "sections": [
        {"length": 60.0, "outer_diameter": 10.0, "mass_per_length": 20.0,
         "bending_stiffness": 1.0e9, "axial_stiffness": 1.0e8, "torsional_stiffness": 7.0e8,
         "poisson_ratio": 0.3},
        {"length": 69.386, "outer_diameter": 8.0, "mass_per_length": 10.0,
         "bending_stiffness": 5.0e8, "axial_stiffness": 6.0e7, "torsional_stiffness": 3.5e8,
         "poisson_ratio": 0.25}]}
    fowt["rna"] = {"mass": 945.77, "centre_of_mass_from_tower_top": [-7.32, -0.1, 4.6],
                   "inertia_about_centre_of_mass": [[4.0e5, 0, 0], [0, 3.4e5, 0],
                                                    [0, 0, 3.4e5]]}
    fowt["rotor_thrust"] = {"thrust": 2457.2, "hub_position": [-12.0313, 0.0, 150.0],
                            "shaft_tilt": 6.0}
    return fowt
