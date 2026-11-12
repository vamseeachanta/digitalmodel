"""NREL 5 MW / OC4-DeepCwind semi-submersible spec for the OrcaFlex generator.

Uses the same builder as the IEA 15 MW case with the OC4 names and mooring
coefficients. Sources are listed in
``docs/domains/orcaflex/wind/fowt-oc4/basis.yml`` and ``generator_data.yml``.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml

from digitalmodel.floating_wind.iea15_orcaflex_spec import ROOT, build_spec

FOLDER = ROOT / "docs" / "domains" / "orcaflex" / "wind" / "fowt-oc4"

OC4_DESIGN: dict[str, Any] = {
    "hull_name": "OC4-DeepCwind",
    "line_type_name": "Chain equivalent 76.6mm",
    # Normal Cd 1.1 and Ca 1.0 from Robertson et al. (2014) Table 5-1. Axial
    # drag and axial added mass are not published; they are set to zero.
    "mooring_coefficients": {"normal_drag": 1.1, "axial_drag": 0.0,
                             "normal_added_mass": 1.0, "axial_added_mass": 0.0},
    "description": "NREL 5 MW reference turbine on the OC4-DeepCwind "
                   "semi-submersible; rigid floating wind model",
    "source_note": (
        "basis.yml and generator_data.yml (OpenFAST r-test at {commit}; Robertson et al. "
        "2014 NREL/TP-5000-60601). Hull mass properties are whole-system values (rigid "
        "representation) aggregated from the report's platform with ballast and the "
        "ElastoDyn tower, nacelle and rotor. WAMIT 22, 44, 24 completed by three-fold "
        "symmetry. Mooring axial drag and axial added mass are not published and are set "
        "to zero; no minimum breaking load is published."
    ),
}


def load_sources(folder: Path = FOLDER) -> tuple[dict, dict]:
    basis = yaml.safe_load((folder / "basis.yml").read_text(encoding="utf-8"))
    data = yaml.safe_load((folder / "generator_data.yml").read_text(encoding="utf-8"))
    return basis, data


def build_oc4_spec(basis: dict, data: dict, **kwargs) -> dict:
    design = dict(OC4_DESIGN, hull_length=basis["geometry"]["hull_length"]["value"])
    kwargs.setdefault("name", "oc4_deepcwind_lc0_statics")
    return build_spec(basis, data, design=design, **kwargs)
