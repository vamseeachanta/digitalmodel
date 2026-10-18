# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Reference-data provenance and rigid-body aggregation regressions."""
from pathlib import Path

import numpy as np
import pytest
import yaml

ROOT = Path(__file__).resolve().parents[3]
BASIS = ROOT / "docs/domains/orcaflex/wind/fowt-volturnus/basis.yml"


def test_all_numerical_basis_entries_have_units_and_source():
    def visit(node):
        if isinstance(node, dict):
            if "value" in node:
                assert node.get("source")
                assert node.get("unit")
            else:
                for child in node.values():
                    visit(child)
    visit(yaml.safe_load(BASIS.read_text()))


def test_both_inertia_readings_are_retained_as_assumptions():
    basis = yaml.safe_load(BASIS.read_text())
    assert basis["qualification"]["use_for_analysis"] == "assumed"
    assert set(basis["system"]["inertia_readings"]) == {"at_cg", "at_tower_top"}


def test_parallel_axis_and_aggregate_mass():
    from digitalmodel.marine_ops.fowt_quasi_static.basis import aggregate, parallel_axis
    np.testing.assert_allclose(parallel_axis(2, [1, 0, 0]), np.diag([0, 2, 2]))
    basis = yaml.safe_load(BASIS.read_text())
    first = aggregate(basis, "at_cg")
    second = aggregate(basis, "at_tower_top")
    assert first["mass"] == pytest.approx(basis["system"]["mass"]["value"], rel=1e-12)
    assert first["mass"] == second["mass"]
    np.testing.assert_allclose(first["cg"], second["cg"])
    assert np.linalg.eigvalsh(second["inertia_swl"]).min() > 0
    assert not np.allclose(first["inertia_swl"], second["inertia_swl"])


def test_canonical_tensor_matches_selected_reading_and_saved_alternatives():
    from digitalmodel.marine_ops.fowt_quasi_static.basis import aggregate, parallel_axis
    basis = yaml.safe_load(BASIS.read_text())
    system = basis["system"]
    selected = aggregate(basis, system["adopted_reading"])
    np.testing.assert_allclose(system["inertia_swl"]["value"], selected["inertia_swl"], rtol=1e-12)
    np.testing.assert_allclose(system["inertia_cg"]["value"], selected["inertia_cg"], rtol=1e-12)
    first = aggregate(basis, "at_cg")
    second = aggregate(basis, "at_tower_top")
    relative = np.array(system["rna_center_of_gravity"]["value"]) - [0, 0, system["tower_top"]["value"]]
    np.testing.assert_allclose(first["inertia_swl"]-second["inertia_swl"],
                               parallel_axis(system["rna_mass"]["value"], relative), atol=1e-5)
    for reading in ("at_cg", "at_tower_top"):
        np.testing.assert_allclose(system["inertia_readings"][reading]["value"],
                                   aggregate(basis, reading)["inertia_swl"], rtol=1e-12)
