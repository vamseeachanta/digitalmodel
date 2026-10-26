# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Workflow-level absolute CAPEX, provenance and screening verdict coverage."""

import json
from pathlib import Path

import pytest

from digitalmodel.field_development.registry_workflows import (
    FieldDevelopmentRegistryWorkflow,
)


@pytest.fixture()
def cfg(tmp_path):
    return {
        "basename": "concept_screening",
        "_config_dir_path": str(tmp_path),
        "concept_screening": {
            "prospect": {
                "name": "Synthetic analogue",
                "water_depth_m": 1500,
                "well_count": 8,
                "reservoir_size_mmbbl": 300,
                "production_capacity_bopd": 120000,
                "distance_to_infra_km": 80,
            },
            "outputs": {"directory": "results", "summary_json": "summary.json"},
        },
    }


def run(cfg):
    block = FieldDevelopmentRegistryWorkflow().router(cfg)["concept_screening"]
    return json.loads(Path(block["summary_json"]).read_text())


@pytest.mark.parametrize("key", ["Spar", "spar", "SPAR"])
def test_partial_override_normalized_and_echoed(cfg, key):
    raw = {key: 0.5}
    cfg["concept_screening"]["capex_override"] = raw
    summary = run(cfg)
    assert summary["capex_mode"] == "absolute_override_partial"
    assert summary["capex_override_input"] == raw
    assert summary["screening_status"] in {"pass", "review", "fail"}
    for row in summary["ranked"]:
        expected = "absolute_override" if row["host_type"] == "Spar" else "derived_fraction_of_base"
        assert row["capex_source"] == expected
        assert all(isinstance(v, (int, float)) for v in row["capex_line_items_usd_bn"].values())
        if row["host_type"] == "Spar":
            assert row["capex_total_usd_bn"] == pytest.approx(0.5)


def test_unknown_override_key_raises(cfg):
    cfg["concept_screening"]["capex_override"] = {"unknown": 0.5}
    with pytest.raises(ValueError, match="Unsupported host_type"):
        run(cfg)


def test_default_summary_and_repeat_determinism(cfg):
    first = run(cfg)
    note = Path(first["note_md"]).read_bytes()
    assert first["capex_mode"] == "derived"
    assert first["capex_override_input"] is None
    assert first == run(cfg)
    assert note == Path(first["note_md"]).read_bytes()


@pytest.mark.parametrize("candidates", [None, ["Spar", "Semi"]])
def test_status_matches_direct_sweep_and_gates(cfg, candidates):
    from digitalmodel.field_development.concept_comparison_note import weight_sensitivity_sweep
    from digitalmodel.field_development.concept_screening import ProspectSpec

    params = cfg["concept_screening"]
    params["candidates"] = candidates
    params["capex_override"] = {"Spar": 0.5}
    summary = run(cfg)
    sweep = weight_sensitivity_sweep(ProspectSpec(**params["prospect"]), capex_override={"Spar": 0.5})
    ranked = summary["ranked"]
    expected = "fail" if all(row["gated"] for row in ranked) else (
        "review" if ranked[0]["gated"] or len({row.winner for row in sweep}) > 1 else "pass"
    )
    assert summary["screening_status"] == expected


def test_full_override_relative_to_screened_candidates(cfg):
    params = cfg["concept_screening"]
    params["candidates"] = ["Spar", "Semi"]
    params["capex_override"] = {"Spar": 0.5, "Semi": 9.0}
    summary = run(cfg)
    assert summary["capex_mode"] == "absolute_override_full"
    assert all(row["capex_source"] == "absolute_override" for row in summary["ranked"])


def test_all_gated_status_fails(cfg):
    cfg["concept_screening"]["prospect"]["water_depth_m"] = 10000
    summary = run(cfg)
    assert all(row["gated"] for row in summary["ranked"])
    assert summary["screening_status"] == "fail"


# Numerical catalogue inputs are synthetic software-test fixtures.
pytestmark = pytest.mark.usefixtures("synthetic_material_catalog")
