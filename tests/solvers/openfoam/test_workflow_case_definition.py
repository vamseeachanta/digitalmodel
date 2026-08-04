"""Propagation tests for the OpenFOAM generic and batch adapters (#1575).

These assert the leaf actually reaches the rendered case through the adapter,
not merely that the adapter accepted the request. A mock batch passing while a
default static case is rendered is the defect under test, so every assertion
reads an emitted dictionary.

Imports OpenFOAMWorkflow directly rather than through the digitalmodel engine,
because the engine import chain pulls optional solver dependencies that are not
installed on every host.
"""

from __future__ import annotations

import copy
from pathlib import Path
from typing import Any, Dict

import pytest

from digitalmodel.solvers.openfoam.case_definition import CaseDefinitionError
from digitalmodel.solvers.openfoam.workflow import OpenFOAMWorkflow


CANONICAL_SETTINGS: Dict[str, Any] = {
    "operation": "build_case",
    "case_definition": {
        "schema_version": 1,
        "kind": "authored",
        "authored": {
            "case_type": "sloshing",
            "name": "synthetic_sloshing",
            "solver": "interFoam",
            "domain": {
                "min_coords_m": [0.0, 0.0, 0.0],
                "max_coords_m": [2.0, 1.0, 1.0],
                "n_cells": [20, 10, 10],
            },
            "motion": {
                "type": "roll",
                "amplitude": 3.0,
                "amplitude_unit": "deg",
                "period_s": 1.5,
                "origin_m": [1.0, 0.5, 0.0],
            },
            "fill": {"level": 0.4},
            "time": {
                "start_time_s": 0.0,
                "end_time_s": 2.0,
                "delta_t_s": 0.002,
                "write_interval_steps": 25,
                "adjustable_time_step": True,
                "max_co": 0.5,
                "purge_write": 0,
            },
            "function_objects": {
                "pressure_taps": [
                    {
                        "name": "synthetic_tap",
                        "location_m": [1.0, 0.5, 0.8],
                        "fields": ["p", "p_rgh"],
                    }
                ],
                "write_control": "timeStep",
                "write_interval": 1,
            },
        },
    },
    "execution": {
        "mesh_utility": "blockMesh",
        "run_snappy": False,
        "run_set_fields": True,
        "to_vtk": False,
        "timeout_seconds": 43200,
        "dry_run": False,
    },
}


def _route(settings: Dict[str, Any], tmp_path: Path) -> Path:
    cfg = {
        "basename": "openfoam",
        "Analysis": {"result_folder": str(tmp_path)},
        "openfoam": copy.deepcopy(settings),
    }
    OpenFOAMWorkflow().router(cfg)
    return Path(cfg["openfoam"]["case_dir"])


def _control_dict(case_dir: Path) -> Dict[str, str]:
    entries: Dict[str, str] = {}
    for line in (case_dir / "system" / "controlDict").read_text().splitlines():
        stripped = line.strip()
        if not stripped.endswith(";") or stripped.startswith("//"):
            continue
        parts = stripped[:-1].split(None, 1)
        if len(parts) == 2:
            entries.setdefault(parts[0], parts[1].strip())
    return entries


# --------------------------------------------------------------------------- #
#  the five leaves the issue reports as dropped                                #
# --------------------------------------------------------------------------- #


def test_router_propagates_requested_domain(tmp_path: Path) -> None:
    case_dir = _route(CANONICAL_SETTINGS, tmp_path)
    block_mesh = (case_dir / "system" / "blockMeshDict").read_text()
    assert "hex (0 1 2 3 4 5 6 7) (20 10 10) simpleGrading (1 1 1)" in block_mesh


def test_router_propagates_requested_time(tmp_path: Path) -> None:
    case_dir = _route(CANONICAL_SETTINGS, tmp_path)
    assert _control_dict(case_dir)["endTime"] == "2.0"


def test_router_emits_dynamic_mesh_dict(tmp_path: Path) -> None:
    case_dir = _route(CANONICAL_SETTINGS, tmp_path)
    assert (case_dir / "constant" / "dynamicMeshDict").is_file()


def test_router_emits_set_fields_dict(tmp_path: Path) -> None:
    case_dir = _route(CANONICAL_SETTINGS, tmp_path)
    assert (case_dir / "system" / "setFieldsDict").is_file()


def test_router_emits_function_objects(tmp_path: Path) -> None:
    case_dir = _route(CANONICAL_SETTINGS, tmp_path)
    assert "synthetic_tap" in (case_dir / "system" / "controlDict").read_text()


# --------------------------------------------------------------------------- #
#  fail closed rather than silently rendering a default case                   #
# --------------------------------------------------------------------------- #


def test_router_rejects_unknown_authored_key(tmp_path: Path) -> None:
    settings = copy.deepcopy(CANONICAL_SETTINGS)
    settings["case_definition"]["authored"]["turbulence"] = "kOmegaSST"
    with pytest.raises(CaseDefinitionError, match="turbulence"):
        _route(settings, tmp_path)


def test_router_rejects_semantic_keys_at_the_legacy_root(tmp_path: Path) -> None:
    # The pre-#1575 router silently accepted and dropped these, rendering a
    # default static case. They must now be refused by name.
    settings = {
        "operation": "build_case",
        "case_type": "sloshing",
        "name": "synthetic_sloshing",
        "solver": "interFoam",
        "domain": {"min_coords_m": [0.0, 0.0, 0.0],
                   "max_coords_m": [2.0, 1.0, 1.0],
                   "n_cells": [20, 10, 10]},
    }
    with pytest.raises(CaseDefinitionError, match="domain"):
        _route(settings, tmp_path)


def test_router_rejects_case_definition_without_execution(tmp_path: Path) -> None:
    settings = copy.deepcopy(CANONICAL_SETTINGS)
    settings.pop("execution")
    with pytest.raises(CaseDefinitionError, match="execution"):
        _route(settings, tmp_path)


# --------------------------------------------------------------------------- #
#  unconsumed function-object controls                                         #
# --------------------------------------------------------------------------- #


def test_empty_taps_reject_a_non_default_write_control(tmp_path: Path) -> None:
    # With no taps there is nothing to carry write_control, so accepting a
    # non-default value would silently drop it.
    settings = copy.deepcopy(CANONICAL_SETTINGS)
    settings["case_definition"]["authored"]["function_objects"] = {
        "pressure_taps": [],
        "write_control": "runTime",
        "write_interval": 1,
    }
    with pytest.raises(CaseDefinitionError, match="write_control"):
        _route(settings, tmp_path)


def test_empty_taps_reject_a_non_default_write_interval(tmp_path: Path) -> None:
    settings = copy.deepcopy(CANONICAL_SETTINGS)
    settings["case_definition"]["authored"]["function_objects"] = {
        "pressure_taps": [],
        "write_control": "timeStep",
        "write_interval": 9,
    }
    with pytest.raises(CaseDefinitionError, match="write_interval"):
        _route(settings, tmp_path)


def test_empty_taps_accept_the_default_controls(tmp_path: Path) -> None:
    settings = copy.deepcopy(CANONICAL_SETTINGS)
    settings["case_definition"]["authored"]["function_objects"] = {
        "pressure_taps": [],
        "write_control": "timeStep",
        "write_interval": 1,
    }
    case_dir = _route(settings, tmp_path)
    assert "functions" not in (case_dir / "system" / "controlDict").read_text()


# --------------------------------------------------------------------------- #
#  legacy generic requests keep working                                        #
# --------------------------------------------------------------------------- #


LEGACY_SETTINGS = {
    "operation": "build_case",
    "case_type": "current_loading",
    "name": "legacy_case",
    "solver": "simpleFoam",
    "mesh_utility": "blockMesh",
    "to_vtk": True,
    "dry_run": False,
}


def test_legacy_router_request_still_builds(tmp_path: Path) -> None:
    case_dir = _route(LEGACY_SETTINGS, tmp_path)
    assert case_dir.name == "legacy_case"


def test_legacy_router_request_renders_unchanged_block_mesh(tmp_path: Path) -> None:
    from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
    from digitalmodel.solvers.openfoam.models import CaseType, OpenFOAMCase

    expected_dir = OpenFOAMCaseBuilder(
        OpenFOAMCase.for_case_type(CaseType.CURRENT_LOADING, "legacy_case")
    ).build(tmp_path / "expected")
    actual_dir = _route(LEGACY_SETTINGS, tmp_path / "actual")
    expected = (expected_dir / "system" / "blockMeshDict").read_bytes()
    assert (actual_dir / "system" / "blockMeshDict").read_bytes() == expected


def test_legacy_router_defaults_case_name_from_case_type(tmp_path: Path) -> None:
    settings = {"operation": "build_case", "case_type": "current_loading"}
    assert _route(settings, tmp_path).name == "current_loading_case"


# --------------------------------------------------------------------------- #
#  batch adapter carries the canonical contract                                #
# --------------------------------------------------------------------------- #


def test_batch_renders_canonical_base_into_each_case(tmp_path: Path) -> None:
    from digitalmodel.workflows.openfoam_batch_config import render_cases

    base = copy.deepcopy(CANONICAL_SETTINGS)
    rendered = render_cases(base, [{"name": "case_a"}], {}, tmp_path)
    authored = rendered[0]["settings"]["case_definition"]["authored"]
    assert authored["domain"]["n_cells"] == [20, 10, 10]


def test_batch_mapping_reaches_the_authored_leaf(tmp_path: Path) -> None:
    from digitalmodel.workflows.openfoam_batch_config import render_cases

    base = copy.deepcopy(CANONICAL_SETTINGS)
    rendered = render_cases(
        base,
        [{"name": "case_a", "end_time": 5.0}],
        {"end_time": "case_definition.authored.time.end_time_s"},
        tmp_path,
    )
    authored = rendered[0]["settings"]["case_definition"]["authored"]
    assert authored["time"]["end_time_s"] == 5.0


def test_batch_mapped_leaf_reaches_the_rendered_case(tmp_path: Path) -> None:
    from digitalmodel.workflows.openfoam_batch_config import render_cases

    base = copy.deepcopy(CANONICAL_SETTINGS)
    rendered = render_cases(
        base,
        [{"name": "case_a", "end_time": 5.0}],
        {"end_time": "case_definition.authored.time.end_time_s"},
        tmp_path,
    )
    case_dir = _route(rendered[0]["settings"], tmp_path / "built")
    assert _control_dict(case_dir)["endTime"] == "5.0"


def test_batch_rejects_a_mapping_target_that_is_not_an_accepted_leaf(
    tmp_path: Path,
) -> None:
    from digitalmodel.workflows.openfoam_batch_config import render_cases

    base = copy.deepcopy(CANONICAL_SETTINGS)
    with pytest.raises(ValueError, match="case_definition.authored.turbulence"):
        render_cases(
            base,
            [{"name": "case_a", "knob": 1.0}],
            {"knob": "case_definition.authored.turbulence"},
            tmp_path,
        )


def test_batch_rejects_a_mapping_target_naming_a_container(tmp_path: Path) -> None:
    from digitalmodel.workflows.openfoam_batch_config import render_cases

    base = copy.deepcopy(CANONICAL_SETTINGS)
    with pytest.raises(ValueError, match="case_definition.authored.domain"):
        render_cases(
            base,
            [{"name": "case_a", "knob": 1.0}],
            {"knob": "case_definition.authored.domain"},
            tmp_path,
        )


def test_batch_rejects_a_mapping_target_naming_the_discriminator(
    tmp_path: Path,
) -> None:
    from digitalmodel.workflows.openfoam_batch_config import render_cases

    base = copy.deepcopy(CANONICAL_SETTINGS)
    with pytest.raises(ValueError, match="case_definition.kind"):
        render_cases(
            base,
            [{"name": "case_a", "knob": "authored"}],
            {"knob": "case_definition.kind"},
            tmp_path,
        )


def test_batch_legacy_base_still_renders_unmapped_knobs(tmp_path: Path) -> None:
    from digitalmodel.workflows.openfoam_batch_config import render_cases

    base = {"case_type": "current_loading", "solver": "simpleFoam",
            "mesh_utility": "blockMesh", "to_vtk": False}
    rendered = render_cases(
        base, [{"name": "case_a", "solver_app": "pimpleFoam"}],
        {"solver_app": "solver"}, tmp_path,
    )
    assert rendered[0]["settings"]["solver"] == "pimpleFoam"


# --------------------------------------------------------------------------- #
#  the batch router accepts a canonical base end to end                        #
# --------------------------------------------------------------------------- #


def _canonical_batch_cfg(tmp_path: Path) -> Dict[str, Any]:
    return {
        "basename": "openfoam_run_batch",
        "_config_dir_path": str(tmp_path),
        "openfoam_run_batch": {
            "base": copy.deepcopy(
                {k: v for k, v in CANONICAL_SETTINGS.items() if k != "operation"}
            ),
            "cases": [{"name": "case_a", "end_time": 3.0},
                      {"name": "case_b", "end_time": 4.0}],
            "mapping": {"end_time": "case_definition.authored.time.end_time_s"},
            "run_batch": {"mode": "pool", "workers": 2, "mock": True},
        },
    }


def test_batch_router_runs_a_canonical_base_in_mock_mode(tmp_path: Path) -> None:
    from digitalmodel.workflows import openfoam_run_batch as ofb

    result = ofb.router(_canonical_batch_cfg(tmp_path))
    statuses = [row["status"] for row in result["openfoam_run_batch"]["cases"]]
    assert statuses == ["completed", "completed"]


def test_batch_router_swept_leaf_reaches_each_rendered_case(tmp_path: Path) -> None:
    from digitalmodel.workflows import openfoam_run_batch as ofb

    ofb.router(_canonical_batch_cfg(tmp_path))
    case_b = tmp_path / "batch_runs" / "case_b"
    assert _control_dict(case_b)["endTime"] == "4.0"


def test_batch_router_carries_motion_into_each_rendered_case(tmp_path: Path) -> None:
    from digitalmodel.workflows import openfoam_run_batch as ofb

    ofb.router(_canonical_batch_cfg(tmp_path))
    assert (tmp_path / "batch_runs" / "case_a" / "constant" / "dynamicMeshDict").is_file()


def test_canonical_base_mpi_plan_uses_the_authored_solver(tmp_path: Path) -> None:
    # The mpi plan is recorded even in mock mode, so it is the observable proof
    # that a canonical base reaches the dispatch layer rather than falling back
    # to flat-key defaults.
    from digitalmodel.workflows import openfoam_run_batch as ofb

    cfg = _canonical_batch_cfg(tmp_path)
    cfg["openfoam_run_batch"]["cases"] = [{"name": "case_a", "end_time": 3.0}]
    cfg["openfoam_run_batch"]["run_batch"] = {
        "mode": "mpi", "workers": 2, "mock": True,
    }
    result = ofb.router(cfg)
    plan = result["openfoam_run_batch"]["cases"][0]["mpi_plan"]
    assert "mpirun -np 2 --oversubscribe interFoam -parallel" in plan


def test_canonical_base_mpi_plan_includes_set_fields(tmp_path: Path) -> None:
    from digitalmodel.workflows import openfoam_run_batch as ofb

    cfg = _canonical_batch_cfg(tmp_path)
    cfg["openfoam_run_batch"]["cases"] = [{"name": "case_a", "end_time": 3.0}]
    cfg["openfoam_run_batch"]["run_batch"] = {
        "mode": "mpi", "workers": 2, "mock": True,
    }
    result = ofb.router(cfg)
    assert "setFields" in result["openfoam_run_batch"]["cases"][0]["mpi_plan"]
