"""Contract tests for the OpenFOAM authored case-definition schema (#1575).

Every semantic leaf a caller authors must reach the rendered case. A mock batch
that renders a default static case while silently dropping domain, motion, fill,
time controls and function objects is the exact defect these tests exist to
prevent, so the assertions read the emitted dictionaries rather than trusting
that parsing succeeded.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any, Dict

import pytest

from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.case_definition import (
    ACCEPTED_LEAF_CONSUMERS,
    SCHEMA_VERSION,
    CaseDefinitionError,
    parse_case_request,
)


# --------------------------------------------------------------------------- #
#  helpers                                                                     #
# --------------------------------------------------------------------------- #


def _authored_request(**overrides: Any) -> Dict[str, Any]:
    """A complete, valid canonical request; overrides replace top-level keys."""
    request: Dict[str, Any] = {
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
    request.update(overrides)
    return request


def _authored_with(section: str, value: Any) -> Dict[str, Any]:
    """Return a request whose authored[section] is replaced by ``value``."""
    request = _authored_request()
    if value is None:
        request["case_definition"]["authored"].pop(section, None)
    else:
        request["case_definition"]["authored"][section] = value
    return request


def _build(request: Dict[str, Any], tmp_path: Path) -> Path:
    parsed = parse_case_request(request)
    builder = OpenFOAMCaseBuilder(
        parsed.case,
        parsed.function_objects.pressure_taps,
        tap_write_control=parsed.function_objects.write_control,
        tap_write_interval=parsed.function_objects.write_interval,
    )
    return builder.build(tmp_path)


def _control_dict(case_dir: Path) -> Dict[str, str]:
    """Parse the scalar ``key value;`` entries of a controlDict."""
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
#  propagation: every authored leaf must reach the rendered case               #
# --------------------------------------------------------------------------- #


def test_requested_cell_counts_reach_block_mesh_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    block_mesh = (case_dir / "system" / "blockMeshDict").read_text()
    assert "hex (0 1 2 3 4 5 6 7) (20 10 10) simpleGrading (1 1 1)" in block_mesh


def test_requested_domain_extent_reaches_block_mesh_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    block_mesh = (case_dir / "system" / "blockMeshDict").read_text()
    assert "(     2.0000      1.0000      1.0000 )" in block_mesh


def test_requested_end_time_reaches_control_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    assert _control_dict(case_dir)["endTime"] == "2.0"


def test_requested_delta_t_reaches_control_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    assert _control_dict(case_dir)["deltaT"] == "0.002"


def test_requested_write_interval_reaches_control_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    assert _control_dict(case_dir)["writeInterval"] == "25"


def test_requested_max_co_reaches_control_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    assert _control_dict(case_dir)["maxCo"] == "0.5"


def test_requested_adjustable_time_step_reaches_control_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    assert _control_dict(case_dir)["adjustTimeStep"] == "yes"


def test_requested_solver_reaches_control_dict_application(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    assert _control_dict(case_dir)["application"] == "interFoam"


def test_motion_emits_dynamic_mesh_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    assert (case_dir / "constant" / "dynamicMeshDict").is_file()


def test_motion_period_reaches_dynamic_mesh_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    body = (case_dir / "constant" / "dynamicMeshDict").read_text()
    assert "T=1.5 s" in body


def test_motion_origin_reaches_dynamic_mesh_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    body = (case_dir / "constant" / "dynamicMeshDict").read_text()
    assert "(1 0.5 0)" in body


def test_absent_motion_emits_no_dynamic_mesh_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_with("motion", None), tmp_path)
    assert not (case_dir / "constant" / "dynamicMeshDict").exists()


def test_fill_emits_set_fields_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    assert (case_dir / "system" / "setFieldsDict").is_file()


def test_fill_level_reaches_set_fields_box(tmp_path: Path) -> None:
    # height 1.0 m over 10 cells => 0.1 m cells; a 0.4 fill lands exactly on a
    # cell face, so the emitted box top is the requested level with no snapping.
    case_dir = _build(_authored_request(), tmp_path)
    body = (case_dir / "system" / "setFieldsDict").read_text()
    assert "box (-2 -2 -2) (4 3 0.4);" in body


def test_function_objects_reach_control_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    assert "synthetic_tap" in (case_dir / "system" / "controlDict").read_text()


def test_tap_write_control_reaches_control_dict(tmp_path: Path) -> None:
    request = _authored_request()
    request["case_definition"]["authored"]["function_objects"][
        "write_control"
    ] = "runTime"
    case_dir = _build(request, tmp_path)
    body = (case_dir / "system" / "controlDict").read_text()
    functions_block = body[body.index("functions"):]
    assert "writeControl    runTime;" in functions_block


def test_tap_write_interval_reaches_control_dict(tmp_path: Path) -> None:
    request = _authored_request()
    request["case_definition"]["authored"]["function_objects"]["write_interval"] = 7
    case_dir = _build(request, tmp_path)
    body = (case_dir / "system" / "controlDict").read_text()
    functions_block = body[body.index("functions"):]
    assert "writeInterval   7;" in functions_block


def test_tap_fields_reach_control_dict(tmp_path: Path) -> None:
    case_dir = _build(_authored_request(), tmp_path)
    body = (case_dir / "system" / "controlDict").read_text()
    assert "fields          (p p_rgh);" in body


# --------------------------------------------------------------------------- #
#  execution plan                                                              #
# --------------------------------------------------------------------------- #


def test_execution_timeout_is_carried_onto_the_plan() -> None:
    parsed = parse_case_request(_authored_request())
    assert parsed.execution.timeout_seconds == 43200


def test_execution_run_set_fields_is_carried_onto_the_plan() -> None:
    parsed = parse_case_request(_authored_request())
    assert parsed.execution.run_set_fields is True


def test_execution_to_vtk_is_carried_onto_the_plan() -> None:
    parsed = parse_case_request(_authored_request())
    assert parsed.execution.to_vtk is False


# --------------------------------------------------------------------------- #
#  fail closed: unknown and dropped fields                                     #
# --------------------------------------------------------------------------- #


def test_unknown_authored_key_is_rejected_by_name() -> None:
    request = _authored_request()
    request["case_definition"]["authored"]["turbulence"] = "kOmegaSST"
    with pytest.raises(CaseDefinitionError, match="turbulence"):
        parse_case_request(request)


def test_unknown_domain_key_is_rejected_by_name() -> None:
    request = _authored_with(
        "domain",
        {
            "min_coords_m": [0.0, 0.0, 0.0],
            "max_coords_m": [2.0, 1.0, 1.0],
            "n_cells": [20, 10, 10],
            "base_cell_size_m": 0.1,
        },
    )
    with pytest.raises(CaseDefinitionError, match="base_cell_size_m"):
        parse_case_request(request)


def test_unknown_execution_key_is_rejected_by_name() -> None:
    request = _authored_request()
    request["execution"]["n_subdomains"] = 8
    with pytest.raises(CaseDefinitionError, match="n_subdomains"):
        parse_case_request(request)


def test_authored_rejects_n_subdomains_rank_authority() -> None:
    request = _authored_request()
    request["case_definition"]["authored"]["n_subdomains"] = 8
    with pytest.raises(CaseDefinitionError, match="n_subdomains"):
        parse_case_request(request)


def test_unknown_root_key_is_rejected_by_name() -> None:
    request = _authored_request()
    request["work_root"] = "/tmp/somewhere"
    with pytest.raises(CaseDefinitionError, match="work_root"):
        parse_case_request(request)


def test_missing_required_domain_is_rejected() -> None:
    with pytest.raises(CaseDefinitionError, match="domain"):
        parse_case_request(_authored_with("domain", None))


def test_missing_required_time_is_rejected() -> None:
    with pytest.raises(CaseDefinitionError, match="time"):
        parse_case_request(_authored_with("time", None))


def test_partial_optional_mapping_is_rejected() -> None:
    # motion is optional, but a present motion must carry all applicable leaves.
    with pytest.raises(CaseDefinitionError, match="period_s"):
        parse_case_request(
            _authored_with(
                "motion",
                {"type": "roll", "amplitude": 3.0, "amplitude_unit": "deg",
                 "origin_m": [1.0, 0.5, 0.0]},
            )
        )


def test_unsupported_schema_version_is_rejected() -> None:
    request = _authored_request()
    request["case_definition"]["schema_version"] = 2
    with pytest.raises(CaseDefinitionError, match="schema_version"):
        parse_case_request(request)


def test_unknown_kind_is_rejected() -> None:
    request = _authored_request()
    request["case_definition"]["kind"] = "imported"
    with pytest.raises(CaseDefinitionError, match="imported"):
        parse_case_request(request)


def test_prebuilt_kind_is_rejected_in_v1() -> None:
    request = _authored_request()
    request["case_definition"] = {
        "schema_version": 1,
        "kind": "prebuilt",
        "prebuilt": {"case_id": "some_case"},
    }
    with pytest.raises(CaseDefinitionError, match="prebuilt"):
        parse_case_request(request)


def test_schema_version_constant_is_one() -> None:
    assert SCHEMA_VERSION == 1


# --------------------------------------------------------------------------- #
#  value validation                                                            #
# --------------------------------------------------------------------------- #


def test_boolean_is_rejected_where_integer_expected() -> None:
    request = _authored_request()
    request["case_definition"]["authored"]["time"]["write_interval_steps"] = True
    with pytest.raises(CaseDefinitionError, match="write_interval_steps"):
        parse_case_request(request)


def test_non_finite_coordinate_is_rejected() -> None:
    with pytest.raises(CaseDefinitionError, match="max_coords_m"):
        parse_case_request(
            _authored_with(
                "domain",
                {"min_coords_m": [0.0, 0.0, 0.0],
                 "max_coords_m": [float("inf"), 1.0, 1.0],
                 "n_cells": [20, 10, 10]},
            )
        )


def test_non_positive_cell_count_is_rejected() -> None:
    with pytest.raises(CaseDefinitionError, match="n_cells"):
        parse_case_request(
            _authored_with(
                "domain",
                {"min_coords_m": [0.0, 0.0, 0.0],
                 "max_coords_m": [2.0, 1.0, 1.0],
                 "n_cells": [20, 0, 10]},
            )
        )


def test_inverted_domain_extent_is_rejected() -> None:
    with pytest.raises(CaseDefinitionError, match="max_coords_m"):
        parse_case_request(
            _authored_with(
                "domain",
                {"min_coords_m": [0.0, 0.0, 0.0],
                 "max_coords_m": [-2.0, 1.0, 1.0],
                 "n_cells": [20, 10, 10]},
            )
        )


def test_fill_level_above_one_is_rejected() -> None:
    with pytest.raises(CaseDefinitionError, match="level"):
        parse_case_request(_authored_with("fill", {"level": 1.4}))


def test_end_time_not_after_start_time_is_rejected() -> None:
    request = _authored_request()
    request["case_definition"]["authored"]["time"]["end_time_s"] = 0.0
    with pytest.raises(CaseDefinitionError, match="end_time_s"):
        parse_case_request(request)


def test_unsupported_write_control_is_rejected() -> None:
    request = _authored_request()
    request["case_definition"]["authored"]["function_objects"][
        "write_control"
    ] = "onEnd"
    with pytest.raises(CaseDefinitionError, match="onEnd"):
        parse_case_request(request)


# --------------------------------------------------------------------------- #
#  coordinate frame, motion units, and incompatible combinations               #
# --------------------------------------------------------------------------- #


def test_rotational_motion_requires_degrees() -> None:
    with pytest.raises(CaseDefinitionError, match="amplitude_unit"):
        parse_case_request(
            _authored_with(
                "motion",
                {"type": "roll", "amplitude": 3.0, "amplitude_unit": "m",
                 "period_s": 1.5, "origin_m": [1.0, 0.5, 0.0]},
            )
        )


def test_translational_motion_requires_metres() -> None:
    with pytest.raises(CaseDefinitionError, match="amplitude_unit"):
        parse_case_request(
            _authored_with(
                "motion",
                {"type": "sway", "amplitude": 0.1, "amplitude_unit": "deg",
                 "period_s": 1.5},
            )
        )


def test_translational_motion_rejects_rotation_origin() -> None:
    with pytest.raises(CaseDefinitionError, match="origin_m"):
        parse_case_request(
            _authored_with(
                "motion",
                {"type": "sway", "amplitude": 0.1, "amplitude_unit": "m",
                 "period_s": 1.5, "origin_m": [1.0, 0.5, 0.0]},
            )
        )


def test_rotational_motion_rejects_phase_shift() -> None:
    with pytest.raises(CaseDefinitionError, match="phase_shift_s"):
        parse_case_request(
            _authored_with(
                "motion",
                {"type": "roll", "amplitude": 3.0, "amplitude_unit": "deg",
                 "period_s": 1.5, "origin_m": [1.0, 0.5, 0.0],
                 "phase_shift_s": 0.25},
            )
        )


def test_fill_requires_run_set_fields() -> None:
    request = _authored_request()
    request["execution"]["run_set_fields"] = False
    with pytest.raises(CaseDefinitionError, match="run_set_fields"):
        parse_case_request(request)


def test_fill_requires_a_multiphase_case() -> None:
    request = _authored_request()
    authored = request["case_definition"]["authored"]
    authored["case_type"] = "current_loading"
    authored["solver"] = "simpleFoam"
    authored.pop("motion")
    with pytest.raises(CaseDefinitionError, match="multiphase"):
        parse_case_request(request)


def test_run_set_fields_without_fill_is_rejected() -> None:
    request = _authored_with("fill", None)
    with pytest.raises(CaseDefinitionError, match="run_set_fields"):
        parse_case_request(request)


def test_motion_requires_a_transient_solver() -> None:
    request = _authored_request()
    authored = request["case_definition"]["authored"]
    authored["case_type"] = "current_loading"
    authored["solver"] = "simpleFoam"
    authored.pop("fill")
    request["execution"]["run_set_fields"] = False
    with pytest.raises(CaseDefinitionError, match="transient"):
        parse_case_request(request)


# --------------------------------------------------------------------------- #
#  case-name confinement                                                       #
# --------------------------------------------------------------------------- #


@pytest.mark.parametrize(
    "name",
    ["../escape", "with/separator", "with space", "dotted.name", ".", "..",
     "", "CON", "nul", "lpt1", "trailing\x00"],
)
def test_unsafe_case_name_is_rejected(name: str) -> None:
    request = _authored_request()
    request["case_definition"]["authored"]["name"] = name
    with pytest.raises(CaseDefinitionError):
        parse_case_request(request)


def test_safe_case_name_is_accepted() -> None:
    request = _authored_request()
    request["case_definition"]["authored"]["name"] = "synthetic_case-01"
    assert parse_case_request(request).case.name == "synthetic_case-01"


# --------------------------------------------------------------------------- #
#  legacy compatibility                                                        #
# --------------------------------------------------------------------------- #


LEGACY_REQUEST = {
    "operation": "build_case",
    "case_type": "current_loading",
    "name": "legacy_case",
    "solver": "simpleFoam",
    "mesh_utility": "blockMesh",
    "run_snappy": False,
    "to_vtk": True,
    "dry_run": False,
    "timeout_seconds": 7200,
}


def test_legacy_request_still_parses() -> None:
    parsed = parse_case_request(dict(LEGACY_REQUEST))
    assert parsed.case.name == "legacy_case"


def test_legacy_request_keeps_its_timeout() -> None:
    parsed = parse_case_request(dict(LEGACY_REQUEST))
    assert parsed.execution.timeout_seconds == 7200


def test_legacy_request_renders_the_case_type_default_case(tmp_path: Path) -> None:
    # The legacy form carries no domain, so the case-type default must survive
    # normalisation byte-for-byte rather than being replaced by a schema default.
    from digitalmodel.solvers.openfoam.models import CaseType, OpenFOAMCase

    expected_dir = OpenFOAMCaseBuilder(
        OpenFOAMCase.for_case_type(CaseType.CURRENT_LOADING, "legacy_case")
    ).build(tmp_path / "expected")
    actual_dir = _build(dict(LEGACY_REQUEST), tmp_path / "actual")
    expected = (expected_dir / "system" / "blockMeshDict").read_bytes()
    assert (actual_dir / "system" / "blockMeshDict").read_bytes() == expected


def test_legacy_and_canonical_forms_cannot_mix() -> None:
    request = _authored_request()
    request["case_type"] = "sloshing"
    with pytest.raises(CaseDefinitionError, match="case_type"):
        parse_case_request(request)


# --------------------------------------------------------------------------- #
#  accepted-leaf consumption ledger                                            #
# --------------------------------------------------------------------------- #


VALID_CONSUMERS = frozenset(
    {
        "OpenFOAMCase",
        "FunctionObjectsConfig",
        "SelectedExecutionPlan",
        "WorkLayout/RunIdentity",
        "batch matrix/dispatch",
        "generic operation/output",
    }
)


def test_every_accepted_leaf_names_a_valid_consumer() -> None:
    unknown = {
        leaf: consumer
        for leaf, consumer in ACCEPTED_LEAF_CONSUMERS.items()
        if consumer not in VALID_CONSUMERS
    }
    assert unknown == {}


def test_every_canonical_authored_leaf_appears_in_the_ledger() -> None:
    authored_leaves = {
        "case_definition.authored.case_type",
        "case_definition.authored.name",
        "case_definition.authored.solver",
        "case_definition.authored.domain.min_coords_m",
        "case_definition.authored.domain.max_coords_m",
        "case_definition.authored.domain.n_cells",
        "case_definition.authored.motion.type",
        "case_definition.authored.motion.amplitude",
        "case_definition.authored.motion.amplitude_unit",
        "case_definition.authored.motion.period_s",
        "case_definition.authored.motion.origin_m",
        "case_definition.authored.motion.phase_shift_s",
        "case_definition.authored.fill.level",
        "case_definition.authored.time.start_time_s",
        "case_definition.authored.time.end_time_s",
        "case_definition.authored.time.delta_t_s",
        "case_definition.authored.time.write_interval_steps",
        "case_definition.authored.time.adjustable_time_step",
        "case_definition.authored.time.max_co",
        "case_definition.authored.time.purge_write",
        "case_definition.authored.function_objects.pressure_taps",
        "case_definition.authored.function_objects.write_control",
        "case_definition.authored.function_objects.write_interval",
        "execution.mesh_utility",
        "execution.run_snappy",
        "execution.run_set_fields",
        "execution.to_vtk",
        "execution.timeout_seconds",
        "execution.dry_run",
    }
    assert authored_leaves - set(ACCEPTED_LEAF_CONSUMERS) == set()


def test_ledger_has_no_leaf_the_schema_does_not_accept() -> None:
    request = _authored_request()
    accepted = set(_accepted_paths(request))
    assert set(ACCEPTED_LEAF_CONSUMERS) - accepted == set()


def _accepted_paths(request: Dict[str, Any], prefix: str = "") -> list[str]:
    """Enumerate the dotted leaf paths present in a complete valid request."""
    paths: list[str] = []
    for key, value in request.items():
        if key in ("operation", "output_directory"):
            continue
        path = f"{prefix}{key}"
        if key in ("schema_version", "kind"):
            continue
        if isinstance(value, dict):
            paths.extend(_accepted_paths(value, f"{path}."))
        elif key == "pressure_taps":
            paths.append(path)
        else:
            paths.append(path)
    return paths
