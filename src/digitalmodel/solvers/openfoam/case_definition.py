"""Fail-closed schema-v1 parser for authored OpenFOAM case requests."""

from __future__ import annotations

from dataclasses import dataclass
from types import MappingProxyType
from typing import Any, Mapping

from ._case_definition_validation import (
    ValidationError,
    check_keys,
    require_bool,
    require_int,
    require_mapping,
    require_number,
    require_string,
    require_string_sequence,
    require_vector,
    validate_case_name,
)
from .models import CaseType, DomainConfig, OpenFOAMCase
from .motion import MotionType, PrescribedMotion
from .pressure_taps import PressureTap

SCHEMA_VERSION = 1


class CaseDefinitionError(ValueError):
    """Raised when a case request does not conform to the supported schema."""


@dataclass(frozen=True)
class FunctionObjectsConfig:
    """Neutral function-object settings carried to the case builder."""

    pressure_taps: tuple[PressureTap, ...]
    write_control: str
    write_interval: int


@dataclass(frozen=True)
class SelectedExecutionPlan:
    """Validated execution choices selected by the request."""

    mesh_utility: str
    run_snappy: bool
    run_set_fields: bool
    to_vtk: bool
    timeout_seconds: int
    dry_run: bool


@dataclass(frozen=True)
class ParsedAuthoredCaseV1:
    """A constructed case and its non-case execution configuration."""

    case: OpenFOAMCase
    function_objects: FunctionObjectsConfig
    execution: SelectedExecutionPlan


_AUTHORED_KEYS = {
    "case_type", "name", "solver", "domain", "motion", "fill", "time",
    "function_objects",
}
_EXECUTION_KEYS = {
    "mesh_utility", "run_snappy", "run_set_fields", "to_vtk",
    "timeout_seconds", "dry_run",
}
_LEGACY_KEYS = {
    "operation", "case_type", "name", "output_directory", "solver",
    "mesh_utility", "run_snappy", "to_vtk", "dry_run", "timeout_seconds",
}
_CANONICAL_ROOT_KEYS = {"operation", "output_directory", "case_definition", "execution"}
_WRITE_CONTROLS = {"timeStep", "runTime", "adjustableRunTime"}
# The builder's own defaults; an empty tap list may carry only these, because
# any other value would have nothing to render it onto.
_DEFAULT_WRITE_CONTROL = "timeStep"
_DEFAULT_WRITE_INTERVAL = 1


def _ledger() -> dict[str, str]:
    case = "OpenFOAMCase"
    function = "FunctionObjectsConfig"
    execution = "SelectedExecutionPlan"
    return {
        "case_definition.authored.case_type": case,
        "case_definition.authored.name": "WorkLayout/RunIdentity",
        "case_definition.authored.solver": case,
        "case_definition.authored.domain.min_coords_m": case,
        "case_definition.authored.domain.max_coords_m": case,
        "case_definition.authored.domain.n_cells": case,
        "case_definition.authored.motion.type": case,
        "case_definition.authored.motion.amplitude": case,
        "case_definition.authored.motion.amplitude_unit": case,
        "case_definition.authored.motion.period_s": case,
        "case_definition.authored.motion.origin_m": case,
        "case_definition.authored.motion.phase_shift_s": case,
        "case_definition.authored.fill.level": case,
        "case_definition.authored.time.start_time_s": case,
        "case_definition.authored.time.end_time_s": case,
        "case_definition.authored.time.delta_t_s": case,
        "case_definition.authored.time.write_interval_steps": case,
        "case_definition.authored.time.adjustable_time_step": case,
        "case_definition.authored.time.max_co": case,
        "case_definition.authored.time.purge_write": case,
        "case_definition.authored.function_objects.pressure_taps": function,
        "case_definition.authored.function_objects.write_control": function,
        "case_definition.authored.function_objects.write_interval": function,
        **{f"execution.{key}": execution for key in _EXECUTION_KEYS},
    }


ACCEPTED_LEAF_CONSUMERS: Mapping[str, str] = MappingProxyType(_ledger())


def _parse_execution(value: Any) -> SelectedExecutionPlan:
    data = require_mapping(value, "execution")
    check_keys(data, allowed=_EXECUTION_KEYS, required=_EXECUTION_KEYS, path="execution")
    return SelectedExecutionPlan(
        mesh_utility=require_string(data["mesh_utility"], "execution.mesh_utility"),
        run_snappy=require_bool(data["run_snappy"], "execution.run_snappy"),
        run_set_fields=require_bool(data["run_set_fields"], "execution.run_set_fields"),
        to_vtk=require_bool(data["to_vtk"], "execution.to_vtk"),
        timeout_seconds=require_int(
            data["timeout_seconds"], "execution.timeout_seconds", positive=True
        ),
        dry_run=require_bool(data["dry_run"], "execution.dry_run"),
    )


def _parse_domain(value: Any) -> DomainConfig:
    data = require_mapping(value, "case_definition.authored.domain")
    keys = {"min_coords_m", "max_coords_m", "n_cells"}
    check_keys(data, allowed=keys, required=keys, path="case_definition.authored.domain")
    minimum = require_vector(data["min_coords_m"], "domain.min_coords_m")
    maximum = require_vector(data["max_coords_m"], "domain.max_coords_m")
    cells = require_vector(data["n_cells"], "domain.n_cells", integers=True, positive=True)
    if any(maximum[index] <= minimum[index] for index in range(3)):
        raise ValidationError("domain.max_coords_m must exceed min_coords_m")
    return DomainConfig(
        min_coords=list(minimum), max_coords=list(maximum), n_cells=list(cells)
    )


def _parse_time(value: Any, case: OpenFOAMCase) -> None:
    data = require_mapping(value, "case_definition.authored.time")
    keys = {
        "start_time_s", "end_time_s", "delta_t_s", "write_interval_steps",
        "adjustable_time_step", "max_co", "purge_write",
    }
    check_keys(data, allowed=keys, required=keys, path="case_definition.authored.time")
    start = require_number(data["start_time_s"], "time.start_time_s")
    end = require_number(data["end_time_s"], "time.end_time_s")
    if end <= start:
        raise ValidationError("time.end_time_s must be after start_time_s")
    solver = case.solver_config
    solver.start_time = start
    solver.end_time = end
    solver.delta_t = require_number(data["delta_t_s"], "time.delta_t_s", positive=True)
    solver.write_interval = require_int(
        data["write_interval_steps"], "time.write_interval_steps", positive=True
    )
    solver.adjustable_time_step = require_bool(
        data["adjustable_time_step"], "time.adjustable_time_step"
    )
    solver.max_co = require_number(data["max_co"], "time.max_co")
    solver.purge_write = require_int(data["purge_write"], "time.purge_write")


def _parse_motion(value: Any) -> PrescribedMotion:
    data = require_mapping(value, "case_definition.authored.motion")
    keys = {"type", "amplitude", "amplitude_unit", "period_s", "origin_m", "phase_shift_s"}
    check_keys(data, allowed=keys, path="case_definition.authored.motion")
    for required in ("type", "amplitude", "amplitude_unit", "period_s"):
        if required not in data:
            raise ValidationError(f"missing required key motion.{required}")
    motion_type = _motion_type(data["type"])
    unit = require_string(data["amplitude_unit"], "motion.amplitude_unit")
    if motion_type.is_rotational:
        origin, phase = _rotational_motion_values(data, unit)
    else:
        origin, phase = _translational_motion_values(data, unit)
    return PrescribedMotion(
        motion_type,
        require_number(data["amplitude"], "motion.amplitude", positive=True),
        require_number(data["period_s"], "motion.period_s", positive=True),
        origin,
        phase,
    )


def _motion_type(value: Any) -> MotionType:
    name = require_string(value, "motion.type")
    try:
        return MotionType(name)
    except ValueError as error:
        raise ValidationError(f"unsupported motion.type {name!r}") from error


def _rotational_motion_values(
    data: Mapping[str, Any], unit: str
) -> tuple[tuple[float, float, float], float]:
    if unit != "deg":
        raise ValidationError("motion.amplitude_unit must be 'deg' for rotation")
    if "origin_m" not in data:
        raise ValidationError("missing required key motion.origin_m")
    if "phase_shift_s" in data:
        raise ValidationError("motion.phase_shift_s is invalid for rotation")
    return require_vector(data["origin_m"], "motion.origin_m"), 0.0


def _translational_motion_values(
    data: Mapping[str, Any], unit: str
) -> tuple[tuple[float, float, float], float]:
    if unit != "m":
        raise ValidationError("motion.amplitude_unit must be 'm' for translation")
    if "origin_m" in data:
        raise ValidationError("motion.origin_m is invalid for translation")
    phase = require_number(data.get("phase_shift_s", 0.0), "motion.phase_shift_s")
    return (0.0, 0.0, 0.0), phase


def _parse_function_objects(value: Any) -> FunctionObjectsConfig:
    data = require_mapping(value, "case_definition.authored.function_objects")
    keys = {"pressure_taps", "write_control", "write_interval"}
    check_keys(data, allowed=keys, required=keys, path="case_definition.authored.function_objects")
    control = require_string(data["write_control"], "function_objects.write_control")
    if control not in _WRITE_CONTROLS:
        raise ValidationError(f"unsupported write_control {control!r}")
    taps_value = data["pressure_taps"]
    if isinstance(taps_value, (str, bytes)) or not isinstance(taps_value, (list, tuple)):
        raise ValidationError("function_objects.pressure_taps must be a sequence")
    interval = require_int(
        data["write_interval"], "function_objects.write_interval", positive=True
    )
    taps = tuple(_parse_tap(tap, index) for index, tap in enumerate(taps_value))
    if not taps:
        # Nothing would carry these values into the emitted functions block, so
        # accepting a non-default pair would silently drop it.
        if control != _DEFAULT_WRITE_CONTROL:
            raise ValidationError(
                "function_objects.write_control is unconsumed with no pressure_taps"
            )
        if interval != _DEFAULT_WRITE_INTERVAL:
            raise ValidationError(
                "function_objects.write_interval is unconsumed with no pressure_taps"
            )
    return FunctionObjectsConfig(taps, control, interval)


def _parse_tap(value: Any, index: int) -> PressureTap:
    path = f"function_objects.pressure_taps[{index}]"
    data = require_mapping(value, path)
    keys = {"name", "location_m", "fields"}
    check_keys(data, allowed=keys, required=keys, path=path)
    return PressureTap(
        name=require_string(data["name"], f"{path}.name"),
        location=require_vector(data["location_m"], f"{path}.location_m"),
        fields=require_string_sequence(data["fields"], f"{path}.fields"),
    )


def _case_type(value: Any) -> CaseType:
    name = require_string(value, "case_type")
    try:
        return CaseType(name)
    except ValueError as error:
        # Wording preserved from the pre-#1575 router; callers and tests match
        # on it.
        valid = ", ".join(item.value for item in CaseType)
        raise ValidationError(
            f"Unknown openfoam.case_type {name!r}. Valid: {valid}"
        ) from error


def _parse_authored(value: Any, execution: SelectedExecutionPlan) -> ParsedAuthoredCaseV1:
    data = require_mapping(value, "case_definition.authored")
    required = {"case_type", "name", "solver", "domain", "time"}
    check_keys(data, allowed=_AUTHORED_KEYS, required=required, path="case_definition.authored")
    case = OpenFOAMCase.for_case_type(
        _case_type(data["case_type"]), validate_case_name(data["name"], "name")
    )
    case.solver_config.solver_name = require_string(data["solver"], "solver")
    case.domain = _parse_domain(data["domain"])
    _parse_time(data["time"], case)
    functions = _optional_function_objects(data)
    if "motion" in data:
        case.motion = _parse_motion(data["motion"])
    if "fill" in data:
        case.fill_level = _parse_fill(data["fill"])
    _validate_combinations(case, execution)
    return ParsedAuthoredCaseV1(case, functions, execution)


def _optional_function_objects(data: Mapping[str, Any]) -> FunctionObjectsConfig:
    if "function_objects" not in data:
        return FunctionObjectsConfig((), "timeStep", 1)
    return _parse_function_objects(data["function_objects"])


def _parse_fill(value: Any) -> float:
    data = require_mapping(value, "case_definition.authored.fill")
    check_keys(data, allowed={"level"}, required={"level"}, path="case_definition.authored.fill")
    level = require_number(data["level"], "fill.level")
    if not 0.0 <= level <= 1.0:
        raise ValidationError("fill.level must be between 0 and 1")
    return level


def _validate_combinations(case: OpenFOAMCase, plan: SelectedExecutionPlan) -> None:
    if case.fill_level is not None and not case.solver_config.is_multiphase:
        raise ValidationError("fill requires a multiphase case")
    if case.fill_level is not None and not plan.run_set_fields:
        raise ValidationError("fill requires execution.run_set_fields true")
    if plan.run_set_fields and case.fill_level is None:
        raise ValidationError("execution.run_set_fields requires fill")
    if case.motion is not None and case.solver_config.solver_name not in {
        "interFoam", "pimpleFoam"
    }:
        raise ValidationError("motion requires a transient solver")


def _parse_canonical(root: Mapping[str, Any]) -> ParsedAuthoredCaseV1:
    check_keys(root, allowed=_CANONICAL_ROOT_KEYS, path="root")
    for required in ("case_definition", "execution"):
        if required not in root:
            raise ValidationError(f"missing required root key {required}")
    definition = require_mapping(root["case_definition"], "case_definition")
    check_keys(
        definition,
        allowed={"schema_version", "kind", "authored", "prebuilt"},
        required={"schema_version", "kind"},
        path="case_definition",
    )
    version = require_int(
        definition["schema_version"], "case_definition.schema_version"
    )
    if version != SCHEMA_VERSION:
        raise ValidationError("unsupported case_definition.schema_version")
    kind = require_string(definition["kind"], "case_definition.kind")
    if kind == "prebuilt":
        raise ValidationError("prebuilt reserved in schema v1; use OpenFOAMRunner.run(prebuilt_manifest=...)")
    if kind != "authored":
        raise ValidationError(f"unsupported case_definition.kind {kind!r}")
    check_keys(
        definition,
        allowed={"schema_version", "kind", "authored"},
        required={"schema_version", "kind", "authored"},
        path="case_definition",
    )
    return _parse_authored(definition["authored"], _parse_execution(root["execution"]))


def _parse_legacy(root: Mapping[str, Any]) -> ParsedAuthoredCaseV1:
    check_keys(root, allowed=_LEGACY_KEYS, path="root")
    if "case_type" not in root:
        # Wording preserved from the pre-#1575 router.
        raise ValidationError("openfoam.case_type is required")
    case_type = _case_type(root["case_type"])
    # The legacy generic form has always defaulted the case name from the case
    # type; normalisation must not turn that into a required key.
    name = root.get("name") or f"{case_type.value}_case"
    case = OpenFOAMCase.for_case_type(case_type, validate_case_name(name, "name"))
    if "solver" in root:
        case.solver_config.solver_name = require_string(root["solver"], "solver")
    plan = SelectedExecutionPlan(
        mesh_utility=require_string(root.get("mesh_utility", "blockMesh"), "mesh_utility"),
        run_snappy=require_bool(root.get("run_snappy", False), "run_snappy"),
        run_set_fields=False,
        to_vtk=require_bool(root.get("to_vtk", True), "to_vtk"),
        timeout_seconds=require_int(root.get("timeout_seconds", 7200), "timeout_seconds", positive=True),
        dry_run=require_bool(root.get("dry_run", False), "dry_run"),
    )
    return ParsedAuthoredCaseV1(case, FunctionObjectsConfig((), "timeStep", 1), plan)


def parse_case_request(settings: Mapping[str, Any]) -> ParsedAuthoredCaseV1:
    """Validate and construct one canonical authored-v1 or legacy request."""
    try:
        root = require_mapping(settings, "root")
        if "case_definition" in root or "execution" in root:
            return _parse_canonical(root)
        return _parse_legacy(root)
    except (ValidationError, KeyError, TypeError, ValueError) as error:
        if isinstance(error, CaseDefinitionError):
            raise
        raise CaseDefinitionError(str(error)) from error


__all__ = [
    "ACCEPTED_LEAF_CONSUMERS",
    "SCHEMA_VERSION",
    "CaseDefinitionError",
    "FunctionObjectsConfig",
    "ParsedAuthoredCaseV1",
    "SelectedExecutionPlan",
    "parse_case_request",
]
