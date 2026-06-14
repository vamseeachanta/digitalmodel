"""UV-workflow router for offline riser and mooring code checks."""

from __future__ import annotations

import csv
import re
from pathlib import Path
from typing import Any

import numpy as np

from digitalmodel.orcaflex.code_check_engine import (
    APIRP2RDInput,
    APIRP2SKInput,
    CodeStandard,
    DNVOSF201Input,
    check_api_rp_2rd,
    check_dnv_os_f201,
    check_mooring_api_2sk,
)


REPO_ROOT = Path(__file__).resolve().parents[3]


def router(cfg: dict) -> dict:
    settings = cfg.get("code_check") or {}
    inputs = _inputs(settings)
    code = _code_standard(settings.get("code"))
    rows = _run_check(code, inputs)
    csv_path = _results_csv_path(cfg, settings)
    _write_results_csv(csv_path, rows)

    governing = max(rows, key=lambda row: float(row["utilisation"]))
    overall_pass = all(bool(row["pass"]) for row in rows)
    cfg["code_check"] = {
        "code": code.value,
        "points": len(rows),
        "governing_utilisation": float(governing["utilisation"]),
        "pass": overall_pass,
        "governing_result": _strip_code(governing),
        "results": [_strip_code(row) for row in rows],
        "results_csv": _display_path(csv_path),
    }
    return cfg


def _run_check(code: CodeStandard, inputs: dict[str, Any]) -> list[dict[str, Any]]:
    if code == CodeStandard.API_RP_2RD:
        return _run_api_rp_2rd(inputs)
    if code == CodeStandard.DNV_OS_F201:
        return _run_dnv_os_f201(inputs)
    if code == CodeStandard.API_RP_2SK:
        return _run_api_rp_2sk(inputs)
    raise ValueError(f"Unsupported code_check code: {code}")


def _run_api_rp_2rd(inputs: dict[str, Any]) -> list[dict[str, Any]]:
    pipe = APIRP2RDInput(**_model_input_fields(APIRP2RDInput, inputs))
    arc_lengths = _required_array(inputs, ("arc_lengths", "arc_length_m"))
    tensions = _required_array_like(
        inputs,
        ("effective_tension_kN", "effective_tension", "tensions_kN", "tension_kN"),
        len(arc_lengths),
        "effective_tension_kN",
    )
    moments = _required_array_like(
        inputs,
        ("bending_moment_kNm", "bending_moment", "bending_moments_kNm", "moment_kNm"),
        len(arc_lengths),
        "bending_moment_kNm",
    )
    pressures = _optional_array_like(
        inputs,
        ("pressure_diff_MPa", "pressure_diff", "pressures_MPa", "pressure_MPa"),
        len(arc_lengths),
    )
    results = check_api_rp_2rd(pipe, arc_lengths, tensions, moments, pressures)
    return _result_rows(CodeStandard.API_RP_2RD, results)


def _run_dnv_os_f201(inputs: dict[str, Any]) -> list[dict[str, Any]]:
    pipe = DNVOSF201Input(**_model_input_fields(DNVOSF201Input, inputs))
    arc_lengths = _required_array(inputs, ("arc_lengths", "arc_length_m"))
    tensions = _required_array_like(
        inputs,
        ("effective_tension_kN", "effective_tension", "tensions_kN", "tension_kN"),
        len(arc_lengths),
        "effective_tension_kN",
    )
    moments = _required_array_like(
        inputs,
        ("bending_moment_kNm", "bending_moment", "bending_moments_kNm", "moment_kNm"),
        len(arc_lengths),
        "bending_moment_kNm",
    )
    external_pressure = _optional_array_like(
        inputs,
        ("external_pressure_MPa", "external_pressure", "pressure_external_MPa"),
        len(arc_lengths),
    )
    internal_pressure = _optional_array_like(
        inputs,
        ("internal_pressure_MPa", "internal_pressure", "pressure_internal_MPa"),
        len(arc_lengths),
    )
    results = check_dnv_os_f201(
        pipe, arc_lengths, tensions, moments, external_pressure, internal_pressure
    )
    return _result_rows(CodeStandard.DNV_OS_F201, results)


def _run_api_rp_2sk(inputs: dict[str, Any]) -> list[dict[str, Any]]:
    line = APIRP2SKInput(**_model_input_fields(APIRP2SKInput, inputs))
    line_name = str(inputs.get("line_name", "Line-1"))
    max_tension_kN = _required_float(
        inputs,
        ("max_tension_kN", "maximum_tension_kN", "tension_kN"),
        "max_tension_kN",
    )
    result = check_mooring_api_2sk(
        line_name=line_name,
        max_tension_kN=max_tension_kN,
        mbl_kN=line.mbl_kN,
        condition=line.condition,
    )
    return _result_rows(CodeStandard.API_RP_2SK, [result])


def _inputs(settings: dict[str, Any]) -> dict[str, Any]:
    inputs = settings.get("inputs")
    if not isinstance(inputs, dict):
        raise ValueError("code_check inputs must be a mapping")
    return inputs


def _code_standard(raw_code: Any) -> CodeStandard:
    if raw_code is None or str(raw_code).strip() == "":
        raise ValueError("code_check code is required")
    normalized = re.sub(r"[^A-Z0-9]+", "_", str(raw_code).strip().upper()).strip("_")
    aliases = {
        "API_2RD": "API_RP_2RD",
        "APIRP2RD": "API_RP_2RD",
        "API_RP2RD": "API_RP_2RD",
        "DNV_F201": "DNV_OS_F201",
        "DNVOSF201": "DNV_OS_F201",
        "DNV_OSF201": "DNV_OS_F201",
        "API_2SK": "API_RP_2SK",
        "APIRP2SK": "API_RP_2SK",
        "API_RP2SK": "API_RP_2SK",
    }
    normalized = aliases.get(normalized, normalized)
    try:
        return CodeStandard(normalized)
    except ValueError as exc:
        raise ValueError(f"Unsupported code_check code: {raw_code}") from exc


def _model_input_fields(model_type: Any, inputs: dict[str, Any]) -> dict[str, Any]:
    fields = getattr(model_type, "model_fields", getattr(model_type, "__fields__", {}))
    return {name: inputs[name] for name in fields if name in inputs}


def _required_array(inputs: dict[str, Any], aliases: tuple[str, ...]) -> np.ndarray:
    value = _first_value(inputs, aliases)
    if value is None:
        raise ValueError(f"code_check inputs.{aliases[0]} is required")
    values = _float_array(value)
    if len(values) == 0:
        raise ValueError(f"code_check inputs.{aliases[0]} must not be empty")
    return values


def _required_array_like(
    inputs: dict[str, Any],
    aliases: tuple[str, ...],
    size: int,
    display_name: str,
) -> np.ndarray:
    value = _first_value(inputs, aliases)
    if value is None:
        raise ValueError(f"code_check inputs.{display_name} is required")
    return _array_like(value, size, display_name)


def _optional_array_like(
    inputs: dict[str, Any],
    aliases: tuple[str, ...],
    size: int,
) -> np.ndarray | None:
    value = _first_value(inputs, aliases)
    if value is None:
        return None
    return _array_like(value, size, aliases[0])


def _array_like(value: Any, size: int, display_name: str) -> np.ndarray:
    values = _float_array(value)
    if len(values) == 1 and size != 1:
        values = np.repeat(values[0], size)
    if len(values) != size:
        raise ValueError(
            f"code_check inputs.{display_name} length must be 1 or match arc_lengths"
        )
    return values


def _float_array(value: Any) -> np.ndarray:
    if isinstance(value, np.ndarray):
        values = value
    elif isinstance(value, (list, tuple)):
        values = np.array(value, dtype=float)
    else:
        values = np.array([value], dtype=float)
    return values.astype(float)


def _required_float(
    inputs: dict[str, Any],
    aliases: tuple[str, ...],
    display_name: str,
) -> float:
    value = _first_value(inputs, aliases)
    if value is None:
        raise ValueError(f"code_check inputs.{display_name} is required")
    return float(value)


def _first_value(inputs: dict[str, Any], aliases: tuple[str, ...]) -> Any:
    for alias in aliases:
        if inputs.get(alias) is not None:
            return inputs[alias]
    return None


def _result_rows(code: CodeStandard, results: list[Any]) -> list[dict[str, Any]]:
    rows = []
    for result in results:
        row = _model_dump(result)
        row = {"code": code.value, **row}
        rows.append(row)
    if not rows:
        raise ValueError(f"code_check {code.value} returned no results")
    return rows


def _model_dump(model: Any) -> dict[str, Any]:
    if hasattr(model, "model_dump"):
        return model.model_dump(by_alias=True)
    return model.dict(by_alias=True)


def _strip_code(row: dict[str, Any]) -> dict[str, Any]:
    return {key: value for key, value in row.items() if key != "code"}


def _results_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    stem = _input_stem(cfg)
    return output_dir / f"{stem}_code_check.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "code_check"))


def _write_results_csv(path: Path, rows: list[dict[str, Any]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = list(rows[0].keys())
    for row in rows[1:]:
        for key in row:
            if key not in fieldnames:
                fieldnames.append(key)
    with path.open("w", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
