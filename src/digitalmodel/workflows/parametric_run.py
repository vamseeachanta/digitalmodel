"""YAML-config-driven parametric workflow runner."""

from __future__ import annotations

import sys
from copy import deepcopy
from pathlib import Path
from typing import Any

import pandas as pd
import yaml
from loguru import logger

from digitalmodel.orcaflex.batch_parametric import ParameterSweep, ParametricStudy


REPO_ROOT = Path(__file__).resolve().parents[3]


def router(cfg: dict) -> dict:
    settings = cfg.get("parametric_run") or {}
    cfg_dir = Path(cfg.get("_config_dir_path") or REPO_ROOT)
    base_input = _resolve_input_path(settings["base_input"], cfg_dir)
    base_cfg = _read_yaml(base_input)
    variants = settings.get("variants") or {}
    cases = _load_cases(variants, cfg_dir)

    render = settings.get("render") or {}
    output_dir = _resolve_output_path(
        render.get("output_dir", "results/parametric_run")
    )
    output_pattern = render.get("output_pattern", "case_{index}")
    output_dir.mkdir(parents=True, exist_ok=True)

    run = settings.get("run") or {}
    _validate_run_settings(run)

    collect = settings.get("collect") or {}
    manifest_path = _resolve_output_path(
        collect.get("manifest") or str(output_dir / "cases.csv")
    )
    summary_path = _optional_output_path(collect.get("summary"))

    rows, first_error = _render_and_run_cases(
        cases=cases,
        base_cfg=base_cfg,
        target_basename=settings["target_basename"],
        mapping=variants.get("mapping") or {},
        output_dir=output_dir,
        output_pattern=output_pattern,
    )
    _write_manifest(rows, manifest_path)
    if summary_path is not None:
        _write_summary(rows, summary_path)
    if first_error is not None:
        _raise_case_error(first_error)

    settings["cases"] = rows
    settings["outputs"] = {"manifest": _display_path(manifest_path)}
    if summary_path is not None:
        settings["outputs"]["summary"] = _display_path(summary_path)
    cfg["parametric_run"] = settings
    return cfg


def _validate_run_settings(run: dict) -> None:
    engine_name = run.get("engine", "python")
    if engine_name != "python":
        raise ValueError(f"parametric_run only supports run.engine=python, got {engine_name}")

    mode = run.get("mode", "serial")
    if mode == "parallel":
        logger.warning("parametric_run parallel mode is not implemented; using serial")
    elif mode != "serial":
        raise ValueError(f"parametric_run only supports serial|parallel, got {mode}")


def _render_and_run_cases(
    cases: list[dict[str, Any]],
    base_cfg: dict,
    target_basename: str,
    mapping: dict[str, str],
    output_dir: Path,
    output_pattern: str,
) -> tuple[list[dict[str, Any]], Exception | None]:
    rows: list[dict[str, Any]] = []
    first_error: Exception | None = None
    for index, case in enumerate(cases):
        case_input = output_dir / _case_filename(output_pattern, index, case)
        case_cfg = _case_config(base_cfg, target_basename, case, mapping)
        _write_yaml(case_input, case_cfg)
        row = {"index": index, **case, "input_file": _display_path(case_input)}
        try:
            _run_python_engine(case_cfg, case_input)
            row["status"] = "completed"
        except Exception as exc:
            row["status"] = "failed"
            row["error"] = str(exc)
            first_error = exc
        rows.append(row)
        if first_error is not None:
            break
    return rows, first_error


def _load_cases(variants: dict, cfg_dir: Path) -> list[dict[str, Any]]:
    source = variants.get("source")
    if source == "factorial":
        return _factorial_cases(variants)
    if source == "range":
        return _range_cases(variants)
    if source == "csv":
        return _csv_cases(variants, cfg_dir)
    if source == "yaml_matrix":
        return [_normalise_record(row) for row in variants.get("list", [])]
    raise ValueError(f"Unsupported parametric_run variants.source: {source}")


def _factorial_cases(variants: dict) -> list[dict[str, Any]]:
    parameters = [ParameterSweep(**param) for param in variants.get("parameters", [])]
    study = ParametricStudy(parameters=parameters)
    matrix = study.generate_case_matrix()
    records = matrix.drop(columns=["case_id"], errors="ignore").to_dict("records")
    return [_normalise_record(record) for record in records]


def _range_cases(variants: dict) -> list[dict[str, Any]]:
    param = variants["param"]
    start = variants["from"]
    stop = variants["to"]
    step = variants.get("step", 1)
    if step == 0:
        raise ValueError("parametric_run range step cannot be zero")

    values = []
    value = start
    tolerance = abs(step) * 1.0e-9
    while (step > 0 and value <= stop + tolerance) or (
        step < 0 and value >= stop - tolerance
    ):
        values.append(_coerce_number(value))
        value += step
    return [{param: item} for item in values]


def _csv_cases(variants: dict, cfg_dir: Path) -> list[dict[str, Any]]:
    csv_path = _resolve_input_path(variants["file"], cfg_dir)
    records = pd.read_csv(csv_path).to_dict("records")
    return [_normalise_record(record) for record in records]


def _case_config(
    base_cfg: dict,
    target_basename: str,
    case: dict[str, Any],
    mapping: dict[str, str],
) -> dict:
    case_cfg = deepcopy(base_cfg)
    case_cfg["basename"] = target_basename
    for name, value in case.items():
        _set_dotted(case_cfg, mapping.get(name, name), value)
    return case_cfg


def _set_dotted(target: dict, dotted_path: str, value: Any) -> None:
    parts = dotted_path.split(".")
    current: Any = target
    for index, part in enumerate(parts[:-1]):
        next_part = parts[index + 1]
        if isinstance(current, list):
            current = current[int(part)]
        else:
            current = current.setdefault(part, [] if next_part.isdigit() else {})
    final = parts[-1]
    if isinstance(current, list):
        current[int(final)] = value
    else:
        current[final] = value


def _run_python_engine(case_cfg: dict, case_input: Path) -> None:
    from digitalmodel.engine import engine

    run_cfg = deepcopy(case_cfg)
    run_cfg["_config_file_path"] = str(case_input.resolve())
    run_cfg["_config_dir_path"] = str(case_input.parent.resolve())
    original_argv = sys.argv[:]
    sys.argv = [original_argv[0], str(case_input)]
    try:
        engine(inputfile=str(case_input), cfg=run_cfg)
    finally:
        sys.argv = original_argv


def _case_filename(pattern: str, index: int, case: dict[str, Any]) -> str:
    name = pattern.format(index=index, **case)
    if Path(name).suffix not in {".yml", ".yaml"}:
        name = f"{name}.yml"
    return name


def _read_yaml(path: Path) -> dict:
    with path.open() as stream:
        loaded = yaml.safe_load(stream) or {}
    if not isinstance(loaded, dict):
        raise ValueError(f"Expected mapping YAML in {path}")
    return loaded


def _write_yaml(path: Path, payload: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(yaml.safe_dump(payload, sort_keys=False))


def _write_manifest(rows: list[dict[str, Any]], path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows).to_csv(path, index=False)


def _write_summary(rows: list[dict[str, Any]], path: Path) -> None:
    statuses = pd.Series([row["status"] for row in rows]).value_counts().to_dict()
    summary = {
        "total_cases": len(rows),
        "completed": statuses.get("completed", 0),
        "failed": statuses.get("failed", 0),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame([summary]).to_csv(path, index=False)


def _resolve_input_path(path_value: str, cfg_dir: Path) -> Path:
    path = Path(path_value)
    if path.is_absolute():
        return path
    cfg_relative = cfg_dir / path
    if cfg_relative.exists():
        return cfg_relative
    return REPO_ROOT / path


def _resolve_output_path(path_value: str) -> Path:
    path = Path(path_value)
    if path.is_absolute():
        return path
    return REPO_ROOT / path


def _optional_output_path(path_value: str | None) -> Path | None:
    if not path_value:
        return None
    return _resolve_output_path(path_value)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)


def _normalise_record(record: dict[str, Any]) -> dict[str, Any]:
    return {name: _normalise_value(value) for name, value in record.items()}


def _normalise_value(value: Any) -> Any:
    if hasattr(value, "item"):
        value = value.item()
    if not isinstance(value, (list, tuple, dict)) and pd.isna(value):
        return None
    return value


def _coerce_number(value: int | float) -> int | float:
    if isinstance(value, float) and value.is_integer():
        return int(value)
    return value


def _raise_case_error(error: Exception) -> None:
    raise RuntimeError("parametric_run failed while running a generated case") from error
