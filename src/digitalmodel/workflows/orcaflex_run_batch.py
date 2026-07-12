"""Licensed OrcaFlex dynamic time-domain batch workflow (issue #1554).

One input.yml -> N OrcaFlex simulations run CONCURRENTLY: composes
parametric_run's case generation (factorial/range/csv/yaml_matrix) with the
existing OrcaFlexParallelAnalysis ProcessPool executor. Worker count defaults
to ~90% of host cores (owner resource policy, dm#1553); OrcaFlex sims are
single-threaded each, so real runs use processes.

Small conclusions (results/cases.csv + results/batch_summary.json) fit the
licensed-run queue return allowlist; .sim files stay on the host.
"""

from __future__ import annotations

import math
import json
import os
from copy import deepcopy
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

import pandas as pd
import yaml
from loguru import logger

from digitalmodel.solvers.orcaflex.orcaflex_parallel_analysis import (
    OrcaFlexParallelAnalysis,
)
from digitalmodel.workflows.parametric_run import _load_cases, _set_dotted

# Owner resource policy (dm#1553): OrcaFlex = multiple-batch mode sized to the
# host's cores; each sim is single-threaded, so size the pool to ~90% of them.
WORKER_CORE_FRACTION = 0.9
DEFAULT_ANALYSIS_TYPE = "both"
DEFAULT_OUTPUT_DIR = "results"
DEFAULT_WORK_DIR = "batch_runs"
CASE_FILENAME_PATTERN = "case_{index}_{stem}.yml"
# OrcaFlex model YAML keeps stage durations under General.StageDuration
# ([build-up, stage-1, ...]); the duration override targets the main stage.
DEFAULT_DURATION_PATH = "General.StageDuration.1"
MOCK_SIM_CONTENT = "mock OrcaFlex simulation (orcaflex_run_batch mock mode)\n"
LICENSE_ERROR_MESSAGE = (
    "OrcaFlex license / OrcFxAPI is not available on this host. "
    "orcaflex_run_batch is a requires-license workflow: dispatch it to a "
    "licensed host (deckhand licensed-run lane) or set run_batch.mock: true "
    "for a license-free dry run."
)

_ANALYSIS_FLAGS = {
    "statics": {"static": True, "dynamic": False},
    "dynamics": {"static": False, "dynamic": True},
    "both": {"static": True, "dynamic": True},
}
_YAML_SUFFIXES = {".yml", ".yaml"}
_STATUS_LABELS = {"success": "completed"}


def router(cfg: dict) -> dict:
    settings = cfg.get("orcaflex_run_batch") or {}
    cfg_dir = Path(cfg.get("_config_dir_path") or Path.cwd())

    run_settings = settings.get("run_batch") or {}
    mock = bool(run_settings.get("mock", False))
    save_sim = bool(run_settings.get("save_sim", True))
    workers = resolve_workers(run_settings)

    if not mock and not _license_available():
        raise RuntimeError(LICENSE_ERROR_MESSAGE)

    model_files = _resolve_models(settings.get("models") or {}, cfg_dir)
    variants = settings.get("variants") or {}
    cases = _load_cases(variants, cfg_dir) if variants else [{}]
    mapping = variants.get("mapping") or {}

    analysis = settings.get("analysis") or {}
    analysis_type = analysis.get("type", DEFAULT_ANALYSIS_TYPE)
    if analysis_type not in _ANALYSIS_FLAGS:
        raise ValueError(
            "orcaflex_run_batch analysis.type must be statics|dynamics|both, "
            f"got {analysis_type}"
        )

    results_dir = _resolve_dir(
        run_settings.get("output_dir", DEFAULT_OUTPUT_DIR), cfg_dir
    )
    work_dir = _resolve_dir(run_settings.get("work_dir", DEFAULT_WORK_DIR), cfg_dir)

    rendered = _render_cases(
        model_files=model_files,
        cases=cases,
        mapping=mapping,
        duration=analysis.get("duration"),
        duration_path=analysis.get("duration_path", DEFAULT_DURATION_PATH),
        work_dir=work_dir,
    )

    # Real sims are single-threaded OrcFxAPI solves -> ProcessPool. The mock
    # leaf worker is trivial I/O -> threads (no process-spawn overhead in CI).
    executor_class = _MockOrcaFlexBatch if mock else OrcaFlexParallelAnalysis
    executor = executor_class(num_threads=workers, use_processes=not mock)
    executor_config = {
        **_ANALYSIS_FLAGS[analysis_type],
        "save_sim": save_sim,
        "save_dat": False,
        "output_dir": str(work_dir / "sims"),
    }

    started_at = datetime.now(timezone.utc)
    pool_summary = executor.process_files_parallel(
        [str(item["input_file"]) for item in rendered], executor_config
    )
    finished_at = datetime.now(timezone.utc)

    rows = _manifest_rows(rendered, pool_summary)
    manifest_path = results_dir / "cases.csv"
    summary_path = results_dir / "batch_summary.json"
    _write_manifest(rows, manifest_path)
    summary = _write_summary(
        rows=rows,
        path=summary_path,
        pool_summary=pool_summary,
        workers=workers,
        mock=mock,
        analysis_type=analysis_type,
        save_sim=save_sim,
        started_at=started_at,
        finished_at=finished_at,
    )
    if summary["failed"]:
        logger.warning(
            "orcaflex_run_batch: {} of {} cases failed; see {}",
            summary["failed"],
            summary["total_cases"],
            manifest_path,
        )

    settings["cases"] = rows
    settings["outputs"] = {
        "manifest": str(manifest_path),
        "summary": str(summary_path),
    }
    cfg["orcaflex_run_batch"] = settings
    return cfg


def default_workers(cpu_count: int | None = None) -> int:
    """~90% of host cores, floored, never below 1 (dm#1553 resource policy)."""
    cores = cpu_count if cpu_count is not None else (os.cpu_count() or 1)
    return max(1, math.floor(WORKER_CORE_FRACTION * cores))


def resolve_workers(run_settings: dict) -> int:
    explicit = run_settings.get("workers")
    if explicit is None:
        return default_workers()
    workers = int(explicit)
    if workers < 1:
        raise ValueError(f"run_batch.workers must be >= 1, got {explicit}")
    return workers


def _license_available() -> bool:
    try:
        from digitalmodel.solvers.orcaflex.orcaflex_utilities import (
            OrcaflexUtilities,
        )
    except Exception:
        return False
    try:
        return bool(OrcaflexUtilities().is_orcaflex_available())
    except Exception:
        return False


def _resolve_models(models: dict, cfg_dir: Path) -> list[Path]:
    files = models.get("files")
    if files:
        # Missing explicit files surface as per-case failures in cases.csv
        # (the batch continues); glob patterns only ever match existing files.
        return [_resolve_path(item, cfg_dir) for item in files]
    pattern = models.get("pattern")
    if not pattern:
        raise ValueError(
            "orcaflex_run_batch.models needs files: [...] or pattern: '*.yml'"
        )
    directory = _resolve_path(models.get("directory", "."), cfg_dir)
    matched = sorted(directory.glob(pattern))
    if not matched:
        raise ValueError(
            f"orcaflex_run_batch: no model files match {pattern} in {directory}"
        )
    return matched


def _render_cases(
    model_files: list[Path],
    cases: list[dict[str, Any]],
    mapping: dict[str, str],
    duration: float | None,
    duration_path: str,
    work_dir: Path,
) -> list[dict[str, Any]]:
    rendered: list[dict[str, Any]] = []
    index = 0
    for model_path in model_files:
        for case in cases:
            needs_injection = bool(case) or duration is not None
            if needs_injection:
                if model_path.suffix.lower() not in _YAML_SUFFIXES:
                    raise ValueError(
                        "orcaflex_run_batch variants/duration need a YAML "
                        f"OrcaFlex model to inject into, got {model_path.name}"
                    )
                case_model = deepcopy(_read_model_yaml(model_path))
                for name, value in case.items():
                    _set_dotted(case_model, mapping.get(name, name), value)
                if duration is not None:
                    _set_dotted(case_model, duration_path, duration)
                case_file = work_dir / "cases" / CASE_FILENAME_PATTERN.format(
                    index=index, stem=model_path.stem
                )
                case_file.parent.mkdir(parents=True, exist_ok=True)
                case_file.write_text(yaml.safe_dump(case_model, sort_keys=False))
            else:
                case_file = model_path
            rendered.append(
                {
                    "index": index,
                    "model": model_path.name,
                    "case": case,
                    "input_file": case_file,
                }
            )
            index += 1
    return rendered


def _manifest_rows(
    rendered: list[dict[str, Any]], pool_summary: dict
) -> list[dict[str, Any]]:
    by_path = {
        result["file_path"]: result for result in pool_summary.get("results", [])
    }
    rows = []
    for item in rendered:
        result = by_path.get(str(item["input_file"]), {})
        sim_path = next(
            (f for f in result.get("output_files") or [] if f.endswith(".sim")),
            None,
        )
        rows.append(
            {
                "index": item["index"],
                "model": item["model"],
                **item["case"],
                "input_file": str(item["input_file"]),
                "status": _STATUS_LABELS.get(result.get("status"), "failed"),
                "error": result.get("error"),
                "wall_seconds": round(result.get("duration") or 0.0, 3),
                "sim_path": sim_path,
            }
        )
    return rows


def _write_manifest(rows: list[dict[str, Any]], path: Path) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    pd.DataFrame(rows).to_csv(path, index=False)


def _write_summary(
    rows: list[dict[str, Any]],
    path: Path,
    pool_summary: dict,
    workers: int,
    mock: bool,
    analysis_type: str,
    save_sim: bool,
    started_at: datetime,
    finished_at: datetime,
) -> dict:
    completed = sum(1 for row in rows if row["status"] == "completed")
    summary = {
        "workflow": "orcaflex_run_batch",
        "total_cases": len(rows),
        "completed": completed,
        "failed": len(rows) - completed,
        "workers": workers,
        "host_cpu_count": os.cpu_count(),
        "mock": mock,
        "analysis_type": analysis_type,
        "save_sim": save_sim,
        "wall_seconds_total": round(pool_summary.get("total_duration") or 0.0, 3),
        "parallel_speedup": round(pool_summary.get("parallel_speedup") or 1.0, 3),
        "started_at_utc": started_at.isoformat(),
        "finished_at_utc": finished_at.isoformat(),
    }
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(summary, indent=2) + "\n")
    return summary


def _read_model_yaml(path: Path) -> dict:
    with path.open() as stream:
        loaded = yaml.safe_load(stream) or {}
    if not isinstance(loaded, dict):
        raise ValueError(f"Expected mapping YAML OrcaFlex model in {path}")
    return loaded


def _resolve_path(path_value: str, cfg_dir: Path) -> Path:
    path = Path(path_value)
    if path.is_absolute():
        return path
    return cfg_dir / path


def _resolve_dir(path_value: str, cfg_dir: Path) -> Path:
    resolved = _resolve_path(path_value, cfg_dir)
    resolved.mkdir(parents=True, exist_ok=True)
    return resolved


class _MockOrcaFlexBatch(OrcaFlexParallelAnalysis):
    """License-free stand-in: inherits the pool/summary/failure-isolation
    machinery from OrcaFlexParallelAnalysis and only swaps the leaf worker
    (no OrcFxAPI; writes a marker .sim so sim_path plumbing is exercised)."""

    def process_single_file(self, file_info: dict[str, Any]) -> dict[str, Any]:
        file_path = file_info["file_path"]
        config = file_info.get("config", {})
        result = {
            "file_path": file_path,
            "status": "pending",
            "start_time": datetime.now(),
            "end_time": None,
            "duration": None,
            "error": None,
            "output_files": [],
        }
        try:
            path = Path(file_path)
            if not path.exists():
                raise FileNotFoundError(f"Model file not found: {file_path}")
            if config.get("static", True):
                result["static_complete"] = True
            if config.get("dynamic", False):
                result["dynamic_complete"] = True
            if config.get("save_sim", True):
                output_dir = Path(config.get("output_dir") or path.parent)
                output_dir.mkdir(parents=True, exist_ok=True)
                sim_path = output_dir / f"{path.stem}.sim"
                sim_path.write_text(MOCK_SIM_CONTENT)
                result["output_files"].append(str(sim_path))
            result["status"] = "success"
        except Exception as exc:  # per-case isolation: the batch continues
            result["status"] = "failed"
            result["error"] = str(exc)
        finally:
            result["end_time"] = datetime.now()
            result["duration"] = (
                result["end_time"] - result["start_time"]
            ).total_seconds()
        return result
