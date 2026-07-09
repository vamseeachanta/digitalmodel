"""UV workflow router for response_spectrum accelerogram preprocessing."""

from __future__ import annotations

from pathlib import Path
from typing import Any

from digitalmodel.reporting import DataSource
from digitalmodel.seismic.readers.ascii import read_ascii_accelerogram
from digitalmodel.seismic.response_spectrum.calculations import compute_kinematics
from digitalmodel.seismic.response_spectrum.render import render_report

REPO_ROOT = Path(__file__).resolve().parents[4]


def router(cfg: dict) -> dict:
    """Run PR1 response_spectrum preprocessing and update ``cfg``."""

    settings = cfg.get("response_spectrum") or {}
    record_cfg = settings.get("record") or {}
    record_path = _config_path(cfg, record_cfg["file"])
    record = read_ascii_accelerogram(
        record_path,
        units=record_cfg.get("units"),
        dt_s=record_cfg.get("dt_s"),
    )
    result = compute_kinematics(
        record,
        baseline=settings.get("baseline"),
        frequency_filter=settings.get("filter"),
    )
    output_dir = _output_dir(cfg, settings)
    stem = _input_stem(cfg)
    timeseries_csv = output_dir / f"{stem}_timeseries.csv"
    report_html = output_dir / f"{stem}_report.html"
    _write_timeseries(timeseries_csv, result.timeseries)
    render_report(
        result,
        output_path=report_html,
        data_source=_data_source(record),
        generated_label=settings.get("generated_label"),
    )
    cfg["screening_status"] = "pass"
    cfg["response_spectrum"] = _payload(result, timeseries_csv, report_html)
    cfg.setdefault("outputs", {})["timeseries_csv"] = _display_path(timeseries_csv)
    cfg["outputs"]["report_html"] = _display_path(report_html)
    return cfg


def _payload(result, timeseries_csv: Path, report_html: Path) -> dict[str, Any]:
    return {
        "screening_status": "pass",
        "method": result.method,
        "summary": result.summary,
        "processing_chain": result.processing_chain,
        "timeseries_csv": _display_path(timeseries_csv),
        "report_html": _display_path(report_html),
    }


def _data_source(record) -> DataSource:
    return DataSource(
        kind="file",
        identifier=_source_identifier(record.source_path, record.source_digest),
        digest=f"sha256:{record.source_digest}",
        description=f"Accelerogram, units={record.units}, npts={record.npts}",
    )


def _source_identifier(path: Path, digest: str) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return f"external-file:{resolved.name}:sha256:{digest}"


def _write_timeseries(path: Path, timeseries) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    timeseries.to_csv(path, index=False, float_format="%.10g")


def _output_dir(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(str(settings.get("output_dir", "results")))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir


def _config_path(cfg: dict, value: str) -> Path:
    path = Path(str(value))
    if not path.is_absolute():
        path = _config_dir(cfg) / path
    return path


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "response_spectrum"))


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
