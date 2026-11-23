"""UV-workflow router for offline CSV/YAML tabular comparison overlays."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import pandas as pd
import yaml


REPO_ROOT = Path(__file__).resolve().parents[3]


def router(cfg: dict) -> dict:
    settings = cfg.get("compare_tool") or {}
    key = _required_text(settings, "key")
    value = _required_text(settings, "value")
    sources = _sources(settings)

    tables = [
        _read_source(_source_path(cfg, source), source, key, value)
        for source in sources
    ]
    comparison = tables[0]
    for table in tables[1:]:
        comparison = comparison.merge(table, on=key, how="outer")
    comparison = comparison.sort_values(key).reset_index(drop=True)

    baseline_label = str(sources[0]["label"])
    baseline_column = f"{baseline_label}_{value}"
    max_abs_delta = {}
    for source in sources[1:]:
        label = str(source["label"])
        source_column = f"{label}_{value}"
        delta_column = f"{label}_minus_{baseline_label}"
        ratio_column = f"{label}_ratio"
        comparison[delta_column] = (
            comparison[source_column] - comparison[baseline_column]
        )
        comparison[ratio_column] = comparison[source_column] / comparison[baseline_column]
        max_delta = comparison[delta_column].abs().max()
        max_abs_delta[label] = None if pd.isna(max_delta) else float(max_delta)

    csv_path = _comparison_csv_path(cfg, settings)
    csv_path.parent.mkdir(parents=True, exist_ok=True)
    comparison.to_csv(csv_path, index=False)

    cfg["compare_tool"] = {
        "key": key,
        "value": value,
        "n_sources": len(sources),
        "n_rows": len(comparison),
        "columns": list(comparison.columns),
        "max_abs_delta": max_abs_delta,
        "comparison_csv": _display_path(csv_path),
    }
    return cfg


def _read_source(path: Path, source: dict[str, Any], key: str, value: str) -> pd.DataFrame:
    label = str(source["label"])
    if path.suffix.lower() == ".csv":
        table = pd.read_csv(path)
    elif path.suffix.lower() in {".yml", ".yaml"}:
        table = _read_yaml_source(path, key, value)
    else:
        raise ValueError(f"compare_tool unsupported source file type: {path}")

    missing = [column for column in (key, value) if column not in table.columns]
    if missing:
        raise ValueError(f"compare_tool source {label} missing column(s): {missing}")
    if table[key].duplicated().any():
        raise ValueError(f"compare_tool source {label} has duplicate key values")

    value_column = f"{label}_{value}"
    return table[[key, value]].rename(columns={value: value_column})


def _read_yaml_source(path: Path, key: str, value: str) -> pd.DataFrame:
    data = yaml.safe_load(path.read_text())
    if isinstance(data, list):
        return pd.DataFrame(data)
    if isinstance(data, dict):
        rows = [
            {key: item_key, value: item_value}
            for item_key, item_value in data.items()
        ]
        return pd.DataFrame(rows)
    raise ValueError("compare_tool YAML sources must be a list or mapping")


def _sources(settings: dict[str, Any]) -> list[dict[str, Any]]:
    sources = settings.get("sources")
    if not isinstance(sources, list) or len(sources) < 2:
        raise ValueError("compare_tool requires at least two sources")

    seen = set()
    for source in sources:
        if not isinstance(source, dict):
            raise ValueError("compare_tool sources must be mappings")
        label = _required_text(source, "label")
        _required_text(source, "file")
        if label in seen:
            raise ValueError("compare_tool source labels must be unique")
        seen.add(label)
    return sources


def _source_path(cfg: dict, source: dict[str, Any]) -> Path:
    path = Path(str(source["file"]))
    if not path.is_absolute():
        path = _config_dir(cfg) / path
    return path


def _comparison_csv_path(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(settings.get("output_dir", "results"))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    if cfg.get("_config_file_path"):
        stem = Path(cfg["_config_file_path"]).stem
    else:
        stem = str(cfg.get("basename", "compare_tool"))
    return output_dir / f"{stem}_comparison.csv"


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _required_text(settings: dict[str, Any], name: str) -> str:
    value = settings.get(name)
    if value is None or str(value).strip() == "":
        raise ValueError(f"compare_tool {name} is required")
    return str(value)


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)
