"""Engine runner for the wall-thickness quickcheck workflow."""

from __future__ import annotations

import csv
import importlib.util
import json
from pathlib import Path
from typing import Any


def _repo_root() -> Path:
    return Path(__file__).resolve().parents[3]


def _load_quickcheck_module():
    module_path = (
        _repo_root()
        / "examples"
        / "structural"
        / "wall_thickness_quickcheck"
        / "quick_check.py"
    )
    spec = importlib.util.spec_from_file_location(
        "digitalmodel_wall_thickness_quick_check", module_path
    )
    if spec is None or spec.loader is None:
        raise ImportError(f"Cannot load wall-thickness quickcheck: {module_path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def _resolve_path(config_dir: Path, value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return config_dir / path


def _selection_rows(payload: dict[str, Any]) -> list[dict[str, Any]]:
    rows = []
    for branch, selection in payload["selection"].items():
        rows.append(
            {
                "branch": branch,
                "selected_standard_label": selection["selected_standard_label"],
                "selected_standard_wall_mm": selection["selected_standard_wall_mm"],
                "selected_standard_wall_in": selection["selected_standard_wall_in"],
                "nonstandard_minimum_wall_mm": selection[
                    "nonstandard_minimum_wall_mm"
                ],
                "governing_check": selection["governing_check"],
                "governing_utilisation": selection["governing_utilisation"],
            }
        )
    return rows


def _write_selection_csv(payload: dict[str, Any], output_path: Path) -> None:
    rows = _selection_rows(payload)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)


class WallThicknessQuickCheck:
    """Thin adapter around the existing deckhand quickcheck scripts."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        config_dir = Path(cfg.get("_config_dir_path", Path.cwd()))
        quickcheck_cfg = cfg.get("wall_thickness", {}).get("quickcheck", {})
        cache_path = _resolve_path(config_dir, quickcheck_cfg["cache"])
        html_path = _resolve_path(config_dir, quickcheck_cfg["output_html"])
        json_path = _resolve_path(config_dir, quickcheck_cfg["output_json"])
        csv_path = _resolve_path(config_dir, quickcheck_cfg["output_csv"])

        quick_check = _load_quickcheck_module()
        payload = quick_check.run_from_cache(cache_path)
        html_path.parent.mkdir(parents=True, exist_ok=True)
        quick_check.write_report(payload, html_path)
        json_path.parent.mkdir(parents=True, exist_ok=True)
        json_path.write_text(
            json.dumps(payload, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
        _write_selection_csv(payload, csv_path)

        cfg.setdefault("wall_thickness", {})
        cfg["wall_thickness"]["quickcheck"] = {
            **quickcheck_cfg,
            "report_html": str(html_path),
            "result_json": str(json_path),
            "result_csv": str(csv_path),
            "selection": payload["selection"],
            "buckle_arrestor_sizing": payload["buckle_arrestor_sizing"],
        }
        return cfg
