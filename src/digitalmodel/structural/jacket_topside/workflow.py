"""Engine-routed API RP 2A-WSD jacket/topside capacity workflow."""

from __future__ import annotations

import csv
import json
from dataclasses import asdict
from pathlib import Path
from typing import Any

from .joint_checks import JointGeometry, JointLoads, punching_shear_uc
from .member_checks import MemberLoads, TubularSection, member_checks


def _resolve_path(cfg: dict[str, Any], value: str | Path) -> Path:
    path = Path(value)
    if path.is_absolute():
        return path
    return Path(cfg.get("_config_dir_path", Path.cwd())) / path


def _member_row(item: dict[str, Any]) -> dict[str, Any]:
    section = TubularSection(**item["section"])
    loads = MemberLoads(**item["loads"])
    results = member_checks(section, loads)
    checks = {
        name: asdict(result)
        for name, result in results.items()
        if name != "governing"
    }
    governing = asdict(results["governing"])
    return {
        "member_id": item["id"],
        "status": "PASS" if governing["passes"] else "FAIL",
        "governing_check": governing["check_type"],
        "governing_uc": governing["unity_check"],
        "governing": governing,
        "checks": checks,
    }


def _joint_row(item: dict[str, Any]) -> dict[str, Any]:
    geometry = JointGeometry(**item["geometry"])
    loads = JointLoads(**item["loads"])
    result = punching_shear_uc(
        geometry,
        loads,
        chord_fy=item["chord_fy"],
        brace_index=item.get("brace_index", 0),
        chord_utilization=item.get("chord_utilization", 0.0),
    )
    details = dict(result.details)
    return {
        "joint_id": item["id"],
        "status": "PASS" if result.passes else "FAIL",
        "joint_type": result.joint_type.value,
        "applied_vp": result.applied_vp,
        "allowable_vp": result.allowable_vp,
        "unity_check": result.unity_check,
        "passes": result.passes,
        "details": details,
    }


def _write_csv(rows: list[dict[str, Any]], path: Path, fields: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields)
        writer.writeheader()
        for row in rows:
            writer.writerow({field: row[field] for field in fields})


class JacketChecksWorkflow:
    """Thin adapter over existing tubular member and joint check functions."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        workflow_cfg = cfg.get("jacket_checks", {})
        members = [_member_row(item) for item in workflow_cfg.get("members", [])]
        joints = [_joint_row(item) for item in workflow_cfg.get("joints", [])]
        passes = all(item["status"] == "PASS" for item in members + joints)

        result = {
            "overall_status": "PASS" if passes else "FAIL",
            "members": members,
            "joints": joints,
        }
        cfg["jacket_checks"] = result
        self._write_outputs(cfg, workflow_cfg.get("output", {}), result)
        return cfg

    def _write_outputs(
        self,
        cfg: dict[str, Any],
        output_cfg: dict[str, str],
        result: dict[str, Any],
    ) -> None:
        outputs = cfg.setdefault("outputs", {})
        if "summary_json" in output_cfg:
            path = _resolve_path(cfg, output_cfg["summary_json"])
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(json.dumps(result, indent=2), encoding="utf-8")
            outputs["summary_json"] = str(path)
        if "members_csv" in output_cfg:
            path = _resolve_path(cfg, output_cfg["members_csv"])
            _write_csv(
                result["members"],
                path,
                ["member_id", "status", "governing_check", "governing_uc"],
            )
            outputs["members_csv"] = str(path)
        if "joints_csv" in output_cfg:
            path = _resolve_path(cfg, output_cfg["joints_csv"])
            _write_csv(
                result["joints"],
                path,
                ["joint_id", "status", "joint_type", "unity_check"],
            )
            outputs["joints_csv"] = str(path)
