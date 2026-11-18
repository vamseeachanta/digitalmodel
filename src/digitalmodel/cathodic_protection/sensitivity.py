# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Run one-at-a-time B401 sensitivities without changing engine calculations."""

from __future__ import annotations

import argparse
from collections.abc import Mapping, Sequence
from copy import deepcopy
import json
import math
from pathlib import Path
from typing import Any

import yaml

from digitalmodel.cathodic_protection.engine_adapter import (
    KEY_B401,
    run_cathodic_protection,
)

PARAMETER_UNITS = {
    "environment.seawater_temperature_C": "degC",
    "design_data.design_life": "yr",
    "environment.seawater_resistivity_ohm_m": "ohm.m",
}
MODES = ("published", "standard_tables")


def _number(value: Any, name: str, *, positive: bool = False) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} requires a finite number")
    if not math.isfinite(value) or (positive and value <= 0):
        raise ValueError(f"{name} requires a finite {'positive ' if positive else ''}number")
    return float(value)


def _read_mapping(path: Path) -> dict[str, Any]:
    with path.open(encoding="utf-8") as stream:
        data = yaml.safe_load(stream)
    if not isinstance(data, dict):
        raise ValueError("sweep config and base input must be mappings")
    return data


def load_sweep(config: str | Path) -> tuple[dict[str, Any], dict[str, Any]]:
    """Load a config and resolve its base input relative to the config file."""
    path = Path(config)
    spec = _read_mapping(path)
    base_name = spec.get("base_input")
    if not isinstance(base_name, str) or not base_name.strip():
        raise ValueError("base_input is required")
    return spec, _read_mapping(path.parent / base_name)


def _mode(spec: Mapping[str, Any], mode: str | None) -> str:
    selected = spec.get("mode") if mode is None else mode
    if selected not in MODES:
        raise ValueError("mode must be published or standard_tables")
    return str(selected)


def _parameter_target(base: Mapping[str, Any], path: str) -> tuple[str, str]:
    if path not in PARAMETER_UNITS:
        raise ValueError(f"unsupported sensitivity parameter: {path}")
    section, key = path.split(".")
    node = base.get("inputs", {}).get(section, {})
    if not isinstance(node, Mapping) or key not in node:
        raise ValueError(f"{path} is required in the base input")
    _number(node[key], path, positive=path != "environment.seawater_temperature_C")
    return section, key


def _validate_zone_depth(structure: Mapping[str, Any], name: str, depth: float) -> None:
    geometry = structure.get("report_geometry")
    if geometry is None:
        return
    if not isinstance(geometry, Mapping):
        raise ValueError("report geometry must be a mapping")
    if "zones" not in geometry:
        return
    bands = geometry["zones"]
    if (not isinstance(bands, list) or not bands
            or any(not isinstance(band, Mapping) for band in bands)):
        raise ValueError("report geometry zones must be a non-empty list of mappings")
    matches = [band for band in bands if band.get("calculation_zone") == name]
    if len(matches) != 1:
        raise ValueError("report geometry requires one matching calculation zone")
    lower = _number(matches[0].get("lower_m"), "report geometry lower bound")
    upper = _number(matches[0].get("upper_m"), "report geometry upper bound")
    if lower >= upper:
        raise ValueError("report geometry lower bound must be below upper bound")
    # Geometry uses elevations; table lookups use positive-down seawater depths.
    # A shared boundary belongs to the shallower zone (B401 2021, Table 8-1,
    # 0-30 m versus >30-100 m); the surface at zero depth remains included.
    if not lower <= -depth <= upper or (upper < 0 and -depth == upper):
        raise ValueError("standard_table_zones depth is outside its zone bounds")


def prepare_base(
    base: Mapping[str, Any], spec: Mapping[str, Any], mode: str,
) -> dict[str, Any]:
    """Copy a single-structure B401 input and apply the declared mode only."""
    _mode(spec, mode)
    cfg = deepcopy(dict(base))
    inputs = cfg.get("inputs")
    if not isinstance(inputs, dict) or inputs.get("calculation_type") != KEY_B401:
        raise ValueError("sensitivity requires DNV_RP_B401_offshore inputs")
    if any(inputs.get(key) is not None for key in
           ("components", "anode_families", "riser_base_assessment")):
        raise ValueError("sensitivity requires a single-structure input")
    for path in PARAMETER_UNITS:
        _parameter_target(cfg, path)
    count = inputs.get("anode", {}).get("count")
    if type(count) is not int or count < 1:
        raise ValueError("installed anode count is required as a positive integer")
    for zone in inputs.get("structure", {}).get("zones", []):
        if "effective_current_density_A_m2" in zone:
            densities = zone["effective_current_density_A_m2"]
            if not isinstance(densities, Mapping) or set(densities) != {"initial", "mean", "final"}:
                raise ValueError("published density requires initial, mean and final values")
            for value in densities.values():
                if _number(value, "published density") < 0:
                    raise ValueError("published density must be non-negative")
    if mode == "published":
        return cfg
    depths = spec.get("standard_table_zones")
    if not isinstance(depths, Mapping) or len(depths) != 2:
        raise ValueError("standard_tables requires two bare seawater zones and depths")
    zones = inputs.get("structure", {}).get("zones", [])
    for name, depth in depths.items():
        _number(depth, "standard_table_zones depth")
        if depth < 0:
            raise ValueError("standard_table_zones depth must be non-negative")
        matches = [zone for zone in zones if zone.get("zone") == name]
        if len(matches) != 1:
            raise ValueError("standard_tables requires two bare seawater zones present once")
        zone = matches[0]
        if (zone.get("base_zone", zone.get("zone")) != "submerged"
                or zone.get("coating_category") != "bare"
                or not isinstance(zone.get("effective_current_density_A_m2"), Mapping)):
            raise ValueError("standard_tables requires two bare seawater zones with published densities")
        _validate_zone_depth(inputs["structure"], name, depth)
        del zone["effective_current_density_A_m2"]
        zone.pop("density_source", None)
        zone["depth_m"] = depth
    return cfg


def _case_row(result: Mapping[str, Any], mode: str, path: str, value: float) -> dict[str, Any]:
    ver = result["current_output_verification"]
    status = result["status"]
    flag_names = ("adequate", "initial_meets_demand", "final_meets_demand", "mass_meets_requirement")
    if any(type(ver.get(key)) is not bool for key in flag_names):
        raise ValueError("engine adequacy flags are not evaluated")
    row = {
        "mode": mode, "parameter": path, "value": value, "unit": PARAMETER_UNITS[path],
        "initial_current_A": ver["initial_current_demand_A"],
        "mean_current_A": ver["mean_current_demand_A"],
        "final_current_A": ver["final_current_demand_A"],
        "required_net_mass_kg": result["anode_requirements"]["total_mass_kg"],
        "recommended_anode_count": ver["recommended_anode_count"],
        "adequacy": deepcopy(ver), "status": deepcopy(status),
    }
    for key in ("initial_current_A", "mean_current_A", "final_current_A", "required_net_mass_kg"):
        _number(row[key], key, positive=True)
    return row


def run_sweep(
    config: str | Path | Mapping[str, Any], *,
    mode: str | None = None, base: Mapping[str, Any] | None = None,
) -> list[dict[str, Any]]:
    """Return one row per configured value, changing one parameter at a time.

    A mapping config needs an explicit base mapping. File configs resolve the
    base from their own directory. Inputs are copied; no source file is written.
    Invalid or non-applicable parameters raise instead of producing a pass.
    """
    if isinstance(config, Mapping):
        spec = dict(config)
        if base is None:
            raise ValueError("base input is required with a mapping config")
    else:
        spec, base = load_sweep(config)
    selected = _mode(spec, mode)
    prepared = prepare_base(base, spec, selected)
    parameters = spec.get("parameters")
    if not isinstance(parameters, list) or not parameters:
        raise ValueError("parameters must be a non-empty list")
    cases: list[tuple[str, float]] = []
    seen: set[str] = set()
    for parameter in parameters:
        if not isinstance(parameter, Mapping):
            raise ValueError("each parameter requires a path and values")
        path = parameter.get("path")
        if not isinstance(path, str):
            raise ValueError("parameter path is required")
        _parameter_target(prepared, path)
        if path in seen:
            raise ValueError("parameter paths must be unique")
        seen.add(path)
        values = parameter.get("values")
        if not isinstance(values, list) or not values:
            raise ValueError("parameter values must be a non-empty list")
        for value in values:
            cases.append((path, _number(value, path, positive=path != "environment.seawater_temperature_C")))
    rows = []
    for path, value in cases:
        case = deepcopy(prepared)
        section, key = path.split(".")
        case["inputs"][section][key] = value
        result = run_cathodic_protection(case)["results"]
        row = _case_row(result, selected, path, value)
        row["basis"] = {
            "base_input": Path(str(spec.get("base_input", "in-memory input"))).name,
            "standard": result["standard"],
        }
        rows.append(row)
    return rows


def sweep_report(config: str | Path, *, mode: str | None = None, rows=None):
    """Build the existing base-input analysis report with a sensitivity section."""
    from digitalmodel.cathodic_protection.report_adapters import anode_design_report

    spec, base = load_sweep(config)
    selected = _mode(spec, mode)
    if rows is None:
        rows = run_sweep(spec, base=base, mode=selected)
    cfg = run_cathodic_protection(deepcopy(base))
    cfg.setdefault("report", {})["sensitivity"] = rows
    return anode_design_report(cfg)


def _write(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8", newline="\n")


def main(argv: Sequence[str] | None = None) -> int:
    """Write JSON rows and optionally the existing HTML report with sensitivities."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("config", type=Path)
    parser.add_argument("--mode", choices=MODES)
    parser.add_argument("--output", type=Path, help="JSON rows; default is standard output")
    parser.add_argument("--report", type=Path, help="HTML analysis with sensitivity table")
    args = parser.parse_args(argv)
    spec, _ = load_sweep(args.config)
    paths = [args.config.resolve(), (args.config.parent / spec["base_input"]).resolve()]
    paths.extend(path.resolve() for path in (args.output, args.report) if path is not None)
    if len(paths) != len(set(paths)):
        raise ValueError("config, base input and output paths must be distinct")
    rows = run_sweep(args.config, mode=args.mode)
    payload = json.dumps(rows, indent=2, allow_nan=False) + "\n"
    if args.output:
        _write(args.output, payload)
    else:
        print(payload, end="")
    if args.report:
        from digitalmodel.reporting import render_html

        _write(args.report, render_html(sweep_report(args.config, mode=args.mode, rows=rows)))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
