#!/usr/bin/env python3
# ABOUTME: Deckhand wall-thickness quickcheck sweep with DNV/API utilization plots.
# ABOUTME: Deterministic cache mode, arrestor branch headline, and self-contained HTML.
"""Pipeline Wall Thickness — Utilization Sweep Quick Check.

Stable command:
    PYTHONPATH=src uv run python \
        examples/structural/wall_thickness_quickcheck/quick_check.py --from-cache

``--compute`` refreshes the JSON cache from the live wall-thickness engine.
``--from-cache`` loads only the committed fixture, then renders the same summary
and report from cached data. The final stdout line is ``ARTIFACT: <abs path>``.
"""
from __future__ import annotations

import argparse
import importlib.util
import json
import math
import sys
from pathlib import Path
from typing import Any

HERE = Path(__file__).resolve().parent
CACHE_PATH = HERE / "data" / "quickcheck_cache.json"
DEFAULT_OUTPUT = HERE / "output" / "wall_thickness_quickcheck.html"

INCH_TO_MM = 25.4
SEAWATER_DENSITY = 1025.0  # kg/m^3
GRAVITY = 9.81  # m/s^2
WATER_DEPTH_M = 1500.0  # m

CASE: dict[str, Any] = {
    "label": "12.75 in OD X65 export line, 1500 m water depth",
    "pressure_basis": "design",
    "buckle_arrestors": "with",
    "geometry": {
        "outer_diameter": 0.3239,  # m; 12.75 in OD
        "corrosion_allowance": 0.003,  # m; 3 mm
    },
    "material": {
        "grade": "X65",
        "smys": 448e6,  # Pa
        "smts": 531e6,  # Pa
    },
    "loads": {
        "internal_pressure": 150e5,  # Pa; 150 bar design pressure
    },
    "water_depth_m": WATER_DEPTH_M,
    "codes": ["DNV-ST-F101", "API-RP-1111"],
}

PROPAGATION_CHECKS = {
    "DNV-ST-F101": {"propagation_buckling"},
    "API-RP-1111": {"propagation"},
}

# Per-check load premises (deckhand#227). A single load pair (full internal +
# full external simultaneously) is NOT a design screen: net-pressure burst at
# the seabed is ~zero (it hid containment entirely, utilisation < 0), and
# collapse must hold for the empty/depressurised line. Each check is taken
# from the engine run matching its design premise:
#   burst/containment  ->  p_i = design pressure, p_e = 0 (route minimum:
#                          shore approach / hydrotest above water)
#   collapse, propagation, combined  ->  p_i = 0 (empty line), p_e = full depth
BURST_CHECKS = {
    "DNV-ST-F101": {"pressure_containment"},
    "API-RP-1111": {"burst"},
}
LOAD_PREMISES = (
    "burst/containment checked at p_i = design pressure with zero external "
    "pressure (route minimum); collapse and propagation checked for the "
    "empty line (p_i = 0) at full water depth"
)


def external_pressure_from_depth(depth_m: float) -> float:
    """Hydrostatic external pressure (Pa) at a given water depth."""
    return SEAWATER_DENSITY * GRAVITY * depth_m


def asme_b36_10_standard_walls() -> list[dict[str, Any]]:
    """ASME B36.10 12-inch NPS wall series used for standard-wall selection."""
    series = [
        ("SCH 20", 0.250),
        ("SCH 30", 0.330),
        ("STD", 0.375),
        ("SCH 40", 0.406),
        ("XS", 0.500),
        ("SCH 60", 0.562),
        ("SCH 80", 0.688),
        ("SCH 100", 0.844),
        ("SCH 120 / XXS", 1.000),
        ("SCH 140", 1.125),
        ("SCH 160", 1.312),
    ]
    return [
        {"label": label, "wall_in": wall_in, "wall_mm": round(wall_in * INCH_TO_MM, 3)}
        for label, wall_in in series
    ]


def wall_grid_mm() -> list[float]:
    grid = {round(10.0 + 0.5 * idx, 3) for idx in range(51)}
    grid.update(wall["wall_mm"] for wall in asme_b36_10_standard_walls())
    return sorted(grid)


def standard_labels_for_wall(wall_mm: float) -> list[str]:
    return [
        wall["label"]
        for wall in asme_b36_10_standard_walls()
        if math.isclose(wall["wall_mm"], wall_mm, abs_tol=0.001)
    ]


def run_engine_point(wall_mm: float) -> dict[str, Any]:
    """Run both design codes at one wall thickness using the live engine."""
    from digitalmodel.structural.analysis.wall_thickness import (
        DesignCode,
        DesignFactors,
        DesignLoads,
        PipeGeometry,
        PipeMaterial,
        WallThicknessAnalyzer,
    )

    geometry = PipeGeometry(
        outer_diameter=CASE["geometry"]["outer_diameter"],
        wall_thickness=wall_mm / 1000.0,
        corrosion_allowance=CASE["geometry"]["corrosion_allowance"],
    )
    material = PipeMaterial(**CASE["material"])
    # Two engine runs per code — one per load premise (see LOAD_PREMISES).
    burst_loads = DesignLoads(
        internal_pressure=CASE["loads"]["internal_pressure"],
        external_pressure=0.0,
    )
    empty_line_loads = DesignLoads(
        internal_pressure=0.0,
        external_pressure=external_pressure_from_depth(CASE["water_depth_m"]),
    )
    code_map = {code.value: code for code in DesignCode}
    code_results = []
    for code_label in CASE["codes"]:
        burst_run = WallThicknessAnalyzer(
            geometry, material, burst_loads, DesignFactors(), code_map[code_label]
        ).perform_analysis()
        empty_run = WallThicknessAnalyzer(
            geometry, material, empty_line_loads, DesignFactors(), code_map[code_label]
        ).perform_analysis()
        checks = {
            name: round(
                (burst_run if name in BURST_CHECKS[code_label] else empty_run)
                .checks[name],
                6,
            )
            for name in empty_run.checks
        }
        code_results.append({"code_label": code_label, "checks": checks})

    labels = standard_labels_for_wall(wall_mm)
    return {
        "wall_thickness_mm": round(wall_mm, 3),
        "wall_thickness_in": round(wall_mm / INCH_TO_MM, 4),
        "is_standard": bool(labels),
        "standard_labels": labels,
        "codes": code_results,
    }


def run_sweep() -> list[dict[str, Any]]:
    return [run_engine_point(wall_mm) for wall_mm in wall_grid_mm()]


def is_propagation_check(code_label: str, check_name: str) -> bool:
    return check_name in PROPAGATION_CHECKS.get(code_label, set())


def iter_check_values(point: dict[str, Any], include_propagation: bool):
    for code_result in point["codes"]:
        code_label = code_result["code_label"]
        for check_name, utilisation in code_result["checks"].items():
            if not include_propagation and is_propagation_check(code_label, check_name):
                continue
            yield code_label, check_name, utilisation


def point_passes(point: dict[str, Any], include_propagation: bool) -> bool:
    return all(utilisation <= 1.0 for _, _, utilisation in iter_check_values(point, include_propagation))


def governing_check(point: dict[str, Any], include_propagation: bool) -> dict[str, Any]:
    code_label, check_name, utilisation = max(
        iter_check_values(point, include_propagation),
        key=lambda item: item[2],
    )
    return {
        "code_label": code_label,
        "check_name": check_name,
        "label": f"{code_label} {check_name}",
        "utilisation": round(utilisation, 6),
    }


def find_sweep_point(payload: dict[str, Any], wall_mm: float) -> dict[str, Any]:
    for point in payload["sweep"]:
        if math.isclose(point["wall_thickness_mm"], wall_mm, abs_tol=0.001):
            return point
    raise KeyError(f"No sweep point for wall {wall_mm:.3f} mm")


def engine_wall_passes(wall_mm: float, include_propagation: bool) -> bool:
    return point_passes(run_engine_point(wall_mm), include_propagation)


def find_nonstandard_minimum(include_propagation: bool) -> float:
    """Bisection on the live engine over the requested 10-35 mm sweep range."""
    low, high = 10.0, 35.0
    if engine_wall_passes(low, include_propagation):
        return low
    if not engine_wall_passes(high, include_propagation):
        raise RuntimeError("No passing wall thickness found up to 35 mm")
    for _ in range(50):
        mid = (low + high) / 2.0
        if engine_wall_passes(mid, include_propagation):
            high = mid
        else:
            low = mid
    return round(high, 3)


def select_standard_wall(
    payload: dict[str, Any],
    *,
    include_propagation: bool,
    nonstandard_minimum_mm: float,
) -> dict[str, Any]:
    for wall in payload["standard_walls"]:
        point = find_sweep_point(payload, wall["wall_mm"])
        if point_passes(point, include_propagation):
            governing = governing_check(point, include_propagation)
            return {
                "include_propagation": include_propagation,
                "nonstandard_minimum_wall_mm": nonstandard_minimum_mm,
                "selected_standard_wall_mm": point["wall_thickness_mm"],
                "selected_standard_wall_in": point["wall_thickness_in"],
                "selected_standard_label": " / ".join(point["standard_labels"]),
                "governing_check": governing["label"],
                "governing_utilisation": governing["utilisation"],
            }
    raise RuntimeError("No passing standard wall found")


def design_propagation_pressure_dnv(wall_mm: float) -> float:
    d = CASE["geometry"]["outer_diameter"]
    ca = CASE["geometry"]["corrosion_allowance"]
    t2 = wall_mm / 1000.0 - ca
    smys = CASE["material"]["smys"]
    p_pr = 35.0 * smys * (t2 / d) ** 2.5
    return p_pr / (1.138 * 1.15)


def design_propagation_pressure_api(wall_mm: float) -> float:
    d = CASE["geometry"]["outer_diameter"]
    t = wall_mm / 1000.0
    smys = CASE["material"]["smys"]
    return 0.80 * 24.0 * smys * (t / d) ** 2.4


def buckle_arrestor_sizing(payload: dict[str, Any]) -> dict[str, Any]:
    selected = payload["selection"]["with_arrestors"]["selected_standard_wall_mm"]
    d = CASE["geometry"]["outer_diameter"]
    pe = external_pressure_from_depth(CASE["water_depth_m"])
    smys = CASE["material"]["smys"]
    ca = CASE["geometry"]["corrosion_allowance"]

    # API RP 1111 Sec 4.3.3 and DNV-ST-F101 Sec 5 D600 use closed-form
    # propagation-pressure relations, p_pr = C*SMYS*(t/D)^n. For this screening
    # integral arrestor, treat crossover as achieved when the thickened section's
    # design propagation pressure is at least the external design pressure.
    dnv_wall_m = d * ((pe * 1.138 * 1.15) / (35.0 * smys)) ** (1.0 / 2.5) + ca
    api_wall_m = d * (pe / (0.80 * 24.0 * smys)) ** (1.0 / 2.4)
    arrestor_wall_mm = math.ceil(max(dnv_wall_m, api_wall_m) * 1000.0 * 10.0) / 10.0
    crossover_mpa = min(
        design_propagation_pressure_dnv(arrestor_wall_mm),
        design_propagation_pressure_api(arrestor_wall_mm),
    ) / 1e6
    return {
        "type": "integral buckle arrestor",
        "pipe_wall_mm": selected,
        "arrestor_wall_mm": round(arrestor_wall_mm, 3),
        "additional_wall_mm": round(arrestor_wall_mm - selected, 3),
        "arrestor_length_m": round(2.0 * d, 3),
        "length_basis": "2.0D screening length for integral arrestor section",
        "governing_code": "DNV-ST-F101"
        if dnv_wall_m >= api_wall_m
        else "API-RP-1111",
        "crossover_pressure_mpa": round(crossover_mpa, 3),
    }


def run_compute() -> dict[str, Any]:
    payload: dict[str, Any] = {
        "schema_version": 3,
        "case": CASE,
        "load_premises": LOAD_PREMISES,
        "external_pressure_pa": round(external_pressure_from_depth(CASE["water_depth_m"]), 3),
        "external_pressure_mpa": round(external_pressure_from_depth(CASE["water_depth_m"]) / 1e6, 6),
        "standard_walls": asme_b36_10_standard_walls(),
        "wall_grid_mm": wall_grid_mm(),
        "sweep": run_sweep(),
    }
    payload["selection"] = {
        "with_arrestors": select_standard_wall(
            payload,
            include_propagation=False,
            nonstandard_minimum_mm=find_nonstandard_minimum(False),
        ),
        "without_arrestors": select_standard_wall(
            payload,
            include_propagation=True,
            nonstandard_minimum_mm=find_nonstandard_minimum(True),
        ),
    }
    payload["buckle_arrestor_sizing"] = buckle_arrestor_sizing(payload)
    return payload


def write_cache(payload: dict[str, Any]) -> None:
    CACHE_PATH.parent.mkdir(parents=True, exist_ok=True)
    CACHE_PATH.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def run_from_cache() -> dict[str, Any]:
    if not CACHE_PATH.exists():
        raise SystemExit(f"[ERROR] cache fixture not found: {CACHE_PATH}")
    return json.loads(CACHE_PATH.read_text(encoding="utf-8"))


def print_summary(payload: dict[str, Any]) -> None:
    case = payload["case"]
    with_arrestors = payload["selection"]["with_arrestors"]
    without_arrestors = payload["selection"]["without_arrestors"]
    print("=" * 72)
    print("Pipeline Wall Thickness — Utilization Sweep Quick Check")
    print("=" * 72)
    print(f"Case              : {case['label']}")
    print(f"Pressure basis    : {case['pressure_basis']} pressure, 150 bar")
    print(f"Buckle arrestors  : {case['buckle_arrestors']} (propagation excluded from headline)")
    print(f"External pressure : {payload['external_pressure_mpa']:.3f} MPa at {case['water_depth_m']:.0f} m")
    print(f"Codes checked     : {', '.join(case['codes'])}")
    print("-" * 72)
    print(
        "Headline wall     : "
        f"{with_arrestors['selected_standard_wall_in']:.3f} in / "
        f"{with_arrestors['selected_standard_wall_mm']:.3f} mm "
        f"({with_arrestors['selected_standard_label']})"
    )
    print(
        "Governing check   : "
        f"{with_arrestors['governing_check']}  "
        f"U={with_arrestors['governing_utilisation']:.3f}"
    )
    print(
        "Non-standard min  : "
        f"{with_arrestors['nonstandard_minimum_wall_mm']:.3f} mm "
        "(secondary, propagation excluded)"
    )
    print(
        "Without arrestors : "
        f"{without_arrestors['selected_standard_wall_in']:.3f} in / "
        f"{without_arrestors['selected_standard_wall_mm']:.3f} mm "
        f"({without_arrestors['selected_standard_label']}), "
        f"non-standard {without_arrestors['nonstandard_minimum_wall_mm']:.3f} mm, "
        f"{without_arrestors['governing_check']} U={without_arrestors['governing_utilisation']:.3f}"
    )
    print("=" * 72)


def write_report(payload: dict[str, Any], output_path: Path) -> Path:
    report_path = HERE / "quick_report.py"
    spec = importlib.util.spec_from_file_location("wt_quick_report", report_path)
    if spec is None or spec.loader is None:
        raise ImportError(f"Cannot load report renderer: {report_path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    render_report = module.write_report

    return render_report(payload, output_path, PROPAGATION_CHECKS)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--from-cache", action="store_true", help="Load committed JSON fixture only.")
    mode.add_argument("--compute", action="store_true", help="Run live engine and refresh cache.")
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    args = parser.parse_args(argv)

    if args.from_cache:
        payload = run_from_cache()
    else:
        payload = run_compute()
        write_cache(payload)

    print_summary(payload)
    artifact = write_report(payload, args.output)
    print(f"ARTIFACT: {artifact.resolve()}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
