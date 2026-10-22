# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the public asset integrity report from live calculation returns.

Run from the repository root with PYTHONPATH=src. No network access, external
solver, result snapshot or machine configuration is used during rebuilding.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import inspect
import json
import math
import re
from dataclasses import asdict
from pathlib import Path

from jinja2 import Environment, StrictUndefined
from markupsafe import Markup, escape

from digitalmodel.asset_integrity import corroded_pipe

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
STYLE_SOURCE = "docs/reports/pipeline-ffs-assessment.html"
DEPENDENCIES = (
    "src/digitalmodel/asset_integrity/corroded_pipe.py",
    "src/digitalmodel/asset_integrity/applicability.py",
    "tests/asset_integrity/test_ffs_published_examples.py",
    "docs/domains/asset-integrity/uniform-loss-source-audit-2026-10-06.md",
)


def assess(inputs, depth):
    """Call the existing engine with the public example's explicit factor."""
    return corroded_pipe.b31g_original(
        inputs["D"],
        inputs["t"],
        depth,
        inputs["L"],
        inputs["smys_psi"],
        maop_psi=inputs["maop_psi"],
        safety_factor=1 / inputs["design_factor"],
    )


def reference_cases(fixture):
    """Retain native returns and apply the disclosed public-comparison cap."""
    inputs = fixture["inputs"]
    intact = assess(inputs, 0)
    flow_multiplier = intact.flow_stress_psi / inputs["smys_psi"]
    cap = intact.safe_pressure_psi / flow_multiplier
    cases = []
    for source in fixture["cases"]:
        result = assess(inputs, source["depth_in"])
        reference = min(result.safe_pressure_psi, cap)
        cases.append(
            {
                **source,
                "result": asdict(result),
                "reference_cap_psi": cap,
                "reference_pressure_psi": reference,
                "difference_bound_percent": math.ceil(
                    1000
                    * abs(reference - source["published_pressure_psi"])
                    / source["published_pressure_psi"]
                )
                / 10,
            }
        )
    return cases


def depth_sensitivity(inputs, cases):
    """Retain pressure and applicability from each native sensitivity call."""
    cap = cases[0]["reference_cap_psi"]
    depth_limit = inputs["t"] * corroded_pipe._B31G_MAX_DT
    depths = sorted(
        {
            *(depth_limit * i / 80 for i in range(81)),
            *(case["depth_in"] for case in cases),
        }
    )
    sensitivity = []
    for depth in depths:
        result = assess(inputs, depth)
        sensitivity.append(
            {
                "depth_in": depth,
                "pressure_psi": result.safe_pressure_psi,
                "reference_pressure_psi": min(result.safe_pressure_psi, cap),
                "within_applicability": result.applicability.ok,
            }
        )
    return sensitivity


def formula_coefficients(inputs, cases):
    """Read display coefficients from native results and the branch comparison."""
    shallow = cases[0]["result"]
    tree = ast.parse(inspect.getsource(corroded_pipe.folias_original))
    branch_limit = next(n for n in ast.walk(tree) if isinstance(n, ast.Compare))
    return {
        "flow_multiplier": shallow["flow_stress_psi"] / inputs["smys_psi"],
        "area_multiplier": shallow["area_ratio"] / cases[0]["depth_in"] * inputs["t"],
        "bulging_coefficient": (shallow["folias_factor"] ** 2 - 1)
        / shallow["details"]["z"],
        "branch_limit": ast.literal_eval(branch_limit.comparators[0]),
    }


def calculate():
    """Collect one public-example result set, including all native flags."""
    fixture = json.loads((HERE / "public-example.json").read_text(encoding="utf-8"))
    inputs = fixture["inputs"]
    cases = reference_cases(fixture)
    return {
        "inputs": inputs,
        "cases": cases,
        "sensitivity": depth_sensitivity(inputs, cases),
        "limits": {
            "depth_fraction": corroded_pipe._B31G_MAX_DT,
            "depth_in": inputs["t"] * corroded_pipe._B31G_MAX_DT,
        },
        "formula": formula_coefficients(inputs, cases),
        "sources": fixture["sources"],
    }


def display_values(data):
    """Define engineering precision once for prose, tables and schematic labels."""
    values = {}

    def add(key, value, precision):
        values[key] = {"value": value, "format": f",.{precision}f"}

    for key, value in data["inputs"].items():
        precision = 3 if key == "t" else 2 if key == "design_factor" else 1
        add("input." + key, value, precision)
    for index, case in enumerate(data["cases"]):
        prefix = f"case{index}."
        for key in (
            "failure_pressure_psi",
            "intact_pressure_psi",
            "flow_stress_psi",
            "safe_pressure_psi",
        ):
            add(prefix + key, case["result"][key], 1)
        for key in ("folias_factor", "area_ratio", "rsf"):
            add(prefix + key, case["result"][key], 3)
        for key in (
            "reference_cap_psi",
            "reference_pressure_psi",
            "published_pressure_psi",
            "tolerance_psi",
            "difference_bound_percent",
        ):
            add(prefix + key, case[key], 1)
        add(prefix + "depth_in", case["depth_in"], 3)
        add(
            prefix + "margin_psi",
            case["reference_pressure_psi"] - data["inputs"]["maop_psi"],
            1,
        )
    add("limit.depth_fraction", data["limits"]["depth_fraction"], 2)
    add("limit.depth_in", data["limits"]["depth_in"], 3)
    for key, value in data["formula"].items():
        add("formula." + key, value, 3 if key == "area_multiplier" else 1)
    return values


def plot_geometry(data):
    """Map calculated pressures to SVG coordinates; no engineering model here."""
    ymax = math.ceil(max(r["pressure_psi"] for r in data["sensitivity"]) / 100) * 100

    def x(depth):
        return 80 + 620 * depth / data["inputs"]["t"]

    def y(pressure):
        return 320 - 270 * pressure / ymax

    points = []
    for index, row in enumerate(data["sensitivity"]):
        points.append(
            {
                **row,
                "index": index,
                "x": x(row["depth_in"]),
                "y": y(row["pressure_psi"]),
                "reference_y": y(row["reference_pressure_psi"]),
            }
        )
    return {
        "points": points,
        "raw_line": " ".join(f"{p['x']:.2f},{p['y']:.2f}" for p in points),
        "reference_line": " ".join(
            f"{p['x']:.2f},{p['reference_y']:.2f}" for p in points
        ),
        "depth_limit_x": x(data["limits"]["depth_in"]),
        "demand_y": y(data["inputs"]["maop_psi"]),
        "x_ticks": [
            {"x": x(data["inputs"]["t"] * i / 4), "value": data["inputs"]["t"] * i / 4}
            for i in range(5)
        ],
        "y_ticks": [{"y": y(ymax * i / 4), "value": ymax * i / 4} for i in range(5)],
    }


def build(output=HERE / "report.html"):
    """Render a deterministic, self-contained report and read it back."""
    data = calculate()
    data["display_values"] = display_values(data)
    data["source_digests"] = {
        path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest()
        for path in (*DEPENDENCIES, STYLE_SOURCE)
    }
    data["source_digests"]["public-example.json"] = hashlib.sha256(
        (HERE / "public-example.json").read_bytes()
    ).hexdigest()
    style_page = (ROOT / STYLE_SOURCE).read_text(encoding="utf-8")
    style = re.search(r"<style>(.*?)</style>", style_page, re.S).group(1)
    changes = json.loads((HERE / "changes.json").read_text(encoding="utf-8"))

    def number(key, tag="span"):
        spec = data["display_values"][key]
        text = format(spec["value"], spec["format"])
        if tag not in {"span", "tspan"}:
            raise ValueError("Unsupported numeric element")
        return Markup('<{0} data-value="{1}">{2}</{0}>').format(
            escape(tag), escape(key), escape(text)
        )

    environment = Environment(
        autoescape=True, undefined=StrictUndefined, keep_trailing_newline=True
    )
    template = environment.from_string(
        (HERE / "report.template.html").read_text(encoding="utf-8")
    )
    rendered = template.render(
        data=data,
        plot=plot_geometry(data),
        style=Markup(style),
        n=number,
        changes=changes,
        revision=changes["revisions"][0],
    )
    output = Path(output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(rendered, encoding="utf-8", newline="\n")
    if output.read_text(encoding="utf-8") != rendered:
        raise OSError("Report readback differs from the rendered document")
    return output


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=HERE / "report.html")
    arguments = parser.parse_args()
    build(arguments.output)
