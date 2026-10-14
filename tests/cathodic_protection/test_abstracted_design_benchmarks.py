# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""End-to-end arithmetic benchmarks for three abstracted design records.

Inputs are assumed, based on previous experience. The public example records
are read without executing their Python blocks. Licensed table values are read
from the existing table API and never duplicated here. Agreement establishes
integration arithmetic, not independent qualification of those tables or a
complete physical design. The comparison analysis records the legacy gaps.
"""

from __future__ import annotations

import ast
import copy
import math
import re
from pathlib import Path
from typing import Any

import pytest

from digitalmodel.cathodic_protection import _kernels as constants
from digitalmodel.cathodic_protection import b401_tables as tables
from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection

EXAMPLES = Path(__file__).resolve().parents[2] / "docs/domains/cathodic_protection/examples"
ANALYSIS = EXAMPLES.parent / "abstracted-benchmark-analysis.html"
PHASES = ("initial", "mean", "final")


def load_record(number: str) -> dict[str, Any]:
    """Read the unique literal configuration, allowing numeric addition only."""
    paths = list(EXAMPLES.glob(f"calc-{number}-*.md"))
    assert len(paths) == 1
    text = paths[0].read_text(encoding="utf-8")
    configs = []
    for block in re.findall(r"```python\s*\n(.*?)```", text, re.S):
        tree = ast.parse(block)
        for node in ast.walk(tree):
            if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Add):
                left, right = ast.literal_eval(node.left), ast.literal_eval(node.right)
                assert isinstance(left, (int, float)) and isinstance(right, (int, float))
                node.__class__ = ast.Constant
                node.value = left + right
        for node in tree.body:
            if isinstance(node, ast.Assign) and any(
                isinstance(target, ast.Name) and target.id == "cfg" for target in node.targets
            ):
                configs.append(ast.literal_eval(node.value))
    assert len(configs) == 1
    return configs[0]


def expected_demand(inputs: dict[str, Any]) -> dict[str, float]:
    """Sum area times phase density times capped linear coating breakdown."""
    life = inputs["design_data"]["design_life"]
    edition = inputs["design_data"]["edition"]
    climate = tables.climate_from_temperature(inputs["environment"]["seawater_temperature_C"])
    totals = dict.fromkeys(PHASES, 0.0)
    for zone in inputs["structure"]["zones"]:
        depth = zone.get("depth_m", 0.0)
        category = zone["coating_category"]
        if category == "bare":
            intercept, slope = 1.0, 0.0
        else:
            a, b = tables.coating_breakdown_constants(
                tables.PaintCategory(category), tables.depth_band(depth), edition
            )
            intercept, slope = a.value, b.value
        for phase, time in zip(PHASES, (0.0, life / 2, life)):
            if zone.get("base_zone", zone["zone"]) == "buried":
                density = tables.buried_current_density(edition).value
            else:
                density = tables.design_current_density(
                    climate, tables.depth_band(depth), tables.DesignPhase(phase), edition
                ).value
            totals[phase] += zone["area_m2"] * density * min(1.0, intercept + slope * time)
    return totals


def expected_design(inputs: dict[str, Any]) -> dict[str, Any]:
    """Calculate independent mass and integer current-output requirements."""
    demand = expected_demand(inputs)
    anode = inputs["anode"]
    edition = inputs["design_data"]["edition"]
    material = tables.AnodeMaterial.ALUMINIUM
    capacity = tables.anode_capacity(material, tables.AnodeEnvironment.SEAWATER, edition)
    voltage = tables.protection_potential(edition).value - tables.anode_closed_circuit_potential(
        material, tables.AnodeEnvironment.SEAWATER, edition
    ).value
    utilization = anode["utilization_factor"]
    mass = demand["mean"] * inputs["design_data"]["design_life"] * constants.HOURS_PER_YEAR
    mass /= utilization * capacity.value
    rho = inputs["environment"]["seawater_resistivity_ohm_m"]
    length, radius = anode["length_m"], anode["radius_m"]
    final_radius = radius * math.sqrt(1 - utilization)
    if anode["type"] == "stand_off":
        resistances = [rho / (2 * math.pi * length) * (math.log(4 * length / r) - 1)
                       for r in (radius, final_radius)]
    else:
        resistances = [rho / (length + 2 * r) for r in (radius, final_radius)]
    counts = (math.ceil(mass / anode["individual_anode_mass_kg"]),
              math.ceil(demand["initial"] * resistances[0] / voltage),
              math.ceil(demand["final"] * resistances[1] / voltage))
    return {"demand": demand, "mass": mass, "resistances": resistances,
            "counts": counts, "recommended": max(counts),
            "governing": ("mass", "initial", "final")[counts.index(max(counts))]}


def assert_design(cfg: dict[str, Any]) -> dict[str, Any]:
    """Check all phases and verify the minimum installed-count boundary."""
    expected = expected_design(cfg["inputs"])
    result = run_cathodic_protection(copy.deepcopy(cfg))["results"]
    for phase in PHASES:
        assert result["current_demand_A"][f"total_{phase}_A"] == pytest.approx(
            expected["demand"][phase], rel=1e-12
        )
    assert result["anode_requirements"]["total_mass_kg"] == pytest.approx(
        expected["mass"], rel=0, abs=0.005
    )
    checks = result["current_output_verification"]
    for stage, resistance in zip(("initial", "final"), expected["resistances"]):
        assert checks[f"anode_resistance_{stage}_ohm"] == pytest.approx(
            resistance, rel=0, abs=0.0000005
        )
    assert tuple(checks[f"count_by_{case}"] for case in
                 ("mass", "initial_current", "final_current")) == expected["counts"]
    assert checks["recommended_anode_count"] == expected["recommended"]
    assert checks["governing_case"] == expected["governing"]
    assert result["citations"] and result["edition"] == cfg["inputs"]["design_data"]["edition"]
    installed = copy.deepcopy(cfg)
    installed["inputs"]["anode"]["count"] = expected["recommended"]
    assert run_cathodic_protection(installed)["results"]["status"]["result"] == "PASS"
    installed["inputs"]["anode"]["count"] -= 1
    assert run_cathodic_protection(installed)["results"]["status"]["result"] == "FAIL"
    return result


@pytest.mark.parametrize("record", ["004", "009", "010"],
                         ids=["subsea-base", "temporary-service", "hull-sensitivity"])
def test_abstracted_design_end_to_end(record: str) -> None:
    """Benchmark three complete configurations; re-run the hull envelope."""
    cfg = load_record(record)
    baseline = assert_design(cfg)
    if record == "010":
        environment = cfg["inputs"]["environment"]
        resistivities = sorted((environment["seawater_resistivity_normal"],
                               environment["seawater_resistivity_ohm_m"],
                               environment["seawater_resistivity_worst"]))
        results = []
        for rho in resistivities:
            case = copy.deepcopy(cfg)
            case["inputs"]["environment"]["seawater_resistivity_ohm_m"] = rho
            results.append(assert_design(case))
        for result in results:
            assert result["current_demand_A"] == baseline["current_demand_A"]
            assert result["anode_requirements"] == baseline["anode_requirements"]
        counts = [r["current_output_verification"]["recommended_anode_count"] for r in results]
        assert counts == sorted(counts) and counts[-1] > counts[0]
        assert results[-1]["status"]["result"] == "FAIL"


def ship_comparison_values() -> tuple[float, ...]:
    """Produce Table 3 current-route quantities from the record-001 input."""
    cfg = load_record("001")
    cfg["citation_repo_root"] = EXAMPLES.parents[3] / "tests/citations/fixtures"
    result = run_cathodic_protection(cfg)["results"]
    demand = result["current_demand_A"]["totals"]
    requirements = result["anode_requirements"]
    return (*(demand[phase] for phase in PHASES),
            requirements["total_mass_kg"], requirements["final_output_count"])


def legacy_comparison_values() -> tuple[float, ...]:
    """Read the repository comparator; retain the archived fractional count."""
    path = next(EXAMPLES.glob("calc-001-*.md"))
    basis = path.read_text(encoding="utf-8").split(
        "## Calculation Results (from source document)", 1
    )[1]
    values = []
    for label in ("Initial Ici", "Mean Icm", "Final Icf", "Total net anode mass"):
        match = re.search(r"\| " + label + r" \| ([\d .]+) \|", basis)
        assert match is not None
        values.append(float(match[1].replace(" ", "")))
    # Cached derived count held in the analysis at its preceding revision.
    # The workbook digest supplies secondary provenance; no formula is rerun.
    return (*values, 186.2)


def test_record_001_current_route_matches_table_3() -> None:
    """Reproduce every rounded current-route cell from the ABS ships record."""
    assert "ship_comparison_values" in globals(), "Table 3 has no committed producer"
    values = ship_comparison_values()
    assert [f"{value:.1f}" for value in values] == [
        "145.5", "218.3", "291.0", "4634.9", "300.0"
    ]
    html = ANALYSIS.read_text(encoding="utf-8")
    comparison = html.split("<h2>Legacy workbook comparison</h2>", 1)[1].split("</table>", 1)[0]
    rows = re.findall(r"<tr>(.*?)</tr>", comparison, re.S)[1:]
    assert [re.findall(r"<td>(.*?)</td>", row)[3] for row in rows] == [
        f"{value:.1f}" for value in values
    ]
    for row, previous, current in zip(rows, legacy_comparison_values(), values):
        cells = re.findall(r"<td>(.*?)</td>", row)
        difference = current - previous
        assert cells[2] == f"{previous:.1f}"
        assert cells[4:] == [f"{difference:+.1f}", f"{100 * difference / previous:+.1f}%"]


def test_analysis_summary_revision_and_comparison_basis() -> None:
    """Require the summary, appended revision and both comparison sources."""
    html = ANALYSIS.read_text(encoding="utf-8")
    assert re.findall(r"<h2>(.*?)</h2>", html)[:2] == ["Executive Summary", "Revision history"]
    assert "<td>3</td>" in html.split("<h2>Basis and limitations</h2>", 1)[0]
    assert "ABS Guidance Notes on Cathodic Protection of Ships, December 2017 edition" in html
    assert "calc-001-abs-gn-ships-2017-fst-hull.md:122-132" in html
    assert "secondary provenance for the fractional" in html
