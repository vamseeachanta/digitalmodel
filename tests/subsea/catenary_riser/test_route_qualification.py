# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Vertex hand cases: Catenary Curve (2019), equations (1), (4), (9), (10), (14).

Synthetic inputs a=3 m, d=2 m, w=10 N/m give s=4 m, H=30 N,
V=40 N, T=50 N, x=3 ln(3) m, angle=atan(3/4) from vertical.
These are a 3:4:5 triangle and cosh(ln(3))=5/3, independent of the package.
"""

import ast
import json
import math
from dataclasses import replace
from pathlib import Path

import pytest

from digitalmodel.subsea.catenary_riser import (
    FluidProperties,
    MaterialProperties,
    RiserConfiguration,
    SimpleCatenaryAnalyzer,
)
from digitalmodel.subsea.catenary_riser.legacy.catenary_equation import CatenaryCalculator
from digitalmodel.subsea.catenary_riser.qualification import route_conventions


@pytest.fixture
def unit_riser():
    # Synthetic mass balance: annular area=3*pi/4, rho=40/(3*pi), g=1 => w=10.
    return RiserConfiguration(
        "synthetic", 2, .5, 20,
        MaterialProperties("synthetic", 1, 0, 40 / (3 * math.pi)),
        FluidProperties("empty", 0), FluidProperties("vacuum", 0),
        water_depth=2, horizontal_offset=3 * math.log(3), gravity=1,
    )


@pytest.mark.parametrize("control", [{"horizontal_tension": 30}, {"top_tension": 50}, {}])
def test_modern_vertex_and_distributions(unit_riser, control):
    analyzer = SimpleCatenaryAnalyzer()
    result = analyzer.analyze_riser(unit_riser, **control)
    assert result.arc_length == pytest.approx(4)
    assert result.horizontal_offset == pytest.approx(3 * math.log(3))
    assert result.top_tension == pytest.approx(50)
    assert result.touchdown_tension == pytest.approx(30)
    assert result.top_angle == pytest.approx(math.degrees(math.atan(.75)))
    assert result.max_curvature == pytest.approx(1 / 3)
    s, tension = analyzer.calculate_tension_along_riser(result, n_points=3)
    assert list(s) == pytest.approx([0, 2, 4])
    assert list(tension) == pytest.approx([30, math.sqrt(1300), 50])
    _, angles = analyzer.calculate_angle_along_riser(result, n_points=3)
    assert angles[0] == 0
    assert angles[-1] + result.top_angle == pytest.approx(90)


@pytest.mark.parametrize("route", ["calculator", "methods"])
@pytest.mark.parametrize("control", ["force", "angle"])
def test_legacy_vertex_overlap(route, control):
    data = {"F": 50 if control == "force" else None, "w": 10, "d": 2,
            "q": math.degrees(math.atan(.75)) if control == "angle" else None, "X": None}
    if route == "calculator":
        out = CatenaryCalculator().calculate(data)
    else:
        from digitalmodel.subsea.catenary_riser.legacy.catenaryMethods import catenaryEquation
        out = catenaryEquation(data)
    assert out["S"] == pytest.approx(4)
    assert out["X"] == pytest.approx(3 * math.log(3))
    if control == "force":
        assert out["THorizontal"] == pytest.approx(30)
        assert out["b"] == pytest.approx(1 / 3)
    else:
        assert out["BendRadius"] == pytest.approx(3)


def test_route_inventory_has_output_evidence():
    routes = route_conventions()
    assert len(routes) == 8
    assert all(r["units"] == "m, N, N/m, degrees" for r in routes)
    assert all(r["evidence"] and r["origin"] and r["force_sign"] for r in routes)
    assert {r["status"] for r in routes} == {"TESTED", "QUARANTINED", "NOT_EVALUATED"}


def test_arbitrary_end_boundary_condition_is_refused(unit_riser):
    with pytest.raises(NotImplementedError, match="horizontal_tangent_vertex"):
        SimpleCatenaryAnalyzer().analyze_riser(
            unit_riser, horizontal_tension=30, boundary_condition="two_supported_ends",
        )


def test_nearly_horizontal_span_does_not_lose_the_height(unit_riser):
    """Synthetic conditioning case a=1e16 m, d=2 m, w=10 N/m.

    From cosh(u)=1+u²/2+O(u⁴), x=sqrt(2ad)(1+O(d/a))=2e8 m.
    The series correction is below floating precision for this input. Forming
    acosh(1+d/a) directly instead gives a span error of several percent.
    This is a numerical stress case, not an installation configuration.
    """
    riser = replace(unit_riser, length=1e9, horizontal_offset=None)
    output = SimpleCatenaryAnalyzer().analyze_riser(riser, horizontal_tension=1e17)
    assert output.horizontal_offset == pytest.approx(2e8, rel=1e-12)
    assert output.arc_length == pytest.approx(2e8, rel=1e-12)
    legacy = CatenaryCalculator().calculate({"F": 1e17 + 20, "w": 10, "d": 2})
    assert legacy["X"] == pytest.approx(2e8, rel=1e-12)


def test_every_section_six_claim_has_a_disposition():
    root = Path(__file__).resolve().parents[3]
    docs = root / "src/digitalmodel/subsea/catenary_riser/docs"
    section = (docs / "agent-pack.md").read_text().split("## 6.")[1].split("## 7.")[0]
    claims = [line.split("|")[1].strip() for line in section.splitlines()
              if line.startswith("| `") or line.startswith("| Legacy ")
              or line.startswith("| Conflicting ") or line.startswith("| Distinct ")
              or line.startswith("| README ")]
    records = json.loads((docs / "qualification.json").read_text())["claims"]
    assert claims
    assert [r["claim"] for r in records] == claims
    assert all(r["status"] in {"TESTED", "NOT_EVALUATED"} for r in records)
    assert all(r["reason"] for r in records)
    assert all(r.get("tests") for r in records if r["status"] == "TESTED")
    assert len(records) == 86  # Historical observations, not a numerical acceptance limit.
    for record in records:
        for node in record.get("tests", []):
            file, *names = node.split("::")
            source = ast.parse((root / "tests/subsea/catenary_riser" / file).read_text())
            for name in names:
                source = next(n for n in source.body
                              if isinstance(n, (ast.FunctionDef, ast.ClassDef)) and n.name == name)
