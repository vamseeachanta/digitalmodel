# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent tubular hand calculations; no spreadsheet equivalence is claimed.

Sources: MIT, Stress-Strain Curves (2001), equation 1 (N/A); MIT,
Stresses in Beams (2000), equations 4 and 7 (I = integral(y² dA), My/I).
Integrating a circular annulus gives A = pi*(D²-d²)/4 and
I = pi*(D⁴-d⁴)/64. All test dimensions, forces, yield strengths and
allowables are illustrative inputs, with no API or material-grade status.
"""

import math
import json
import runpy
from dataclasses import asdict, replace
from pathlib import Path

import pytest

from digitalmodel.citations.schema import Citation, CitationResolutionError, CitedValue
from digitalmodel.subsea.structural_checks.api_rp2a import api_rp2a_check


@pytest.fixture
def cited_limit(tmp_path):
    """A resolved test source validates plumbing, not a standard coefficient."""
    page = tmp_path / "wikis" / "engineering" / "standards" / "example.md"
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: EXAMPLE-ONLY\npublisher: Illustrative calculation\n"
        "revision: 1\n---\nAssumed allowable for software testing only.\n",
        encoding="utf-8",
    )
    citation = Citation(
        code_id="EXAMPLE-ONLY",
        publisher="Illustrative calculation",
        revision="1",
        section="Illustrative supplied allowable; not a code value",
        wiki_path="wikis/engineering/standards/example.md",
        source_sibling="generic",
    )
    return CitedValue(20.0, citation, "MPa"), tmp_path


def test_annular_geometry_and_tension():
    """D=10, t=1 => d=8, A=9*pi, I=369*pi/4, Z=369*pi/20.

    N=90*pi N => sigma=10 MPa; M=369*pi/2 N mm => sigma_b=10 MPa.
    The denominators come from integration, not the implementation.
    """
    result = api_rp2a_check(10, 1, 100, axial_force=90 * math.pi,
                           bending_moment=369 * math.pi / 2)
    assert result.area == pytest.approx(9 * math.pi)
    assert result.second_moment == pytest.approx(369 * math.pi / 4)
    assert result.section_modulus == pytest.approx(369 * math.pi / 20)
    checks = {check.name: check for check in result.checks}
    assert checks["axial_tension"].calculated_stress == pytest.approx(10)
    assert checks["axial_compression"].calculated_stress == 0
    assert checks["bending"].calculated_stress == pytest.approx(10)


def test_compression_and_negative_bending():
    """D=6, t=1 => d=4, A=5*pi, I=65*pi/4, Z=65*pi/12.

    N=-100*pi => compression 20 MPa; M=-325*pi/2 => bending 30 MPa.
    MIT equations 1 and 7 apply by the source in the module docstring.
    """
    result = api_rp2a_check(6, 1, 100, axial_force=-100 * math.pi,
                           bending_moment=-325 * math.pi / 2)
    checks = {check.name: check for check in result.checks}
    assert checks["axial_tension"].calculated_stress == 0
    assert checks["axial_compression"].calculated_stress == pytest.approx(20)
    assert checks["bending"].calculated_stress == pytest.approx(30)


def test_zero_load_does_not_establish_code_compliance():
    """Zero demand / unknown capacity is unknown, not a passing check."""
    result = api_rp2a_check(10, 1, 100)
    assert len(result.checks) == 8
    assert len({check.name for check in result.checks}) == 8
    assert result.status == "NOT_EVALUATED"
    assert result.passes is None
    assert result.max_unity_ratio is None
    assert not result.citations
    for check in result.checks:
        assert check.status == "NOT_EVALUATED"
        assert check.passes is None
        assert check.allowable_stress is None
        assert check.unity_ratio is None
        assert check.reason
        assert "unverified" in check.code_reference


@pytest.mark.parametrize("name,load", [
    ("axial_tension", {"axial_force": 90 * math.pi}),
    ("axial_compression", {"axial_force": -90 * math.pi}),
    ("bending", {"bending_moment": 369 * math.pi / 2}),
])
def test_supplied_limit_and_citation_sidecar(name, load, cited_limit):
    """Hand stress=10 MPa, supplied allowable=20 MPa => unity=0.5."""
    limit, root = cited_limit
    result = api_rp2a_check(10, 1, 100, **load, allowables={name: limit},
                           citation_root=root)
    check = next(check for check in result.checks if check.name == name)
    assert check.calculated_stress == pytest.approx(10)
    assert check.allowable_stress == 20
    assert check.unity_ratio == pytest.approx(0.5)
    assert check.passes is True
    assert check.status == "PASS"
    assert check.code_reference == limit.citation.section
    assert result.citations == (limit.citation,)
    assert result.max_unity_ratio == pytest.approx(0.5)
    assert result.status == "NOT_EVALUATED"
    assert result.passes is None
    assert "citations" in asdict(result)


@pytest.mark.parametrize("stress,expected", [(10, True), (20, True), (30, False)])
def test_acceptance_boundary(stress, expected, cited_limit):
    """N=stress*9*pi => demand=stress; unity=stress/20, limit unity=1."""
    limit, root = cited_limit
    result = api_rp2a_check(10, 1, 100, axial_force=stress * 9 * math.pi,
                           allowables={"axial_tension": limit}, citation_root=root)
    check = result.checks[0]
    assert check.unity_ratio == pytest.approx(stress / 20)
    assert check.passes is expected
    assert result.status == ("NOT_EVALUATED" if expected else "FAIL")
    assert result.passes is (None if expected else False)


def test_hydrostatic_and_shear_remain_unevaluated_with_cited_normal_limits(cited_limit):
    """P=2 MPa and V=100 N have no qualified pressure or shear equations."""
    limit, root = cited_limit
    result = api_rp2a_check(10, 1, 100, pressure=2, shear_force=-100,
                           allowables={name: limit for name in
                                       ("axial_tension", "axial_compression", "bending")},
                           citation_root=root)
    assert result.inputs.pressure == 2
    assert result.inputs.shear_force == -100
    assert result.status == "NOT_EVALUATED"
    for check in result.checks[3:]:
        assert check.calculated_stress is None
        assert check.status == "NOT_EVALUATED"
        assert check.passes is None


@pytest.mark.parametrize("field", [
    "diameter", "thickness", "yield_strength", "pressure", "axial_force",
    "bending_moment", "shear_force",
])
@pytest.mark.parametrize("bad", [float("nan"), float("inf"), -float("inf"), True, "1"])
def test_rejects_nonfinite_and_nonnumeric_inputs(field, bad):
    values = {"diameter": 10, "thickness": 1, "yield_strength": 100, field: bad}
    with pytest.raises(ValueError, match=field):
        api_rp2a_check(**values)


@pytest.mark.parametrize("values", [
    {"diameter": 0}, {"diameter": -10}, {"thickness": 0}, {"thickness": -1},
    {"thickness": 5}, {"thickness": 6}, {"yield_strength": 0},
    {"yield_strength": -100}, {"pressure": -1},
])
def test_rejects_invalid_physical_inputs(values):
    inputs = {"diameter": 10, "thickness": 1, "yield_strength": 100, **values}
    with pytest.raises(ValueError):
        api_rp2a_check(**inputs)


@pytest.mark.parametrize("bad", [0, -1, float("nan"), float("inf"), True])
def test_rejects_invalid_allowables(bad, cited_limit):
    limit, root = cited_limit
    with pytest.raises(ValueError, match="allowable"):
        api_rp2a_check(10, 1, 100, allowables={"bending": replace(limit, value=bad)},
                       citation_root=root)


def test_rejects_wrong_allowable_units(cited_limit):
    limit, root = cited_limit
    with pytest.raises(ValueError, match="MPa"):
        api_rp2a_check(10, 1, 100, allowables={"bending": replace(limit, units="psi")},
                       citation_root=root)


@pytest.mark.parametrize("allowables", [{"shear": 1}, {"unknown": 1}, {"bending": 20}])
def test_rejects_unsupported_or_uncited_allowables(allowables):
    with pytest.raises(ValueError):
        api_rp2a_check(10, 1, 100, allowables=allowables)


@pytest.mark.parametrize("allowables", [[], 20, "bending"])
def test_rejects_nonmapping_allowables(allowables):
    with pytest.raises(ValueError, match="mapping"):
        api_rp2a_check(10, 1, 100, allowables=allowables)


def test_rejects_missing_citation_object(cited_limit):
    limit, root = cited_limit
    with pytest.raises(ValueError, match="Citation"):
        api_rp2a_check(10, 1, 100,
                       allowables={"bending": replace(limit, citation=None)},
                       citation_root=root)


def test_rejects_unrepresentable_unity_ratio(cited_limit):
    limit, root = cited_limit
    with pytest.raises(ValueError, match="representable"):
        api_rp2a_check(10, 1, 100, axial_force=90 * math.pi,
                       allowables={"axial_tension": replace(limit, value=1e-308)},
                       citation_root=root)


def test_missing_citation_fails_closed(cited_limit):
    limit, root = cited_limit
    # An empty sibling folder models an unavailable source without deleting data.
    with pytest.raises(CitationResolutionError):
        api_rp2a_check(10, 1, 100, allowables={"bending": limit},
                       citation_root=root / "missing")


def test_mismatched_citation_fails_closed(cited_limit):
    limit, root = cited_limit
    invalid = replace(limit, citation=replace(limit.citation, revision="unmatched"))
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch"):
        api_rp2a_check(10, 1, 100, allowables={"bending": invalid}, citation_root=root)


@pytest.mark.parametrize("diameter,thickness,force", [
    (1e200, 1e199, 0), (1e-200, 1e-201, 0), (10, 1e-300, 0),
    (1e-50, 1e-51, 1e308),
])
def test_rejects_unrepresentable_geometry_or_stress(diameter, thickness, force):
    with pytest.raises(ValueError, match="representable"):
        api_rp2a_check(diameter, thickness, 100, axial_force=force)


def test_worked_example(capsys):
    """The example uses the first hand vector: tension=bending=10 MPa."""
    example = Path(__file__).resolve().parents[3] / "examples/subsea/api_rp2a_tubular.py"
    runpy.run_path(str(example), run_name="__main__")
    result = json.loads(capsys.readouterr().out)
    assert result["stress_mpa"]["axial_tension"] == 10
    assert result["stress_mpa"]["bending"] == 10
    assert result["status"] == "NOT_EVALUATED"
    assert result["passes"] is None
