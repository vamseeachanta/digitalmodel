# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent arithmetic for the one-foot equations in the public issue.

Source: issue 2958, equations under A.1, accessed 2026-10-19; no standard
edition is established there. These are illustrative inputs, not a material
table or a qualified API 650 design. No licensed table is reproduced.
"""

from dataclasses import replace
import math

import pytest

from digitalmodel.code_checks.storage_tanks import (
    MaterialAllowables,
    OneFootBasis,
    TankDesign,
    design_shell,
)


@pytest.fixture
def tank():
    """Synthetic 30 m illustration requested by issue 2958, part C.

    Stresses 150/160 MPa, density 7800 kg/m3, plates and floor/diameter
    limits are ASSUMED test inputs, not API material or acceptance values.
    The coefficient 4.9 and offset 0.3 are from the public issue's equation
    (edition unspecified); neither is a built-in library default.
    """
    return TankDesign(
        diameter_m=30,
        course_heights_m=(2.25,) * 8,
        design_level_m=18,
        test_level_m=18,
        specific_gravity=1,
        corrosion_allowance_mm=1.5,
        density_kg_m3=7800,
        plate_sizes_mm=(6, 8, 10, 12, 14, 16, 18, 20),
        material=MaterialAllowables("Illustrative steel", 150, 160, "assumed"),
        basis=OneFootBasis(
            coefficient=4.9,
            head_offset_m=0.3,
            diameter_limit_m=60,
            minimum_nominal_mm=6,
            source="Issue 2958; assumed test limits",
        ),
    )


def test_bottom_and_upper_course_hand_calculation(tank):
    """SI arithmetic independent of the implementation.

    Bottom: 4.9*30*(18-0.3)*1/150 + 1.5 = 18.846 mm;
    hydrotest: 4.9*30*17.7/160 = 16.261875 mm; selected = 20 mm.
    Course 7 bottom = 6*2.25 = 13.5 m, H=4.5 m:
    design = 4.9*30*4.2/150 + 1.5 = 5.616 mm;
    test = 4.9*30*4.2/160 = 3.85875 mm; 6 mm floor governs.
    Utilisation = governing nominal requirement / selected nominal plate.
    """
    result = design_shell(tank)
    bottom, upper = result.courses[0], result.courses[6]
    assert bottom.design_mm == pytest.approx(18.846)
    assert bottom.test_mm == pytest.approx(16.261875)
    assert bottom.selected_mm == 20
    assert bottom.utilisation == pytest.approx(0.9423)
    assert upper.bottom_m == 13.5
    assert upper.design_head_m == 4.5
    assert upper.design_mm == pytest.approx(5.616)
    assert upper.test_mm == pytest.approx(3.85875)
    assert upper.required_mm == 6
    assert upper.governing_case == "minimum nominal"


def test_nominal_weight_independent_geometry(tank):
    """Mass = pi*D*sum(course height*selected mm/1000)*density.

    Each course reduces design thickness by 4.9*30*2.25/150 = 2.205 mm.
    Requirements: 18.846,16.641,14.436,12.231,10.026,7.821,5.616,3.411.
    Selected plates from bottom to top: 20,18,16,14,12,8,6,6 mm.
    Sum = 100 mm; mass = pi*30*2.25*0.100*7800 kg.
    Thin-shell mid-surface approximation; no roof or bottom included.
    """
    result = design_shell(tank)
    assert [row.selected_mm for row in result.courses] == [20, 18, 16, 14, 12, 8, 6, 6]
    assert result.shell_mass_kg == pytest.approx(math.pi * 30 * 2.25 * 0.100 * 7800)


def test_diameter_limit_is_inclusive_and_excess_refused(tank):
    """Synthetic 30 m limit: equality passes, any excess requires another method."""
    basis = replace(tank.basis, diameter_limit_m=30)
    assert len(design_shell(replace(tank, basis=basis)).courses) == 8
    with pytest.raises(ValueError, match="5.6.4"):
        design_shell(replace(tank, diameter_m=30.001, basis=basis))


def test_missing_limits_are_exposed_not_assumed(tank):
    basis = replace(tank.basis, minimum_nominal_mm=None, diameter_limit_m=None)
    result = design_shell(replace(tank, basis=basis))
    assert result.status == "NOT_EVALUATED"
    assert any("5.6.1.1" in gap for gap in result.not_evaluated)
    assert any("5.6.3.1" in gap for gap in result.not_evaluated)
    assert result.courses[0].design_mm == pytest.approx(18.846)


def test_assumed_inputs_do_not_establish_code_compliance(tank):
    result = design_shell(tank)
    assert result.status == "NOT_EVALUATED"
    assert any("edition" in gap for gap in result.not_evaluated)
    assert any("allowables" in gap for gap in result.not_evaluated)


def test_hydrotest_can_govern_and_has_no_corrosion_addition(tank):
    """G=0.1 reduces design to 3.2346 mm; hydro remains 16.261875 mm."""
    row = design_shell(replace(tank, specific_gravity=0.1)).courses[0]
    assert row.design_mm == pytest.approx(3.2346)
    assert row.test_mm == pytest.approx(16.261875)
    assert row.governing_case == "hydrostatic test"
    assert row.selected_mm == 18


def test_design_level_sizes_both_cases_and_dry_courses(tank):
    """Both sizing heads use design H=1 m; dry upper courses use floor."""
    result = design_shell(replace(tank, design_level_m=1, test_level_m=2))
    assert result.courses[0].design_head_m == 1
    assert result.courses[0].test_head_m == 1
    assert result.courses[-1].design_mm == 1.5
    assert result.courses[-1].test_mm == 0
    assert result.courses[-1].selected_mm == 6


def test_no_plate_large_enough_is_an_error(tank):
    with pytest.raises(ValueError, match="plate"):
        design_shell(replace(tank, plate_sizes_mm=(6, 8)))


@pytest.mark.parametrize(
    "field,value",
    [
        ("diameter_m", 0),
        ("diameter_m", float("nan")),
        ("specific_gravity", -1),
        ("corrosion_allowance_mm", -1),
        ("design_level_m", 18.001),
        ("test_level_m", -1),
        ("density_kg_m3", float("inf")),
        ("course_heights_m", ()),
        ("course_heights_m", (2, -1)),
        ("plate_sizes_mm", ()),
        ("plate_sizes_mm", (8, 6)),
        ("plate_sizes_mm", (6, 6)),
        ("plate_sizes_mm", (float("nan"),)),
    ],
)
def test_invalid_geometry_and_catalogue_rejected(tank, field, value):
    with pytest.raises(ValueError):
        design_shell(replace(tank, **{field: value}))


@pytest.mark.parametrize(
    "field,value",
    [
        ("coefficient", 0),
        ("head_offset_m", -1),
        ("diameter_limit_m", float("inf")),
        ("minimum_nominal_mm", -1),
        ("source", ""),
    ],
)
def test_invalid_basis_rejected(tank, field, value):
    with pytest.raises(ValueError):
        design_shell(replace(tank, basis=replace(tank.basis, **{field: value})))


def test_invalid_material_rejected(tank):
    with pytest.raises(ValueError):
        design_shell(replace(tank, material=replace(tank.material, design_mpa=0)))


def test_supplied_citation_fails_closed_when_unresolved(tank, tmp_path):
    from digitalmodel.citations.schema import Citation, CitationResolutionError

    citation = Citation(
        code_id="TEST",
        publisher="synthetic",
        revision="test",
        section="Synthetic input only",
        source_sibling="generic",
        wiki_path="wikis/engineering/wiki/concepts/test.md",
    )
    basis = replace(tank.basis, citation=citation)
    with pytest.raises(CitationResolutionError):
        design_shell(replace(tank, basis=basis), wiki_root=tmp_path)


def test_resolved_synthetic_citations_emit_sidecar(tank, tmp_path):
    """Resolver contract only: the temporary source is explicitly synthetic.

    Metadata agreement is tested here; no API value is qualified by a fixture.
    CALCULATED describes equation evaluation, never complete tank compliance.
    """
    from digitalmodel.citations.schema import Citation

    citation = Citation(
        code_id="SYNTHETIC-TEST",
        publisher="synthetic",
        revision="test",
        section="Synthetic arithmetic fixture, not code qualification",
        wiki_path="wikis/engineering/wiki/concepts/test.md",
        source_sibling="generic",
    )
    path = tmp_path / citation.wiki_path
    path.parent.mkdir(parents=True)
    path.write_text(
        "---\ncode_id: SYNTHETIC-TEST\npublisher: synthetic\nrevision: test\n---\n"
    )
    cited = replace(
        tank,
        basis=replace(
            tank.basis, citation=citation, diameter_limit_citation=citation,
            minimum_nominal_citation=citation,
        ),
        material=replace(tank.material, citation=citation),
    )
    result = design_shell(cited, wiki_root=tmp_path)
    assert result.status == "CALCULATED"
    assert result.citation_sidecar()["citations"][0]["source_sibling"] == "generic"
    assert result.courses[0].design_mm == pytest.approx(18.846)


def test_mismatched_citation_metadata_fails_closed(tank, tmp_path):
    from digitalmodel.citations.schema import Citation, CitationResolutionError

    citation = Citation(
        code_id="SYNTHETIC-TEST",
        publisher="synthetic",
        revision="expected",
        section="Synthetic fixture",
        source_sibling="generic",
        wiki_path="wikis/engineering/wiki/concepts/test.md",
    )
    path = tmp_path / citation.wiki_path
    path.parent.mkdir(parents=True)
    path.write_text(
        "---\ncode_id: SYNTHETIC-TEST\npublisher: synthetic\nrevision: other\n---\n"
    )
    with pytest.raises(CitationResolutionError, match="frontmatter_mismatch"):
        design_shell(
            replace(tank, basis=replace(tank.basis, citation=citation)),
            wiki_root=tmp_path,
        )


@pytest.mark.parametrize("test_level", [0, 9, 18])
def test_lower_test_fill_does_not_reduce_hydrotest_sizing(tank, test_level):
    """At design H=18 m: 4.9*30*(18-0.3)/160 = 16.261875 mm.

    With synthetic G=0.1, hydrotest governs and needs an 18 mm plate
    regardless of the separately supplied fill used by the pressure plot.
    """
    result = design_shell(replace(tank, specific_gravity=0.1, test_level_m=test_level))
    row = result.courses[0]
    assert row.test_head_m == 18
    assert row.test_mm == pytest.approx(16.261875)
    assert row.governing_case == "hydrostatic test"
    assert row.selected_mm == 18


def _synthetic_citation(
    tmp_path, revision="test", *, code_id="SYNTHETIC-TEST", publisher="synthetic",
):
    """Metadata fixture only; it does not establish an API edition or values."""
    from digitalmodel.citations.schema import Citation

    citation = Citation(
        code_id=code_id, publisher=publisher, revision=revision,
        section="Synthetic arithmetic only", source_sibling="generic",
        wiki_path=f"wikis/engineering/wiki/concepts/basis-{code_id}-{publisher}-{revision}.md",
    )
    path = tmp_path / citation.wiki_path
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        f"---\ncode_id: {code_id}\npublisher: {publisher}\nrevision: {revision}\n---\n",
        encoding="utf-8",
    )
    return citation


def test_method_citation_does_not_qualify_uncited_limits(tank, tmp_path):
    citation = _synthetic_citation(tmp_path)
    inputs = replace(
        tank, basis=replace(tank.basis, citation=citation),
        material=replace(tank.material, citation=citation),
    )
    result = design_shell(inputs, wiki_root=tmp_path)
    assert result.status == "NOT_EVALUATED"
    assert any("5.6.3.1" in gap for gap in result.not_evaluated)
    assert any("5.6.1.1" in gap for gap in result.not_evaluated)


@pytest.mark.parametrize("missing", ["diameter_limit_citation", "minimum_nominal_citation"])
def test_each_applicability_rule_needs_its_own_evidence(tank, tmp_path, missing):
    citation = _synthetic_citation(tmp_path)
    basis = replace(
        tank.basis, citation=citation, diameter_limit_citation=citation,
        minimum_nominal_citation=citation,
    )
    basis = replace(basis, **{missing: None})
    result = design_shell(
        replace(tank, basis=basis, material=replace(tank.material, citation=citation)),
        wiki_root=tmp_path,
    )
    assert result.status == "NOT_EVALUATED"
    assert len(result.not_evaluated) == 1


def test_mixed_method_and_limit_editions_are_rejected(tank, tmp_path):
    citation = _synthetic_citation(tmp_path)
    other = _synthetic_citation(tmp_path, revision="other")
    basis = replace(
        tank.basis, citation=citation, diameter_limit_citation=other,
        minimum_nominal_citation=citation,
    )
    with pytest.raises(ValueError, match="same edition"):
        design_shell(replace(tank, basis=basis), wiki_root=tmp_path)


@pytest.mark.parametrize("field", ["code_id", "publisher", "revision"])
def test_mixed_applicability_sources_without_method_are_rejected(tank, tmp_path, field):
    citation = _synthetic_citation(tmp_path)
    other = _synthetic_citation(tmp_path, **{field: "other"})
    basis = replace(
        tank.basis, diameter_limit_citation=citation, minimum_nominal_citation=other,
    )
    with pytest.raises(ValueError, match="same edition"):
        design_shell(replace(tank, basis=basis), wiki_root=tmp_path)


def test_matching_applicability_sources_do_not_replace_method_evidence(tank, tmp_path):
    citation = _synthetic_citation(tmp_path)
    basis = replace(
        tank.basis, diameter_limit_citation=citation, minimum_nominal_citation=citation,
    )
    result = design_shell(
        replace(tank, basis=basis, material=replace(tank.material, citation=citation)),
        wiki_root=tmp_path,
    )
    assert result.status == "NOT_EVALUATED"
    assert result.not_evaluated == (
        "NOT_EVALUATED: qualified method edition and coefficients source is missing",
    )
    assert result.citations == (citation,)


@pytest.mark.parametrize(
    "field,citation_field", [
        ("diameter_limit_m", "diameter_limit_citation"),
        ("minimum_nominal_mm", "minimum_nominal_citation"),
    ],
)
def test_missing_limit_value_excludes_only_its_orphan_citation(
    tank, tmp_path, field, citation_field,
):
    citation = _synthetic_citation(tmp_path)
    orphan = replace(citation, section="Synthetic metadata for an absent value")
    basis = replace(
        tank.basis, citation=citation, diameter_limit_citation=citation,
        minimum_nominal_citation=citation, **{field: None},
    )
    basis = replace(basis, **{citation_field: orphan})
    result = design_shell(
        replace(tank, basis=basis, material=replace(tank.material, citation=citation)),
        wiki_root=tmp_path,
    )
    assert result.status == "NOT_EVALUATED"
    assert len(result.not_evaluated) == 1
    assert result.citations == (citation,)
    assert len(result.citation_sidecar()["citations"]) == 1


@pytest.mark.parametrize(
    "field,citation_field", [
        ("diameter_limit_m", "diameter_limit_citation"),
        ("minimum_nominal_mm", "minimum_nominal_citation"),
    ],
)
def test_orphan_citations_still_resolve_fail_closed(tank, tmp_path, field, citation_field):
    from digitalmodel.citations.schema import CitationResolutionError

    citation = replace(
        _synthetic_citation(tmp_path),
        wiki_path="wikis/engineering/wiki/concepts/missing.md",
    )
    basis = replace(tank.basis, **{field: None, citation_field: citation})
    with pytest.raises(CitationResolutionError):
        design_shell(replace(tank, basis=basis), wiki_root=tmp_path)
