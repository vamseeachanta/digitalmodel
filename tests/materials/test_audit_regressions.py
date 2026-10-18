"""Contracts for the materials post-merge audit (issues 2726-2731)."""
import math

import pytest

from digitalmodel.materials import grades
from digitalmodel.materials.database import load_database
from digitalmodel.materials.ffs_material_library import (
    TrueStressStrainCurve, lookup_grade,
)
from digitalmodel.materials.line_pipe import line_pipe


@pytest.mark.parametrize(
    "name,minimum", [("X42", 415), ("X46", 435), ("X52", 460),
                     ("X56", 490), ("X60", 520), ("X65", 535), ("X70", 570)],
)
def test_psl1_si_minima_and_table_provenance(name, minimum):
    """API 5L 46th ed., 9.3.1 / Table 6 SI minima, not converted legacy ksi."""
    record = load_database().lookup(name, psl="PSL1")
    assert grades.smts_mpa(name, psl="PSL1") == minimum
    assert record.strengths.smts_mpa == minimum
    assert record.edition == "46th ed."
    assert "Table 6" in record.provenance.citation
    assert "9.3.1" in record.provenance.citation


@pytest.mark.parametrize("name", ["X80", "X90", "X100", "X120"])
def test_psl1_unlisted_grades_raise(name):
    """API 5L 46th ed., Table 6 ends at L485/X70."""
    database = load_database()
    assert not any(r.designation == name and r.product_level == "PSL1"
                   for r in database.values())
    for query in (grades.smts_mpa, database.lookup):
        with pytest.raises(ValueError, match="PSL1"):
            query(name, psl="PSL1")


@pytest.mark.parametrize("name", ["A25", "A", "L175", "l210"])
def test_psl1_only_grades_reject_psl2(name):
    """A25/L175 and A/L210 have no Table 7 product level."""
    grade = grades.get(name)
    database = load_database()
    assert grade.smts_mpa is None
    assert "Table 7" not in grade.source
    assert database.lookup(name, psl="PSL1").strengths.smts_mpa == grade.smts_psl1_mpa
    assert {r.product_level for r in database.values()
            if r.designation == grade.name} == {"PSL1"}
    for query in (grades.smts_mpa, grades.smts_psi, grades.smts_pa,
                  grades.smts_kn_m2, database.lookup):
        with pytest.raises(ValueError, match="PSL2"):
            query(name, psl="psl2")
        with pytest.raises(ValueError, match="PSL2"):
            query(name)
    with pytest.raises(ValueError, match="PSL2"):
        _ = grade.smts_psi
    with pytest.raises(ValueError, match="PSL2"):
        lookup_grade(name)


@pytest.mark.parametrize("name", [
    "B", "X42", "X46", "X52", "X56", "X60", "X65", "X70",
    "X80", "X90", "X100", "X120",
])
def test_psl2_provenance_excludes_psl1_table(name):
    record = load_database().lookup(name, psl="PSL2")
    assert "9.3.2" in record.provenance.citation
    assert "Table 7" in record.provenance.citation
    assert "9.3.1" not in record.provenance.citation
    assert "Table 6" not in record.provenance.citation


@pytest.mark.parametrize("psl", ["unknown", "PSL3", ""])
def test_unknown_api_product_level_raises(psl):
    for query in (grades.smts_mpa, load_database().lookup):
        with pytest.raises(ValueError, match="PSL"):
            query("X52", psl=psl)


@pytest.mark.parametrize(
    "name", ["API 5L B", "API 5L Grade B", "X52 PSL2 Grade E",
             "unknownA", "Grade nonsense B", "", "   "],
)
def test_qualified_or_unknown_grade_never_becomes_hull_grade(name):
    """Issue 2727: exact identities only; no inferred qualified-name grammar."""
    for query in (grades.get, lookup_grade, load_database().lookup):
        with pytest.raises(KeyError, match="Unknown material grade"):
            query(name)
    with pytest.raises(KeyError, match="Unknown material grade"):
        line_pipe(name, od_in=12.0, wt_in=0.5)


@pytest.mark.parametrize("name,canonical", [
    ("A", "A"), ("b", "B"), ("d", "Grade D"), ("E", "Grade E"),
    (" grade a ", "Grade A"), ("gRaDe B", "Grade B"),
    ("l360", "X52"), ("316L", "TP316L"),
])
def test_exact_grade_aliases_remain_available(name, canonical):
    assert grades.get(name).name == canonical


@pytest.mark.parametrize("extra,error", [
    ({}, TypeError), ({"ultimate_true_strain": None}, ValueError),
])
def test_curve_requires_explicit_uniform_strain(extra, error):
    """The offset law does not imply eps_u=n; issue 2731 requires input."""
    with pytest.raises(error, match="ultimate_true_strain"):
        TrueStressStrainCurve(E_mpa=207000, sigma_y_mpa=450,
                             sigma_u_mpa=535, n=0.12, **extra)
    with pytest.raises(error, match="ultimate_true_strain"):
        lookup_grade("X65").true_stress_strain_curve(n=0.12, **extra)


@pytest.mark.parametrize("already_true", [False, True])
def test_hardening_closed_form_anchor(already_true):
    """Offset law: 300+(600-300)*sqrt(.025/.1)=450 MPa.

    eps_u=.103, E=200000 gives ep_u=.1; total eps=.025+450/200000=.02725.
    Uniform conversion sigma_true=sigma_eng*exp(eps_u) is checked separately
    from the prescribed endpoint by exercising both hardening evaluators.
    """
    ultimate = 600.0 if already_true else 600.0 * math.exp(-0.103)
    curve = TrueStressStrainCurve(E_mpa=200000, sigma_y_mpa=300,
                                 sigma_u_mpa=ultimate, n=0.5,
                                 ultimate_true_strain=0.103,
                                 sigma_u_is_true=already_true)
    assert curve.plastic_strain_at_ultimate == pytest.approx(0.1, abs=1e-12)
    assert curve.stress_from_plastic_strain(0.025) == pytest.approx(450, abs=1e-8)
    assert curve.stress(0.02725) == pytest.approx(450, abs=1e-8)
    assert curve.plastic_strain(0.02725) == pytest.approx(0.025, abs=1e-10)
