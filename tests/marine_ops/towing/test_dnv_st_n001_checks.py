"""Approved-plan strength gates, reaction and geometry limits; no solver."""
import pytest


def test_static_and_dynamic_safety_factors_and_twenty_percent_downsize():
    from digitalmodel.marine_ops.towing.dnv_st_n001_checks import check_towline

    # Static T=100 kN; amplified T=130 kN. Original MBL/Tdyn=3.846 >3.
    result = check_towline(mbl_n=500000, static_tension_n=100000,
                           dynamic_tension_n=130000)
    assert result.passed
    assert result.margin == pytest.approx(110000)
    assert result.citations[0].code_id == 'digitalmodel-towing-plan-487'
    assert result.assessment == 'plan-criterion screening'
    assert result.citations[0].source_sibling == 'generic'
    # Same rope construction and grade: MBL scales with diameter squared.
    reduced = check_towline(mbl_n=500000 * 0.8**2, static_tension_n=100000,
                            dynamic_tension_n=130000)
    assert not reduced.passed
    assert 'dynamic' in reduced.reason
    assert reduced.margin == pytest.approx(-70000)


def test_static_factor_is_enforced_separately():
    from digitalmodel.marine_ops.towing.dnv_st_n001_checks import check_towline

    result = check_towline(mbl_n=166999, static_tension_n=100000,
                           dynamic_tension_n=None)
    assert not result.passed
    boundary = check_towline(mbl_n=167000, static_tension_n=100000,
                             dynamic_tension_n=None)
    assert boundary.passed
    assert boundary.margin == 0


def test_fairlead_reaction_and_included_bridle_angle_limits():
    from digitalmodel.marine_ops.towing.dnv_st_n001_checks import check_fairlead, check_bridle_angle

    assert check_fairlead(reaction_n=900, capacity_n=1000).passed
    assert not check_fairlead(reaction_n=1001, capacity_n=1000).passed
    assert check_bridle_angle(60, minimum_deg=20, maximum_deg=90).passed
    assert not check_bridle_angle(91, minimum_deg=20, maximum_deg=90).passed


def test_missing_citation_fails_closed(tmp_path, monkeypatch):
    from shutil import copytree
    from digitalmodel.citations.schema import CitationResolutionError
    from digitalmodel.marine_ops.towing import dnv_st_n001_checks as checks

    copytree(checks._BASIS_ROOT, tmp_path / 'basis')
    monkeypatch.setattr(checks, '_BASIS_ROOT', tmp_path / 'basis')
    page = tmp_path / 'basis/wikis/towing-plan-487.md'
    page.unlink()
    with pytest.raises(CitationResolutionError, match='digitalmodel-towing-plan-487'):
        checks.check_towline(mbl_n=500000, static_tension_n=100000, dynamic_tension_n=130000)
