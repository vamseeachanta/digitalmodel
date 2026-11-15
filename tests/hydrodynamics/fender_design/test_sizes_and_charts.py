"""Neighbouring-size checks carry the case's design adjustments (tolerances,
angular factor, WG211 capacity divisor) before the limit comparison, and the
sensitivity chart shades only the contiguous intervals that are not rated.

Fender rows are the fictional illustrative rows of the generic case.
"""

from __future__ import annotations

import sys
from pathlib import Path

import pytest

from digitalmodel.hydrodynamics import fender_design as fd
from digitalmodel.hydrodynamics.berthing_energy import FenderRating

ROOT = Path(__file__).resolve().parents[3]
EXAMPLE = ROOT / "examples" / "hydrodynamics" / "fender_design"
sys.path.insert(0, str(EXAMPLE))

pytest.importorskip("yaml")
pytest.importorskip("matplotlib")

import fender_charts as fc  # noqa: E402
from fender_case import compute, load_case, size_checks  # noqa: E402

PN4 = FenderRating("GEN-PN-4", "generic", "pneumatic", 2600, 4500, 80, 700.0, 1450.0, 210.0, 60.0, "fictional")
PN5 = FenderRating("GEN-PN-5", "generic", "pneumatic", 3000, 5000, 50, 900.0, 1600.0, 150.0, 60.0, "fictional")
DESIGN = dict(energy_tolerance=0.0, reaction_tolerance=0.10, angular_factor=1.0,
              reaction_limit_kn=1600.0, hull_pressure_limit_kpa=200.0)


def test_neighbour_reaction_includes_tolerance():
    # GEN-PN-5 is rated at 1,600 kN, the limit; with +10 % it is 1,760 kN and exceeds it
    c = {x.fender_id: x for x in size_checks([PN4, PN5], "GEN-PN-4", 500.0, DESIGN)}
    assert c["GEN-PN-5"].rated_reaction_kn == 1600.0
    assert c["GEN-PN-5"].reaction_kn == pytest.approx(1760.0)
    assert c["GEN-PN-5"].meets_reaction is False
    assert c["GEN-PN-5"].hull_pressure_kpa == pytest.approx(165.0)
    assert c["GEN-PN-4"].reaction_kn == pytest.approx(1595.0)
    assert c["GEN-PN-4"].meets_reaction is True
    assert c["GEN-PN-4"].hull_pressure_kpa == pytest.approx(231.0)   # 210 x 1.10 > 200 kPa
    assert c["GEN-PN-4"].meets_pressure is False
    assert c["GEN-PN-4"].is_base and not c["GEN-PN-5"].is_base


def test_neighbour_energy_includes_reductions_and_wg211_divisor():
    d = dict(DESIGN, energy_tolerance=0.10, angular_factor=0.95)
    c = {x.fender_id: x for x in size_checks([PN4, PN5], "GEN-PN-4", 700.0, d, capacity_divisor=1.10)}
    cap = 900.0 * 0.90 * 0.95 / 1.10                 # 699.5 kNm, below the 700 kNm demand
    assert c["GEN-PN-5"].capacity_knm == pytest.approx(cap)
    assert c["GEN-PN-5"].utilisation == pytest.approx(700.0 / cap)
    assert c["GEN-PN-5"].meets_energy is False        # rated 900 kNm alone would pass
    assert not c["GEN-PN-5"].meets_all


def test_no_limit_gives_no_check():
    c = size_checks([PN5], "GEN-PN-5", 500.0, dict(energy_tolerance=0.0, reaction_tolerance=0.10))[0]
    assert c.meets_reaction is None and c.meets_pressure is None
    assert c.meets_all
    assert not c.pressure_missing


def test_missing_hull_pressure_with_a_limit_does_not_meet_it():
    # adequate energy and reaction, no hull-pressure datum, 200 kPa limit: compliance cannot be shown
    row = FenderRating("GEN-FM-X", "generic", "foam", 2800, 5000, None, 1000.0, 1400.0, None, 60.0, "fictional")
    c = size_checks([row], "GEN-FM-X", 500.0, DESIGN)[0]
    assert c.meets_energy and c.meets_reaction is True
    assert c.hull_pressure_kpa is None
    assert c.meets_pressure is False and c.pressure_missing
    assert not c.meets_all
    # the same row as select_fenders treats it: excluded under the limit, kept without one
    from digitalmodel.hydrodynamics.berthing_energy import select_fenders
    kw = dict(reaction_tolerance=0.10, max_reaction_kn=1600.0)
    assert select_fenders([row], 500.0, max_hull_pressure_kpa=200.0, **kw) == []
    assert len(select_fenders([row], 500.0, **kw)) == 1
    # without a pressure limit the missing datum is not checked and does not fail
    free = size_checks([row], "GEN-FM-X", 500.0, dict(DESIGN, hull_pressure_limit_kpa=None))[0]
    assert free.meets_pressure is None and not free.pressure_missing and free.meets_all


def test_generic_case_size_checks_match_the_base_fender_capacity():
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    checks = {c.fender_id: c for c in res.size_checks}
    base = checks[res.base_fender.fender_id]
    assert base.capacity_knm == pytest.approx(res.capacity_knm)
    assert base.reaction_kn == pytest.approx(res.design.reaction_kn)
    assert checks["GEN-PN-5"].reaction_kn == pytest.approx(1760.0)
    assert checks["GEN-PN-5"].meets_reaction is False
    assert res.citation_mode == "record"


def test_case_citation_mode_defaults_to_strict(tmp_path):
    case = load_case(EXAMPLE / "generic_case.yaml")
    case["citations"] = {"wiki_root": str(tmp_path)}  # a directory without the cited pages
    from digitalmodel.citations.schema import CitationResolutionError
    with pytest.raises(CitationResolutionError):
        compute(case)
    with pytest.raises(ValueError):
        compute(case, citations="lenient")


# ------------------------------------------------------------ chart shading


def test_unrated_intervals_are_contiguous_runs():
    xs = [10, 11, 12, 13, 14, 15, 16]
    rated = [False, False, True, True, False, True, True]
    assert fc.unrated_intervals(xs, rated) == [(10, 11.5), (13.5, 14.5)]
    assert fc.unrated_intervals(xs, [True] * 7) == []
    assert fc.unrated_intervals(xs, [True] * 6 + [False]) == [(15.5, 16)]
    assert fc.unrated_intervals(xs, [False] * 7) == [(10, 16)]
    with pytest.raises(ValueError):
        fc.unrated_intervals(xs, [True])


def test_water_depth_shading_leaves_the_base_point_unshaded():
    res = compute(load_case(EXAMPLE / "generic_case.yaml"), citations="record")
    sw = next(s for s in res.sweeps if s.parameter == "water_depth_m")
    pts = sw.points
    spans = fc.unrated_intervals([p.value for p in pts], [p.reaction_kn is not None for p in pts])
    assert any(p.reaction_kn is None for p in pts) and spans    # shallow water exceeds the rating
    for p in pts:
        shaded = any(lo <= p.value <= hi for lo, hi in spans)
        assert shaded == (p.reaction_kn is None), p.value
    assert sw.at(sw.base_value).reaction_kn is not None
    assert not any(lo <= sw.base_value <= hi for lo, hi in spans)
