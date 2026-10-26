"""Tests for nearest-neighbour hull form lookup.

ABOUTME: Unit tests for HullLookupTarget, HullMatch, HullLookup, and the
get_hull_form convenience function in the hull_library lookup module.
"""

import pytest

from digitalmodel.hydrodynamics.hull_library.lookup import (
    HullLookup,
    HullLookupTarget,
    HullMatch,
    _BUILTIN_HULLS,
    get_hull_form,
)
from digitalmodel.hydrodynamics.hull_library.panel_catalog import (
    PanelCatalog,
    PanelCatalogEntry,
    PanelFormat,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import HullType


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def lngc_250_target() -> HullLookupTarget:
    return HullLookupTarget(loa_m=250.0, beam_m=43.0, draft_m=11.5)


@pytest.fixture
def fpso_260_target() -> HullLookupTarget:
    return HullLookupTarget(loa_m=260.0, beam_m=46.0, draft_m=14.0)


@pytest.fixture
def small_vessel_target() -> HullLookupTarget:
    return HullLookupTarget(loa_m=50.0, beam_m=12.0, draft_m=2.0)


@pytest.fixture
def default_lookup() -> HullLookup:
    return HullLookup()


@pytest.fixture
def panel_catalog_with_entries() -> PanelCatalog:
    entries = [
        PanelCatalogEntry(
            hull_id="PC-TANKER-200",
            hull_type=HullType.TANKER,
            name="200m Tanker",
            source="test",
            panel_format=PanelFormat.GDF,
            file_path="data/pc_tanker_200.gdf",
            length_m=200.0,
            beam_m=32.0,
            draft_m=12.0,
            displacement_t=55000.0,
        ),
        PanelCatalogEntry(
            hull_id="PC-BARGE-80",
            hull_type=HullType.BARGE,
            name="80m Barge",
            source="test",
            panel_format=PanelFormat.GDF,
            file_path="data/pc_barge_80.gdf",
            length_m=80.0,
            beam_m=20.0,
            draft_m=3.5,
            displacement_t=4000.0,
        ),
    ]
    return PanelCatalog(entries=entries)


# ---------------------------------------------------------------------------
# HullLookupTarget
# ---------------------------------------------------------------------------


class TestHullLookupTarget:
    def test_required_fields_set(self):
        t = HullLookupTarget(loa_m=200.0, beam_m=35.0, draft_m=10.0)
        assert t.loa_m == 200.0
        assert t.beam_m == 35.0
        assert t.draft_m == 10.0

    def test_displacement_optional_defaults_none(self):
        t = HullLookupTarget(loa_m=100.0, beam_m=20.0, draft_m=5.0)
        assert t.displacement_t is None

    def test_displacement_set(self):
        t = HullLookupTarget(loa_m=250.0, beam_m=43.0, draft_m=11.5, displacement_t=90000.0)
        assert t.displacement_t == 90000.0


# ---------------------------------------------------------------------------
# HullMatch
# ---------------------------------------------------------------------------


class TestHullMatch:
    def test_has_required_fields(self):
        m = HullMatch(
            hull_id="FST-100",
            similarity_score=0.9,
            scaling_factors={"loa": 1.0, "beam": 1.0, "draft": 1.0},
            matched_entry={},
            source="builtin",
        )
        assert m.hull_id == "FST-100"
        assert m.similarity_score == 0.9
        assert "loa" in m.scaling_factors
        assert "beam" in m.scaling_factors
        assert "draft" in m.scaling_factors

    def test_similarity_score_in_unit_interval(self):
        m = HullMatch(
            hull_id="X",
            similarity_score=0.75,
            scaling_factors={"loa": 1.0, "beam": 1.0, "draft": 1.0},
            matched_entry=None,
            source="builtin",
        )
        assert 0.0 <= m.similarity_score <= 1.0


# ---------------------------------------------------------------------------
# get_hull_form convenience function
# ---------------------------------------------------------------------------


class TestGetHullFormConvenienceFunction:
    def test_returns_hull_match(self):
        result = get_hull_form({"loa_m": 250, "beam_m": 43, "draft_m": 11.5})
        assert isinstance(result, HullMatch)

    def test_similarity_score_in_unit_interval(self):
        result = get_hull_form({"loa_m": 250, "beam_m": 43, "draft_m": 11.5})
        assert 0.0 <= result.similarity_score <= 1.0

    def test_scaling_factors_has_loa_beam_draft(self):
        result = get_hull_form({"loa_m": 100, "beam_m": 18, "draft_m": 5.5})
        assert set(result.scaling_factors.keys()) >= {"loa", "beam", "draft"}

    def test_exact_match_returns_score_close_to_one(self):
        result = get_hull_form({"loa_m": 100.0, "beam_m": 18.0, "draft_m": 5.5})
        assert result.similarity_score > 0.95

    def test_missing_beam_raises_value_error(self):
        with pytest.raises((ValueError, KeyError)):
            get_hull_form({"loa_m": 250, "draft_m": 11.5})

    def test_missing_loa_raises_value_error(self):
        with pytest.raises((ValueError, KeyError)):
            get_hull_form({"beam_m": 43, "draft_m": 11.5})

    def test_missing_draft_raises_value_error(self):
        with pytest.raises((ValueError, KeyError)):
            get_hull_form({"loa_m": 250, "beam_m": 43})

    def test_displacement_optional(self):
        result_without = get_hull_form({"loa_m": 250, "beam_m": 43, "draft_m": 11.5})
        result_with = get_hull_form({
            "loa_m": 250,
            "beam_m": 43,
            "draft_m": 11.5,
            "displacement_t": 90000,
        })
        assert isinstance(result_without, HullMatch)
        assert isinstance(result_with, HullMatch)

    def test_very_different_dimensions_still_returns_result(self):
        result = get_hull_form({"loa_m": 999, "beam_m": 200, "draft_m": 50})
        assert isinstance(result, HullMatch)
        assert result.similarity_score >= 0.0

    def test_lngc_target_matches_lngc_hull(self):
        result = get_hull_form({"loa_m": 250, "beam_m": 43, "draft_m": 11.5})
        assert "LNGC" in result.hull_id

    def test_fpso_target_matches_fpso_hull(self):
        result = get_hull_form({"loa_m": 260, "beam_m": 46, "draft_m": 14.0})
        assert "FPSO" in result.hull_id

    def test_small_vessel_matches_small_hull_not_lngc(self):
        result = get_hull_form({"loa_m": 50, "beam_m": 12, "draft_m": 2.0})
        assert "LNGC" not in result.hull_id

    def test_hull_id_is_string(self):
        result = get_hull_form({"loa_m": 150, "beam_m": 25, "draft_m": 7.0})
        assert isinstance(result.hull_id, str)
        assert len(result.hull_id) > 0

    def test_source_field_present(self):
        result = get_hull_form({"loa_m": 150, "beam_m": 25, "draft_m": 7.0})
        assert result.source in {"builtin", "catalog", "panel_catalog"}


# ---------------------------------------------------------------------------
# HullLookup.find_closest
# ---------------------------------------------------------------------------


class TestHullLookupFindClosest:
    def test_returns_list(self, default_lookup, lngc_250_target):
        result = default_lookup.find_closest(lngc_250_target, n=3)
        assert isinstance(result, list)

    def test_returns_three_results_for_n3(self, default_lookup, lngc_250_target):
        result = default_lookup.find_closest(lngc_250_target, n=3)
        assert len(result) == 3

    def test_results_ordered_by_similarity_descending(self, default_lookup, lngc_250_target):
        results = default_lookup.find_closest(lngc_250_target, n=4)
        scores = [r.similarity_score for r in results]
        assert scores == sorted(scores, reverse=True)

    def test_find_closest_n1_matches_get_hull_form(self, default_lookup, fpso_260_target):
        top1 = default_lookup.find_closest(fpso_260_target, n=1)
        best = default_lookup.get_hull_form(fpso_260_target)
        assert top1[0].hull_id == best.hull_id

    def test_all_results_are_hull_match(self, default_lookup, small_vessel_target):
        results = default_lookup.find_closest(small_vessel_target, n=3)
        for r in results:
            assert isinstance(r, HullMatch)

    def test_all_scores_in_unit_interval(self, default_lookup, lngc_250_target):
        results = default_lookup.find_closest(lngc_250_target, n=5)
        for r in results:
            assert 0.0 <= r.similarity_score <= 1.0

    def test_missing_beam_raises_value_error(self, default_lookup):
        bad_target = HullLookupTarget(loa_m=250.0, beam_m=-1.0, draft_m=11.5)
        with pytest.raises(ValueError):
            default_lookup.find_closest(bad_target)

    def test_n_larger_than_catalog_returns_all(self, default_lookup, lngc_250_target):
        results = default_lookup.find_closest(lngc_250_target, n=100)
        assert len(results) == len(_BUILTIN_HULLS)

    def test_exact_builtin_match_scores_near_one(self, default_lookup):
        target = HullLookupTarget(loa_m=300.0, beam_m=50.0, draft_m=13.0)
        best = default_lookup.get_hull_form(target)
        assert best.hull_id == "LNGC-300"
        assert best.similarity_score > 0.95


# ---------------------------------------------------------------------------
# HullLookup with PanelCatalog
# ---------------------------------------------------------------------------


class TestHullLookupWithPanelCatalog:
    def test_uses_panel_catalog_entries(self, panel_catalog_with_entries):
        lookup = HullLookup(catalog=panel_catalog_with_entries)
        target = HullLookupTarget(loa_m=200.0, beam_m=32.0, draft_m=12.0)
        result = lookup.get_hull_form(target)
        assert result.hull_id == "PC-TANKER-200"
        assert result.source == "panel_catalog"

    def test_panel_catalog_barge_match(self, panel_catalog_with_entries):
        lookup = HullLookup(catalog=panel_catalog_with_entries)
        target = HullLookupTarget(loa_m=80.0, beam_m=20.0, draft_m=3.5)
        result = lookup.get_hull_form(target)
        assert result.hull_id == "PC-BARGE-80"

    def test_panel_catalog_exact_match_score_near_one(self, panel_catalog_with_entries):
        lookup = HullLookup(catalog=panel_catalog_with_entries)
        target = HullLookupTarget(loa_m=200.0, beam_m=32.0, draft_m=12.0)
        result = lookup.get_hull_form(target)
        assert result.similarity_score > 0.95


# ---------------------------------------------------------------------------
# HullLookup without catalog — falls back to built-ins
# ---------------------------------------------------------------------------


class TestHullLookupFallback:
    def test_none_catalog_uses_builtins(self):
        lookup = HullLookup(catalog=None)
        assert len(lookup._candidates) == len(_BUILTIN_HULLS)

    def test_empty_panel_catalog_falls_back_to_builtins(self):
        empty = PanelCatalog(entries=[])
        lookup = HullLookup(catalog=empty)
        assert len(lookup._candidates) == len(_BUILTIN_HULLS)


@pytest.fixture
def synthetic_lookup():
    """Use assumed dimensions chosen for independent division by hand.

    The target is (120, 24, 6) metres. The smaller candidate (60, 8, 3)
    needs scales (120/60, 24/8, 6/3) = (2, 3, 2). The exact candidate
    needs (1, 1, 1). No panel file is read or written.
    """
    entries = [
        PanelCatalogEntry(
            hull_id=hull_id,
            hull_type=HullType.BARGE,
            name=hull_id,
            source="synthetic",
            panel_format=PanelFormat.GDF,
            file_path=f"synthetic/{hull_id}.gdf",
            length_m=length,
            beam_m=beam,
            draft_m=draft,
        )
        for hull_id, length, beam, draft in (
            ("synthetic-small", 60.0, 8.0, 3.0),
            ("synthetic-exact", 120.0, 24.0, 6.0),
        )
    ]
    return HullLookup(PanelCatalog(entries=entries))


@pytest.fixture
def synthetic_target():
    """Return the assumed target defined in synthetic_lookup's hand example."""
    return HullLookupTarget(loa_m=120.0, beam_m=24.0, draft_m=6.0)


@pytest.mark.parametrize("n", [0, -1, 1.5, True, False, 1.0, None, "2"])
def test_lookup_rejects_invalid_result_count(synthetic_lookup, synthetic_target, n):
    """A positive integer count excludes zero, negatives and non-integer types."""
    with pytest.raises(ValueError, match="n must be a positive integer"):
        synthetic_lookup.find_closest(synthetic_target, n=n)


@pytest.mark.parametrize(
    "displacement",
    [0.0, -1.0, float("nan"), float("inf"), -float("inf"), True, False, "bad"],
)
@pytest.mark.parametrize("api", ["find_closest", "get_hull_form", "convenience"])
def test_lookup_rejects_invalid_optional_displacement(
    synthetic_lookup, synthetic_target, displacement, api
):
    """Supplied displacement must be a finite positive number, per query contract."""
    synthetic_target.displacement_t = displacement
    with pytest.raises(ValueError, match="displacement_t must be a finite positive number"):
        if api == "convenience":
            get_hull_form(vars(synthetic_target))
        else:
            getattr(synthetic_lookup, api)(synthetic_target)


@pytest.mark.parametrize("displacement", [None, 1.0])
def test_synthetic_lookup_exact_match(synthetic_lookup, synthetic_target, displacement):
    """Equal dimensions give distance zero, exp(0) = 1 and axis ratios of one.

    None omits displacement; an assumed unit displacement remains valid and
    does not change the dimension-only score. Expected scales are the hand
    divisions in synthetic_lookup, independent of the lookup implementation.
    """
    synthetic_target.displacement_t = displacement
    best = synthetic_lookup.get_hull_form(synthetic_target)
    assert best.hull_id == "synthetic-exact"
    assert best.source == "panel_catalog"
    assert best.similarity_score == 1.0
    assert best.scaling_factors == {"loa": 1.0, "beam": 1.0, "draft": 1.0}

    # Three requested results exceed the two synthetic candidates by one.
    matches = synthetic_lookup.find_closest(synthetic_target, n=3)
    assert [match.hull_id for match in matches] == ["synthetic-exact", "synthetic-small"]
    assert matches[0] == best
    assert matches[1].similarity_score < best.similarity_score
    assert matches[1].scaling_factors == {"loa": 2.0, "beam": 3.0, "draft": 2.0}
