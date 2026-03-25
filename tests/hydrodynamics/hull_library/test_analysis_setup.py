"""Tests for hull analysis setup skill.

ABOUTME: Covers HullAnalysisInput validation, HullAnalysisResult field
contracts, hull selection accuracy, graceful fallback for missing mesh/RAO,
and the full setup_hull_analysis chain.
"""

import pytest

from digitalmodel.hydrodynamics.hull_library.analysis_setup import (
    SKILL_NAME,
    HullAnalysisInput,
    HullAnalysisResult,
    setup_hull_analysis,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def lngc_input() -> HullAnalysisInput:
    """LNGC-class vessel matching built-in LNGC-250 hull almost exactly."""
    return HullAnalysisInput(loa_m=250.0, beam_m=43.0, draft_m=11.5)


@pytest.fixture
def small_input() -> HullAnalysisInput:
    """Small vessel — should match BOX-50, not any large hull."""
    return HullAnalysisInput(loa_m=50.0, beam_m=12.0, draft_m=2.0)


@pytest.fixture
def fpso_input() -> HullAnalysisInput:
    """Large FPSO — should match FPSO-320 or FPSO-260, not LNGC-250."""
    return HullAnalysisInput(loa_m=320.0, beam_m=60.0, draft_m=18.0)


# ---------------------------------------------------------------------------
# Basic field contract tests
# ---------------------------------------------------------------------------


def test_returns_hull_analysis_result(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert isinstance(result, HullAnalysisResult)


def test_hull_id_is_nonempty_string(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert isinstance(result.hull_id, str)
    assert len(result.hull_id) > 0


def test_similarity_score_in_unit_interval(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert 0.0 <= result.similarity_score <= 1.0


def test_scaling_factors_has_required_keys(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert "loa" in result.scaling_factors
    assert "beam" in result.scaling_factors
    assert "draft" in result.scaling_factors


def test_scaling_factors_are_positive_floats(lngc_input):
    result = setup_hull_analysis(lngc_input)
    for key in ("loa", "beam", "draft"):
        assert result.scaling_factors[key] > 0.0


def test_source_field_is_correct_string(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert result.source == "skill:hull_analysis_setup"


def test_source_prefixed_with_skill_namespace(lngc_input):
    """source should be in the 'skill:<name>' namespace format."""
    result = setup_hull_analysis(lngc_input)
    assert result.source.startswith("skill:")
    assert SKILL_NAME in result.source


def test_summary_is_dict(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert isinstance(result.summary, dict)


def test_summary_contains_hull_id_key(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert "hull_id" in result.summary


def test_summary_hull_id_matches_result_hull_id(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert result.summary["hull_id"] == result.hull_id


# ---------------------------------------------------------------------------
# Hull selection accuracy tests
# ---------------------------------------------------------------------------


def test_lngc_dimensions_selects_lngc_hull(lngc_input):
    """Exact LNGC-250 dims → LNGC-250 selected."""
    result = setup_hull_analysis(lngc_input)
    assert result.hull_id == "LNGC-250"


def test_lngc_similarity_score_near_one(lngc_input):
    """Exact match yields similarity very close to 1.0."""
    result = setup_hull_analysis(lngc_input)
    assert result.similarity_score > 0.99


def test_small_vessel_matches_box_50_not_lngc(small_input):
    result = setup_hull_analysis(small_input)
    assert result.hull_id == "BOX-50"
    assert "LNGC" not in result.hull_id


def test_large_fpso_matches_fpso_320(fpso_input):
    result = setup_hull_analysis(fpso_input)
    assert "FPSO" in result.hull_id


def test_large_fpso_does_not_match_box_50(fpso_input):
    result = setup_hull_analysis(fpso_input)
    assert result.hull_id != "BOX-50"


# ---------------------------------------------------------------------------
# Mesh refinement option tests
# ---------------------------------------------------------------------------


def test_refinement_zero_mesh_quality_is_none_or_skipped(lngc_input):
    """With mesh_refinement_levels=0 and no PanelMesh in builtin set,
    mesh_quality should be None (graceful fallback)."""
    inp = HullAnalysisInput(
        loa_m=lngc_input.loa_m,
        beam_m=lngc_input.beam_m,
        draft_m=lngc_input.draft_m,
        mesh_refinement_levels=0,
    )
    result = setup_hull_analysis(inp)
    # Builtin hull set has no actual PanelMesh files → mesh_quality is None
    assert result.mesh_quality is None


def test_refinement_default_returns_result_without_raising(lngc_input):
    """Default refinement_levels=1 should not raise even with no mesh file."""
    result = setup_hull_analysis(lngc_input)
    assert result is not None


# ---------------------------------------------------------------------------
# RAO option tests
# ---------------------------------------------------------------------------


def test_include_rao_false_gives_rao_available_false(lngc_input):
    inp = HullAnalysisInput(
        loa_m=lngc_input.loa_m,
        beam_m=lngc_input.beam_m,
        draft_m=lngc_input.draft_m,
        include_rao=False,
    )
    result = setup_hull_analysis(inp)
    assert result.rao_available is False


def test_include_rao_false_gives_rao_data_none(lngc_input):
    inp = HullAnalysisInput(
        loa_m=lngc_input.loa_m,
        beam_m=lngc_input.beam_m,
        draft_m=lngc_input.draft_m,
        include_rao=False,
    )
    result = setup_hull_analysis(inp)
    assert result.rao_data is None


def test_include_rao_true_no_dir_graceful_fallback(lngc_input):
    """include_rao=True but raos_dir=None → graceful fallback, no exception."""
    result = setup_hull_analysis(lngc_input, raos_dir=None)
    assert result.rao_available is False
    assert result.rao_data is None


# ---------------------------------------------------------------------------
# Displacement optional tests
# ---------------------------------------------------------------------------


def test_displacement_optional_no_error_when_omitted():
    """Displacement is optional — omitting it should not raise."""
    inp = HullAnalysisInput(loa_m=150.0, beam_m=25.0, draft_m=7.0)
    result = setup_hull_analysis(inp)
    assert result is not None


def test_displacement_provided_is_accepted():
    inp = HullAnalysisInput(
        loa_m=150.0, beam_m=25.0, draft_m=7.0, displacement_t=8000.0
    )
    result = setup_hull_analysis(inp)
    assert result.hull_id is not None


# ---------------------------------------------------------------------------
# Validation / error-path tests
# ---------------------------------------------------------------------------


def test_missing_loa_raises_value_error():
    with pytest.raises((ValueError, TypeError)):
        inp = HullAnalysisInput(loa_m=None, beam_m=43.0, draft_m=11.5)  # type: ignore[arg-type]
        setup_hull_analysis(inp)


def test_negative_loa_raises_value_error():
    with pytest.raises(ValueError, match="loa_m"):
        inp = HullAnalysisInput(loa_m=-10.0, beam_m=43.0, draft_m=11.5)
        setup_hull_analysis(inp)


def test_zero_beam_raises_value_error():
    with pytest.raises(ValueError, match="beam_m"):
        inp = HullAnalysisInput(loa_m=250.0, beam_m=0.0, draft_m=11.5)
        setup_hull_analysis(inp)


def test_negative_draft_raises_value_error():
    with pytest.raises(ValueError, match="draft_m"):
        inp = HullAnalysisInput(loa_m=250.0, beam_m=43.0, draft_m=-5.0)
        setup_hull_analysis(inp)


# ---------------------------------------------------------------------------
# Summary completeness tests
# ---------------------------------------------------------------------------


def test_summary_contains_target_dimensions(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert result.summary.get("target_loa_m") == lngc_input.loa_m
    assert result.summary.get("target_beam_m") == lngc_input.beam_m
    assert result.summary.get("target_draft_m") == lngc_input.draft_m


def test_summary_rao_available_key_present(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert "rao_available" in result.summary


def test_summary_scaling_factors_present(lngc_input):
    result = setup_hull_analysis(lngc_input)
    assert "scaling_factors" in result.summary


# ---------------------------------------------------------------------------
# Exact scaling factors for exact match
# ---------------------------------------------------------------------------


def test_exact_match_scaling_factors_near_one(lngc_input):
    """When target == reference hull dims, scale factors should be ~1.0."""
    result = setup_hull_analysis(lngc_input)
    assert abs(result.scaling_factors["loa"] - 1.0) < 0.01
    assert abs(result.scaling_factors["beam"] - 1.0) < 0.01
    assert abs(result.scaling_factors["draft"] - 1.0) < 0.01
