"""The depth-band reference validates and fails closed for missing records."""

from pathlib import Path

import pytest

from digitalmodel.citations.registry import get_pianc_depth_band_reference
from digitalmodel.citations.schema import CitationResolutionError


def test_published_depth_band_reference():
    reference = get_pianc_depth_band_reference(
        repo_root=Path(__file__).parent / "fixtures" / "knowledge",
    )
    assert reference.units == "reference"
    assert reference.citation.code_id == "PIANC-DEPTH-BANDS"
    assert "Appendix A.1, p. 201" in reference.citation.section
    assert "10.1016/j.apor.2026.105289" in reference.citation.note


def test_depth_reference_fails_closed_without_record(tmp_path):
    with pytest.raises(CitationResolutionError, match="page_missing"):
        get_pianc_depth_band_reference(repo_root=tmp_path)
