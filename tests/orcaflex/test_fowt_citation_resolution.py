# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Test watch-circle citation resolution using synthetic wiki metadata."""

from pathlib import Path
import warnings

import pytest

from digitalmodel.citations import CitationResolutionError, validate_citation
from digitalmodel.orcaflex.mooring_design_fowt import (
    DynamicCableConfig,
    check_watch_circle_vs_cable,
)

WIKI_PATH = "wikis/engineering-standards/wiki/standards/dnv-rp-0360.md"


def cable() -> DynamicCableConfig:
    """Return the existing compliant geometry test input."""
    return DynamicCableConfig(
        suspended_length=250.0,
        hang_off_elevation=80.0,
        nominal_horizontal_span=200.0,
        mbr_limit_m=50.0,
    )


@pytest.mark.parametrize("layout", ["", "knowledge"])
def test_emitted_citation_resolves(tmp_path: Path, layout: str) -> None:
    """Resolve the sidecar in standalone and overlay layouts."""
    page = tmp_path / layout / WIKI_PATH
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: DNV-RP-0360\npublisher: DNV\nrevision: 2016\n---\n",
        encoding="utf-8",
    )
    result = check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path)
    assert len(result.citations) == 1
    citation = result.citations[0]
    assert citation.wiki_path == WIKI_PATH
    assert citation.revision == "2016"
    assert citation.section == "Dynamic cable minimum bend radius (MBR)"
    assert citation.source_sibling == "generic"
    validate_citation(citation, repo_root=tmp_path)


def test_missing_page_warns_once_and_returns_empty_sidecar(tmp_path: Path) -> None:
    """Keep the geometric check available until the standards page exists."""
    (tmp_path / "wikis").mkdir()
    with pytest.warns(RuntimeWarning, match="Watch-circle check proceeds") as caught:
        result = check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path)
    assert len(caught) == 1
    assert str(tmp_path / WIKI_PATH) in str(caught[0].message)
    assert result.passes
    assert result.citations == []
    with warnings.catch_warnings(record=True) as repeated:
        warnings.simplefilter("always")
        result = check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path)
    assert repeated == []
    assert result.citations == []


def test_invalid_wiki_base_fails_at_call_site(tmp_path: Path) -> None:
    """Propagate explicit configuration errors instead of dropping citations."""
    with pytest.raises(CitationResolutionError) as caught:
        check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path)
    assert caught.value.reason.startswith("override_invalid:")


@pytest.mark.parametrize("revision", ["2024-03", None])
def test_invalid_frontmatter_fails_at_calc_time(
    tmp_path: Path, revision: str | None
) -> None:
    """Reject mismatched or missing metadata before returning the result."""
    page = tmp_path / WIKI_PATH
    page.parent.mkdir(parents=True)
    metadata = "---\ncode_id: DNV-RP-0360\npublisher: DNV\n"
    if revision is not None:
        metadata += f"revision: {revision}\n"
    page.write_text(metadata + "---\n", encoding="utf-8")
    with pytest.raises(CitationResolutionError) as caught:
        check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path)
    assert caught.value.reason.startswith("frontmatter_")
