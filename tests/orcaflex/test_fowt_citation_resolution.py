# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Test watch-circle citation resolution using synthetic wiki metadata."""

from pathlib import Path
import errno
import traceback
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
    assert citation.note == "MBR limit is a cable-specific design input supplied by caller"
    validate_citation(citation, repo_root=tmp_path)


@pytest.mark.parametrize("layout", ["", "knowledge"])
def test_missing_page_fails_at_call_site(tmp_path: Path, layout: str) -> None:
    """Reject a configured wiki without the page on every calculation."""
    (tmp_path / layout / "wikis").mkdir(parents=True)
    for _ in range(2):
        with pytest.raises(CitationResolutionError) as caught:
            check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path)
        assert caught.value.code_id == "DNV-RP-0360"
        assert caught.value.wiki_path == WIKI_PATH
        assert caught.value.reason == "page_missing"


def test_unconfigured_wiki_warns_once_and_returns_empty_sidecar(monkeypatch) -> None:
    """Keep standalone screening distinguishable from a missing configured page."""
    from digitalmodel.citations import resolver
    from digitalmodel.orcaflex import mooring_design_fowt

    def unconfigured(**kwargs):
        raise CitationResolutionError(
            code_id="<resolver>", wiki_path="<base>", reason="resolver_unconfigured: test"
        )

    monkeypatch.setattr(resolver, "resolve_wiki_base", unconfigured)
    monkeypatch.setattr(mooring_design_fowt, "_CITATION_EMITTED_CACHE", {})
    with pytest.warns(RuntimeWarning, match="no wiki tree found"):
        result = check_watch_circle_vs_cable(20.0, cable())
    assert result.passes
    assert result.citations == []
    with warnings.catch_warnings(record=True) as repeated:
        warnings.simplefilter("always")
        result = check_watch_circle_vs_cable(20.0, cable())
    assert repeated == []
    assert result.citations == []


@pytest.mark.parametrize("layout", ["", "knowledge"])
def test_removed_page_fails_after_success(tmp_path: Path, layout: str) -> None:
    """Revalidate the sidecar rather than caching a previous successful lookup."""
    page = tmp_path / layout / WIKI_PATH
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: DNV-RP-0360\npublisher: DNV\nrevision: 2016\n---\n",
        encoding="utf-8",
    )
    assert check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path).citations
    page.unlink()
    with pytest.raises(CitationResolutionError) as caught:
        check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path)
    assert caught.value.reason == "page_missing"


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


@pytest.mark.parametrize("layout", ["", "knowledge"])
@pytest.mark.parametrize("failure", ["permission", "encoding"])
def test_unreadable_page_fails_with_citation_identity(
    tmp_path: Path, monkeypatch, layout: str, failure: str
) -> None:
    """Apply calc-output-citation.md section 4.1 to read failures.

    Independent oracle: a page that cannot be read cannot establish provenance;
    The contract requires CitationResolutionError carrying the code identifier.
    Geometry is the existing synthetic cable input, with no numerical assertion.
    The invalid byte is a text-decoding fault, not a standards-derived value.
    """
    page = tmp_path / layout / WIKI_PATH
    page.parent.mkdir(parents=True)
    page.write_bytes(b"\xff")
    read_text = Path.read_text

    def denied_read(path, *args, **kwargs):
        if path == page:
            raise PermissionError("synthetic private read diagnostic")
        return read_text(path, *args, **kwargs)

    if failure == "permission":
        monkeypatch.setattr(Path, "read_text", denied_read)
        error_type = "PermissionError"
    else:
        error_type = "UnicodeDecodeError"

    with pytest.raises(CitationResolutionError) as caught:
        check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path)
    assert caught.value.code_id == "DNV-RP-0360"
    assert caught.value.wiki_path == WIKI_PATH
    assert caught.value.reason == f"page_unreadable:{error_type}"
    assert caught.value.__suppress_context__
    assert "synthetic private read diagnostic" not in str(caught.value)
    assert str(page) not in str(caught.value)


@pytest.mark.parametrize("layout", ["", "knowledge"])
@pytest.mark.parametrize("failure_at", ["lookup", "stat"])
def test_directory_permission_failure_has_citation_identity(
    tmp_path: Path, monkeypatch, layout: str, failure_at: str
) -> None:
    """Translate directory traversal denial during lookup and final page stat.

    Calculation Output Citation Contract section 4.1 requires the citation
    identity even when the page cannot be reached. Inject the filesystem stat
    error so this regression also works with elevated permissions and on systems
    where directory mode bits do not control traversal.
    """
    page = tmp_path / layout / WIKI_PATH
    page.parent.mkdir(parents=True)
    page.write_text(
        "---\ncode_id: DNV-RP-0360\npublisher: DNV\nrevision: 2016\n---\n",
        encoding="utf-8",
    )
    original_stat = Path.stat
    page_stats = 0

    def denied_stat(path, *args, **kwargs):
        nonlocal page_stats
        if path == page:
            page_stats += 1
            if failure_at == "lookup" or page_stats == 2:
                raise PermissionError(
                    errno.EACCES, "synthetic directory traversal denial", str(page)
                )
        return original_stat(path, *args, **kwargs)

    monkeypatch.setattr(Path, "stat", denied_stat)
    with pytest.raises(CitationResolutionError) as caught:
        check_watch_circle_vs_cable(20.0, cable(), repo_root=tmp_path)
    assert page_stats == (1 if failure_at == "lookup" else 2)
    assert caught.value.code_id == "DNV-RP-0360"
    assert caught.value.wiki_path == WIKI_PATH
    assert caught.value.reason == "page_unreadable:PermissionError"
    assert caught.value.__suppress_context__
    displayed = "".join(traceback.format_exception(caught.value))
    assert "synthetic directory traversal denial" not in displayed
    assert str(page) not in displayed
