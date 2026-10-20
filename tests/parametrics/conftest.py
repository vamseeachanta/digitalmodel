"""Synthetic citation metadata for offline geometry regressions."""
import pytest


@pytest.fixture
def citation_tree(tmp_path, monkeypatch):
    page = tmp_path / "wikis/engineering-standards/wiki/standards/dnv-rp-0360.md"
    page.parent.mkdir(parents=True)
    page.write_text("---\ncode_id: DNV-RP-0360\npublisher: DNV\nrevision: 2016\n---\n",
                    encoding="utf-8")
    monkeypatch.setenv("LLM_WIKI_PATH", str(tmp_path))
