# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Exercise short public references through the existing section retriever."""

import importlib.util
import re
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
REFERENCE = ROOT / "docs/domains/orcaflex/reference"
SOURCE = "https://www.orcina.com/webhelp/OrcaFlex/Content/html/"
TOPICS = (
    ("environment", "Environment.htm", "Environment", "Environment components"),
    ("environment-sea-data", "Environment,Seadata.htm", "Environment: Sea data",
     "Sea properties"),
    ("environment-current-data", "Environment,Currentdata.htm",
     "Environment: Current data", "Current models"),
)


@pytest.fixture
def retriever():
    spec = importlib.util.spec_from_file_location(
        "environment_retriever", ROOT / "scripts/retrieve_orcina_help.py",
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def read_topics(directory, retriever):
    """Require distinct attributed topics with substantive named sections."""
    topics = []
    for slug, page, title, heading in TOPICS:
        path = directory / f"{slug}.md"
        text = path.read_text(encoding="utf-8")
        assert text.startswith(f"# {title}\n"), slug
        assert f"> Source: Orcina WebHelp ({SOURCE}{page})" in text, slug
        assert len(text.splitlines()) <= 30, slug
        assert len(text.split()) <= 150, slug
        assert "moving edition; numbered release not established" in text, slug
        assert re.search(r"Retrieved: \d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z", text), slug
        assert f"Final URL: {SOURCE}{page}" in text, slug
        assert "HTTP: 200" in text, slug
        assert re.search(r"Bytes: [1-9][0-9]*\b", text), slug
        assert re.search(r"SHA-256: [a-f0-9]{64}\b", text), slug
        topic = {"path": f"docs/domains/orcaflex/reference/{slug}.md",
                 "product": "orcaflex", "text": text}
        sections = retriever.chunk_sections(topic)
        assert any(s["section"] == heading and s["text"].strip() for s in sections), slug
        assert all(s["source_url"] == SOURCE + page for s in sections), slug
        assert all(s["attribution"] == "Orcina WebHelp" for s in sections), slug
        topics.append(topic)
    assert len({t["path"] for t in topics}) == 3
    return topics


def test_three_short_attributed_topics(retriever):
    assert len(read_topics(REFERENCE, retriever)) == 3


@pytest.mark.parametrize("slug,page,title,query", TOPICS)
def test_queries_return_topic_pointers(retriever, slug, page, title, query):
    topics = read_topics(REFERENCE, retriever)
    matches = retriever.search(topics, query, limit=1)
    assert matches and matches[0]["path"].endswith(f"/{slug}.md")
    assert matches[0]["topic"] == title
    assert matches[0]["source_url"] == SOURCE + page
    assert matches[0]["attribution"] == "Orcina WebHelp"
    assert "text" not in matches[0]
    assert retriever.search(topics, query, limit=1) == matches
    lines = (REFERENCE / f"{slug}.md").read_text(encoding="utf-8").splitlines()
    assert lines[matches[0]["line"] - 1] == f"## {query}"


def test_index_advertises_only_available_topics():
    index = (REFERENCE / "INDEX.md").read_text(encoding="utf-8")
    available, unavailable = index.split("## Not Yet Available", 1)
    for slug, _, _, _ in TOPICS:
        assert available.count(f"]({slug}.md)") == 1
        assert f"(`{slug}`)" not in unavailable
    for target in re.findall(r"\]\(([^)]+)\)", available):
        assert (REFERENCE / target).is_file(), target
    assert "(`environment-wave-data`)" in unavailable


@pytest.mark.parametrize("mutation", ["attribution", "swapped-urls", "missing-topic"])
def test_reference_validation_detects_mutations(tmp_path, retriever, mutation):
    """Corrupt temporary copies only; repository references remain untouched."""
    for slug, page, _, _ in TOPICS:
        if mutation != "missing-topic" or slug != "environment-current-data":
            text = (REFERENCE / f"{slug}.md").read_text(encoding="utf-8")
            if mutation == "attribution" and slug == "environment":
                text = text.replace("> Source: Orcina WebHelp", "> Source: omitted")
            if mutation == "swapped-urls":
                other = {TOPICS[0][1]: TOPICS[1][1], TOPICS[1][1]: TOPICS[0][1]}
                text = text.replace(SOURCE + page, SOURCE + other.get(page, page))
            (tmp_path / f"{slug}.md").write_text(text, encoding="utf-8")
    expected = FileNotFoundError if mutation == "missing-topic" else AssertionError
    with pytest.raises(expected):
        read_topics(tmp_path, retriever)
