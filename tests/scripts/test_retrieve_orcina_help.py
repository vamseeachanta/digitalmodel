# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Synthetic help sections exercise retrieval without any solver interface."""

import base64
import importlib.util
import json
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/retrieve_orcina_help.py"


def retriever():
    spec = importlib.util.spec_from_file_location("help_retriever", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def topic(product, name, text):
    return {
        "path": f"help/{product}/{name}.md", "product": product,
        "text": f"# Synthetic {name}\n\n> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)\n\n{text}",
    }


def test_section_chunking_respects_fenced_headings_and_source():
    r = retriever()
    sections = r.chunk_sections(topic("orcaflex", "yaml", (
        "## YAML inputs\nUse a text input.\n```yaml\n# not a section\n```\n"
        "## Statics\nSynthetic convergence text.\n"
    )))
    assert [s["section"] for s in sections] == ["YAML inputs", "Statics"]
    assert "# not a section" in sections[0]["text"]
    assert all(s["source_url"] == "https://www.orcina.com/webhelp/" for s in sections)


@pytest.mark.parametrize("query, expected", [
    ("YAML inputs", {"orcaflex", "orcawave", "orcfxapi"}),
    ("model configuration", {"orcaflex", "orcawave", "orcfxapi"}),
    ("results export", {"orcaflex", "orcawave", "orcfxapi"}),
])
def test_three_cross_product_queries_emit_pointers_only(query, expected):
    r = retriever()
    topics = [topic(p, "synthetic", (
        "## YAML inputs\nSynthetic YAML inputs.\n"
        "## Model configuration\nSynthetic model configuration.\n"
        "## Results export\nSynthetic results export.\n"
    )) for p in ("orcaflex", "orcawave", "orcfxapi")]
    matches = r.search(topics, query, limit=3)
    assert {m["product"] for m in matches} == expected
    assert all("text" not in m for m in matches)
    assert all(m["source_url"] and m["attribution"] == "Orcina WebHelp" for m in matches)
    assert r.search(topics, query, limit=3) == matches


def test_missing_attribution_is_rejected():
    with pytest.raises(ValueError, match="source"):
        retriever().chunk_sections({"path": "help/orcaflex/bad.md", "product": "orcaflex",
                                   "text": "# Synthetic\n## Input\nExample\n"})


def test_github_retrieval_is_read_only_pinned_and_product_filtered():
    r = retriever()
    calls = []
    text = topic("orcaflex", "input", "## Input\nSynthetic input.\n")["text"]
    responses = [
        {"sha": "a" * 40},
        {"tree": [{"path": "help", "type": "tree", "sha": "d" * 40}]},
        {"truncated": False, "tree": [
            {"path": "orcaflex/input.md", "type": "blob", "sha": "b" * 40},
            {"path": "other/unrelated.md", "type": "blob", "sha": "c" * 40},
        ]},
        {"encoding": "base64", "content": base64.b64encode(text.encode()).decode()},
    ]
    def api(endpoint):
        calls.append(endpoint)
        return responses.pop(0)
    topics, revision = r.load_github_topics("example/wiki", "main", "help", api=api)
    assert revision == "a" * 40
    assert len(topics) == 1
    assert calls[1].endswith("a" * 40)
    assert calls[2].endswith("d" * 40 + "?recursive=1")
    assert calls[3].endswith("b" * 40)
    assert all(not method in json.dumps(calls) for method in ("POST", "PATCH", "DELETE"))


def test_truncated_private_tree_fails_instead_of_silent_partial_results():
    r = retriever()
    def api(endpoint):
        return {"sha": "a" * 40, "truncated": True, "tree": []}
    with pytest.raises(ValueError, match="truncated"):
        r.load_github_topics("example/wiki", "main", "help", api=api)


@pytest.mark.parametrize("prefix", ["../help", "/help", "help/../private"])
def test_unsafe_corpus_prefix_is_rejected(prefix):
    with pytest.raises(ValueError, match="prefix"):
        retriever().load_github_topics("example/wiki", "main", prefix, api=lambda _: {})


def test_private_corpus_tree_is_resolved_before_recursive_listing():
    r = retriever()
    calls = []
    responses = [
        {"sha": "a" * 40},
        {"tree": [{"path": "help", "type": "tree", "sha": "d" * 40}]},
        {"truncated": False, "tree": [
            {"path": "orcaflex/input.md", "type": "blob", "sha": "b" * 40},
        ]},
        {"encoding": "base64", "content": base64.b64encode(
            topic("orcaflex", "input", "## Input\nSynthetic input.\n")["text"].encode()
        ).decode()},
    ]
    def api(endpoint):
        calls.append(endpoint)
        return responses.pop(0)
    topics, _ = r.load_github_topics("example/wiki", "main", "help", api=api)
    assert calls[1].endswith("a" * 40)
    assert calls[2].endswith("d" * 40 + "?recursive=1")
    assert topics[0]["path"] == "help/orcaflex/input.md"
