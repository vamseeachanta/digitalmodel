# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent section examples for CommonMark 0.31.2 (2024), §§4.2 and 4.5.

Fixtures are synthetic, with expected headings and line numbers counted directly
from the literal inputs. No solver documentation or solver interface is needed.
Source: https://spec.commonmark.org/0.31.2/
"""

import importlib.util
from pathlib import Path

import pytest


@pytest.fixture
def retriever():
    """Load the standalone retrieval script without importing a solver package."""
    script = Path(__file__).resolve().parents[2] / "scripts/retrieve_orcina_help.py"
    spec = importlib.util.spec_from_file_location("section_retriever", script)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def topic(body, product="orcaflex"):
    """Put section input after a title and an attribution line."""
    return {
        "path": f"help/{product}/inputs.md",
        "product": product,
        "text": (
            "# Synthetic inputs\n"
            "> Source: Orcina WebHelp (https://www.orcina.com/webhelp/)\n"
            f"{body}"
        ),
    }


@pytest.mark.parametrize("opening, false_close, closing", [
    ("```yaml", "```still code", "```"),
    ("~~~yaml", "~~~still code", "~~~"),
    ("````yaml", "```", "````"),
    ("```yaml", "~~~", "```"),
    ("~~~yaml", "    ~~~", "~~~"),
    ("~~~yaml", "~~~\tstill code", "~~~"),
])
def test_fence_like_content_does_not_create_searchable_sections(
    retriever, opening, false_close, closing,
):
    """§4.5: only a matching, sufficiently long bare fence closes code.

    Counting the two-line preamble, Inputs starts at line 3 and Results at
    line 9. The apparent heading at line 7 remains inside Inputs in each case.
    """
    sections = retriever.chunk_sections(topic(
        f"## Inputs\n{opening}\nvalue\n{false_close}\n"
        f"## Code comment\n{closing}\n## Results\nExport.\n"
    ))
    assert [(s["section"], s["line"]) for s in sections] == [
        ("Inputs", 3), ("Results", 9),
    ]
    assert "## Code comment" in sections[0]["text"]


@pytest.mark.parametrize("opening, closing", [
    ("```yaml", "   ````\t "),
    ("~~~yaml", "  ~~~~~ "),
])
def test_longer_indented_fence_with_trailing_whitespace_closes(
    retriever, opening, closing,
):
    """§4.5 permits longer closers, up to three spaces and trailing whitespace.

    The literal Results heading is on line 7, after a valid closing fence.
    """
    sections = retriever.chunk_sections(topic(
        f"## Inputs\n{opening}\nvalue\n{closing}\n## Results\nExport.\n"
    ))
    assert [(s["section"], s["line"]) for s in sections] == [
        ("Inputs", 3), ("Results", 7),
    ]


def test_unclosed_fence_keeps_heading_like_text_in_its_section(retriever):
    """§4.5: an unclosed fence extends to the end of the document."""
    sections = retriever.chunk_sections(topic(
        "## Inputs\n```yaml\n## Code comment\nvalue\n"
    ))
    assert [s["section"] for s in sections] == ["Inputs"]
    assert "## Code comment" in sections[0]["text"]


@pytest.mark.parametrize("heading, expected", [
    ("   ## Inputs", "Inputs"),
    ("##\tInputs", "Inputs"),
    ("## Inputs ### \t", "Inputs"),
    ("## Inputs#", "Inputs#"),
    ("##", ""),
    ("###### Inputs", "Inputs"),
])
def test_atx_heading_boundaries_and_literal_hashes(retriever, heading, expected):
    """§4.2: heading indentation, separators and closing hashes are syntactic.

    Each fixture has one section starting at line 3; attached hashes belong
    to its name, and an empty heading is still a section boundary.
    """
    sections = retriever.chunk_sections(topic(f"{heading}\nInput body.\n"))
    assert [(s["section"], s["line"]) for s in sections] == [(expected, 3)]


@pytest.mark.parametrize("line", [
    "    ## Indented code", "####### Too many hashes", "##Missing separator",
    "```bad`info",  # §4.5: backtick-fence info cannot contain backticks.
])
def test_non_boundaries_leave_following_heading_visible(retriever, line):
    """§§4.2, 4.5: invalid heading/fence syntax stays ordinary content.

    Inputs begins on line 3; Results begins on line 6 after that content.
    """
    sections = retriever.chunk_sections(topic(
        f"## Inputs\nBody.\n{line}\n## Results\nExport.\n"
    ))
    assert [(s["section"], s["line"]) for s in sections] == [
        ("Inputs", 3), ("Results", 6),
    ]


@pytest.mark.parametrize("query, expected_section", [
    ("yaml", "YAML inputs"),
    ("configuration", "Model configuration"),
    ("export", "Results export"),
])
def test_cross_product_queries_preserve_named_sections_and_citations(
    retriever, query, expected_section,
):
    """Each query occurs in exactly one real heading per synthetic product.

    The three matching headings are at lines 3, 9 and 11 respectively. A
    code comment contains all query terms but cannot create another section.
    Expected products and sections follow directly from these literal inputs.
    """
    topics = [topic(
        "## YAML inputs\n```yaml\n```still code\n"
        "## yaml configuration export\nvalue\n```\n"
        "## Model configuration\nSettings.\n"
        "## Results export\nOutput.\n", product,
    ) for product in ("orcaflex", "orcawave", "orcfxapi")]
    matches = retriever.search(topics, query)
    expected_line = {"yaml": 3, "configuration": 9, "export": 11}[query]
    selected = [m for m in matches if m["section"] == expected_section]
    assert {m["product"] for m in selected} == {"orcaflex", "orcawave", "orcfxapi"}
    assert all(m["section"] != "yaml configuration export" for m in matches)
    assert all(m["topic"] == "Synthetic inputs" for m in matches)
    assert all(m["line"] == expected_line for m in selected)
    assert all(m["path"] == f"help/{m['product']}/inputs.md" for m in matches)
    assert all(m["source_url"] == "https://www.orcina.com/webhelp/" for m in matches)
    assert all(m["attribution"] == "Orcina WebHelp" for m in matches)
    assert all("text" not in m for m in matches)
