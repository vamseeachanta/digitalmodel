# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Disposable source fixture for repeated table comparisons, also under -O.

The independent hand calculation is two lengths of three metres:
3 m + 3 m = 6 m. These are synthetic inputs, not engineering coefficients.
Only one frontmatter identity field changes between comparisons; the values,
units and distinct but equal in-memory citations remain unchanged.
Explicit exceptions keep the probe effective when assertions are optimized out.
"""

from dataclasses import replace
from pathlib import Path
import sys

from digitalmodel.citations import Citation, CitedValue, CitationResolutionError
from digitalmodel.citations.table_verification import verify_table_value


def check_stale_source(root: Path, field_name: str) -> None:
    """Require fresh source resolution after a previously valid comparison."""
    if field_name not in ("code_id", "publisher", "revision"):
        raise ValueError("probe requires a frontmatter identity field")
    citation = Citation(
        code_id="synthetic-length",
        publisher="synthetic",
        revision="example",
        section="Example length",
        wiki_path="wikis/synthetic/length.md",
        note="two segments of three metres",
    )
    expected = CitedValue(value=6, citation=citation, units="m")
    actual = CitedValue(value=3 + 3, citation=replace(citation), units="m")
    page = root / citation.wiki_path
    page.parent.mkdir(parents=True, exist_ok=True)
    original = (
        "---\ncode_id: synthetic-length\npublisher: synthetic\n"
        "revision: example\n---\nSynthetic arithmetic only.\n"
    )
    page.write_text(original, encoding="utf-8")

    # This must succeed before the source changes, using the real resolver.
    verify_table_value(actual, expected, repo_root=root)
    old_identity = f"{field_name}: {getattr(citation, field_name)}\n"
    new_identity = f"{field_name}: changed-{getattr(citation, field_name)}\n"
    page.write_text(original.replace(old_identity, new_identity), encoding="utf-8")

    # Neither object is reconstructed or edited after its successful comparison.
    try:
        verify_table_value(actual, expected, repo_root=root)
    except CitationResolutionError as error:
        if not error.reason.startswith(f"frontmatter_mismatch:{field_name}:"):
            raise RuntimeError("source rejected for an unexpected reason") from error
    else:
        raise RuntimeError("matching numbers accepted a stale source identity")


if __name__ == "__main__":
    check_stale_source(Path(sys.argv[1]), sys.argv[2])
