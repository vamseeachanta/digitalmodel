# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Historical readiness stays distinct from current catalog evidence."""

from collections import Counter
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import unquote, urlsplit

import pytest
import yaml

from digitalmodel.asset_integrity import offering_catalog as oc

ROOT = Path(__file__).resolve().parents[2]
REVIEW = "docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md"


class Page(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.engines, self.links = [], []
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == "tr" and "data-engine" in attrs:
            self.engines.append(attrs["data-engine"])
        if tag == "a":
            self.links.append(attrs["href"])


@pytest.fixture
def inputs():
    from scripts.ffs import build_ffs_readiness as builder
    cat = oc.load()
    review = (ROOT / REVIEW).read_text(encoding="utf-8")
    registry = yaml.safe_load((ROOT / "docs/registry/workflows.yaml").read_text())
    return builder, cat, review, registry, builder.available_evidence(ROOT, cat, registry)


def test_complete_coverage_and_historical_traceability(inputs):
    builder, cat, review, registry, available = inputs
    text = builder.render(cat, review, registry, available)
    counts = Counter(Page(text).engines)
    assert all(counts[key] == 1 for key in cat.ffs["engines"])
    assert set(counts) - set(cat.ffs["engines"]) == {
        "legacy-bs7910", "acceptance-curves-lookup", "rsf-calculations"}
    assert {key: value[1] for key, value in builder.historical_rows(review).items()} == {
        "ffs-metal-loss": "R3", "measurement-sufficiency": "R3", "level3-handoff": "R1",
        "api579-legacy": "R4*", "corroded-pipe": "R2", "rstreng-2d": "R2", "dnv-f101": "R2",
        "circumferential": "R2", "pitting": "R1", "dents": "R1", "crack-fad": "R1",
        "legacy-bs7910": "R0 (#2160)", "crack-growth": "R1", "riser-joint-ffs": "R1",
        "rbi": "R1", "composite-repair": "R1", "inspection-planning": "R3-",
        "acceptance-curves-lookup": "R1", "rsf-calculations": "R1"}
    note = cat.ffs["engines"]["riser-joint-ffs"]["note"]
    assert note.startswith("anonymized C-scan composition record; independent ")
    assert note.endswith(" qualification not established")
    for key, (label, score) in builder.historical_rows(review).items():
        assert label in text and score in text
    assert "Historical score (2026-09-25)" in text
    assert "Current catalog status" in text
    assert "R4*" in text and "R3-" in text and "Not Evaluated" in text
    assert "independent multi-asset qualification not established" in text
    assert "interacting-defect method and PSF tables" in text


def test_normalized_local_links_resolve(inputs):
    builder, cat, review, registry, available = inputs
    for href in Page(builder.render(cat, review, registry, available)).links:
        url = urlsplit(href)
        assert not url.scheme and not url.netloc
        target = (ROOT / "docs/api/ffs" / unquote(url.path)).resolve()
        assert target.is_relative_to(ROOT) and target.is_file(), href


@pytest.mark.parametrize("registered", [False, True])
def test_missing_evidence_is_a_gap_and_escapes_public_fields(inputs, registered):
    builder, cat, review, _, _ = inputs
    cat.ffs["engines"] = {"new<&\"": {"status": "engine", "note": "<script>&\"",
                                           "validation": "missing.md", "workflow": "absent"}}
    registry = {"workflows": [{"id": "absent", "input": "missing.yml"}] if registered else []}
    text = builder.render(cat, review, registry, {REVIEW})
    assert 'new&lt;&amp;&quot;' in text and '&lt;script&gt;&amp;&quot;' in text
    assert "<script>" not in text and "missing.md" not in text
    assert "Evidence gap: workflow unavailable" in text
    assert "Evidence gap: validation unavailable" in text
    assert "Not Evaluated" in text
    assert all("review-2026-09-25.md" in link for link in Page(text).links)


@pytest.mark.parametrize("fault", ["label", "score", "missing", "duplicate"])
def test_unknown_or_incomplete_historical_evidence_fails(inputs, fault):
    builder, cat, review, registry, available = inputs
    if fault == "label":
        review = review.replace("| RBI screening |", "| Unknown evidence |")
    elif fault == "score":
        review = review.replace("| R1 |", "| R9 |")
    elif fault == "duplicate":
        row = next(line for line in review.splitlines() if line.startswith("| RBI screening |"))
        review = review.replace(row, row + "\n" + row)
    else:
        review = "\n".join(line for line in review.splitlines() if not line.startswith("| RBI screening |"))
    with pytest.raises(ValueError):
        builder.render(cat, review, registry, available)


def test_deterministic_utf8_and_committed_page(inputs):
    builder, cat, review, registry, available = inputs
    first = builder.render(cat, review, registry, available).encode("utf-8")
    cat.ffs["engines"] = dict(reversed(list(cat.ffs["engines"].items())))
    assert first == builder.render(cat, review, registry, available).encode("utf-8")
    assert first == (ROOT / "docs/api/ffs/ffs-readiness.html").read_bytes()
