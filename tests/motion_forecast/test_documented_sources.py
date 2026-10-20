# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Public input claims must agree with the implemented measured sources."""

import ast
import re
from html import unescape
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
MODULE = ROOT / "src/digitalmodel/motion_forecast"
DOCUMENTS = (
    "docs/api/hydro/short-horizon-motion-forecast.html",
    "src/digitalmodel/motion_forecast/docs/agent-pack.md",
    "README.md",
)


def _offline_source_names(source):
    """Discover concrete readers, excluding the structural Protocol interface."""
    readers = []
    for node in ast.parse(source).body:
        if isinstance(node, ast.ClassDef):
            if any(ast.unparse(base).endswith("Protocol") for base in node.bases):
                continue
            if any(isinstance(method, ast.FunctionDef) and method.name == "read"
                   for method in node.body):
                readers.append(node)
        elif isinstance(node, ast.FunctionDef) and node.returns is not None:
            if ast.unparse(node.returns).strip("'\"") == "MeasuredMotion":
                readers.append(node)
    assert readers, "No concrete measured sources discovered; review the claim guard"
    for reader in readers:
        description = ast.get_docstring(reader) or ""
        assert re.search(r"\b(synthetic|stub|csv)\b", description, re.I), (
            f"Review input claims for new source {reader.name}"
        )
    return [reader.name for reader in readers]


def _unsupported_live_claims(document):
    """Inspect visible clauses, preserving limitations without masking claims."""
    text = unescape(re.sub(r"<[^>]+>", " ", document))
    # Ignore link destinations; a site being online does not imply live inputs.
    text = re.sub(r"\[([^\]]+)\]\([^)]*\)", r"\1", text)
    text = re.sub(r"\s+", " ", text).lower()
    clauses = re.split(r"[.!?;]|\b(?:but|however)\b", text)
    claims = []
    for clause in clauses:
        for live in re.finditer(r"\b(?:live|real[- ]time)\b", clause):
            before, after = clause[:live.start()], clause[live.end():]
            context = before[-80:] + " live " + after[:160]
            if not re.search(r"\b(mms|mru|motion|sensor|feed|demo)\b", context):
                continue
            negated = re.search(r"\b(?:no|without|not(?: a)?)\s+$", before)
            planned = re.match(
                r"\s+(?:mms|mru|sensor|feed|input)[\w\s/&-]{0,60}?\s+"
                r"(?:is|are|remains?)\s+(?:planned|not (?:implemented|supported|available))\b",
                after,
            )
            if not (negated or planned):
                claims.append(clause.strip())
    return claims


@pytest.mark.parametrize("relative", DOCUMENTS)
def test_public_input_claims_match_implemented_sources(relative):
    sources = _offline_source_names((MODULE / "measured_source.py").read_text())
    claims = _unsupported_live_claims((ROOT / relative).read_text(encoding="utf-8"))
    assert not claims, f"{relative}: only {sources} are implemented; claims: {claims}"


@pytest.mark.parametrize("relative", DOCUMENTS)
@pytest.mark.parametrize("claim", (
    'Anchored to live <strong>vessel &amp; structure</strong> motions from an MMS.',
    '<span class="pill">Live MMS / MRU feed &middot; phase-resolved forecast</span>',
    'Measured — live vessel &amp; structure motions from an onboard MMS / MRU.',
    'This demo uses LIVE motion data.',
    'The demo uses real-time MMS / MRU feed data.',
    'This is a live demo.',
    'Live MMS ingest is planned, but this demo uses a live MRU feed.',
    'No live MMS feed is implemented; this demo uses live sensor data.',
))
def test_negative_control_rejects_injected_claim_on_each_surface(relative, claim):
    document = (ROOT / relative).read_text(encoding="utf-8")
    assert _unsupported_live_claims(claim)
    assert _unsupported_live_claims(document + "\n\n" + claim)


@pytest.mark.parametrize("description", (
    'Illustrative: synthetic sea and simplified analytic RAOs.',
    'Measured-motion input uses a synthetic stub or a CSV reader.',
    'Live MMS / MRU ingest is planned.',
    'Live MRU feeds are not supported.',
    'A live feed is not implemented.',
    'Real-time MMS / MRU input is planned.',
    'No live MMS feed is implemented.',
    'Live: [engineering capabilities](https://example.org/capabilities/)',
))
def test_accurate_limitations_and_site_links_are_permitted(description):
    assert not _unsupported_live_claims(description)


def test_source_inventory_requires_review_when_a_reader_is_added():
    source = (MODULE / "measured_source.py").read_text()
    unsupported = '\nclass NetworkReader:\n    def read(self):\n        pass\n'
    with pytest.raises(AssertionError, match="new source NetworkReader"):
        _offline_source_names(source + unsupported)


def test_source_inventory_cannot_pass_when_no_reader_is_discovered():
    with pytest.raises(AssertionError, match="No concrete measured sources"):
        _offline_source_names("class Interface: pass")
