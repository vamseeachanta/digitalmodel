# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Generate docs/api/ffs/ffs-readiness.html without engineering calculations."""

import argparse
from html import escape
from pathlib import Path
from posixpath import relpath
from urllib.parse import quote

import yaml

from digitalmodel.asset_integrity import offering_catalog as oc

REPO = Path(__file__).resolve().parents[2]
PAGE = "docs/api/ffs/ffs-readiness.html"
REVIEW = "docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md"
REGISTRY = "docs/registry/workflows.yaml"
# Explicit identities prevent a renamed historical row silently losing its score.
HISTORICAL = dict(zip((
    "Metal-loss coordinator (L1/L2, GML/LML, decision, report)",
    "Measurement sufficiency", "Level 3 escalation handoff", "Legacy API 579 GML/LML",
    "Corroded pipe B31G / Mod B31G / RSTRENG", "RSTRENG 2D river-bottom",
    "DNV-RP-F101 single / PSF / interacting", "Circumferential defect", "Pitting",
    "Dents / dent-gouge", "Crack-like flaw Level 2 FAD", "Legacy BS 7910 flaw limits",
    "Crack growth (Paris)", "Riser-joint FFS on real C-scans", "RBI screening",
    "Composite repair selector", "Inspection planning", "Acceptance curves + lookup",
    "RSF calculations",
), (
    "ffs-metal-loss", "measurement-sufficiency", "level3-handoff", "api579-legacy",
    "corroded-pipe", "rstreng-2d", "dnv-f101", "circumferential", "pitting", "dents",
    "crack-fad", "legacy-bs7910", "crack-growth", "riser-joint-ffs", "rbi",
    "composite-repair", "inspection-planning", "acceptance-curves-lookup", "rsf-calculations",
), strict=True))


def historical_rows(review):
    """Read only the historical matrix; reject unknown identities and scores."""
    matrix = review.split("## Matrix\n", 1)[1].strip().split("\n\n", 1)[0]
    rows = {}
    for line in matrix.splitlines()[2:]:
        cells = [cell.strip() for cell in line.strip("|").split("|")]
        label, score = cells[0], cells[-1]
        if label not in HISTORICAL or score not in {"R0 (#2160)", "R1", "R2", "R3", "R3-", "R4*"}:
            raise ValueError("Unknown historical evidence label or score")
        key = HISTORICAL[label]
        if key in rows:
            raise ValueError("Duplicate historical evidence")
        rows[key] = (label, score)
    if set(rows) != set(HISTORICAL.values()):
        raise ValueError("Incomplete historical evidence")
    return rows


def evidence_targets(cat, registry):
    """Collect only explicit validation records and registered example inputs."""
    workflows = {row["id"]: row["input"] for row in registry["workflows"]}
    return {REVIEW} | {
        path for engine in cat.ffs["engines"].values()
        for path in (engine.get("validation"), workflows.get(engine.get("workflow")))
        if path
    }


def available_evidence(repo, cat, registry):
    """Record missing evidence as a gap without importing assessment engines."""
    return {path for path in evidence_targets(cat, registry) if (repo / path).is_file()}


def render(cat, review, registry, available):
    """Return deterministic HTML from selected public fields and evidence presence."""
    historical = historical_rows(review)
    workflows = {row["id"]: row["input"] for row in registry["workflows"]}

    def link(path, label):
        return f'<a href="{escape(quote(relpath(path, "docs/api/ffs")), quote=True)}">{escape(label)}</a>'

    lines = [
        '<!doctype html><html lang="en"><head><meta charset="utf-8">',
        '<meta name="viewport" content="width=device-width, initial-scale=1">',
        '<title>FFS readiness evidence</title>',
        '<style>body{font:1rem system-ui;margin:2rem;line-height:1.5}table{border-collapse:collapse}',
        'th,td{border:1px solid #aaa;padding:.5rem;text-align:left;vertical-align:top}</style></head><body>',
        '<!-- GENERATED: python scripts/ffs/build_ffs_readiness.py --output docs/api/ffs/ffs-readiness.html -->',
        '<h1>FFS readiness evidence</h1>',
        '<p>Historical scores describe the 2026-09-25 review, amended 2026-09-27. They do not establish current qualification.',
        'Current catalog status is shown separately; unscored engines are Not Evaluated.</p>',
        '<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>',
        '<tr><td>1</td><td>2026-11-16</td><td>Publish dated scores with current evidence links and explicit gaps.</td></tr></table>',
        '<p>R0: engine only or ineffective tests; R1: engine and tests; R2: validation record added;',
        'R3: workflow added; R4: committed report and public page added. Scores retain the review qualifiers:',
        'R4* applies only to the legacy path; R3- is retained as recorded for inspection planning.</p>',
        '<p>Evidence links identify catalog records and registered example inputs; their presence does not independently validate a method.',
        'Campaign composition, published RBI and repair examples, and current requalification remain open.</p>',
        link(REVIEW, 'Historical review (not a validation record)'),
        '<table><caption>FFS catalog engines and historical-only capabilities</caption>',
        '<thead><tr><th>Engine / capability</th><th>Historical score (2026-09-25)</th>',
        '<th>Current catalog status</th><th>Evidence</th><th>Limits</th></tr></thead><tbody>',
    ]
    for key in sorted(set(cat.ffs["engines"]) | set(historical)):
        engine = cat.ffs["engines"].get(key, {})
        label, score = historical.get(key, (key, "Not Evaluated"))
        evidence = []
        for field, target in (("workflow", workflows.get(engine.get("workflow"))),
                              ("validation", engine.get("validation"))):
            evidence.append(link(target, f'{field}: {engine[field]}') if target in available else
                            f'Evidence gap: {field} unavailable')
        evidence.append(link(REVIEW, "Historical review (not validation)"))
        note = engine.get("note", "")
        if key == "riser-joint-ffs":
            note = "anonymized C-scan composition record; independent multi-asset qualification not established"
        limits = "; ".join(filter(None, (engine.get("caveat"), note)))
        lines.append(f'<tr data-engine="{escape(key, quote=True)}"><th scope="row">{escape(key)}: {escape(label)}</th>'
                     f'<td>{escape(score)}</td><td>{escape(engine.get("status", "Not in FFS catalog"))}</td>'
                     f'<td>{"<br>".join(evidence)}</td><td>{escape(limits)}</td></tr>')
    return "\n".join([*lines, '</tbody></table></body></html>', ''])


def main():
    """Write one UTF-8 page to an explicit destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    cat = oc.load()
    registry = yaml.safe_load((REPO / REGISTRY).read_text(encoding="utf-8"))
    text = render(cat, (REPO / REVIEW).read_text(encoding="utf-8"), registry,
                  available_evidence(REPO, cat, registry))
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(text, encoding="utf-8", newline="\n")


if __name__ == "__main__":
    main()
