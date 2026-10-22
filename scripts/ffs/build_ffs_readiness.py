# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Publish the historical readiness matrix with current campaign evidence."""
from html import escape
from pathlib import Path


def render_readiness(root: Path) -> str:
    """Retain all historical scores; expose qualification gaps without upgrades."""
    review = root / "docs/domains/asset-integrity/ffs-readiness-review-2026-09-25.md"
    lines = review.read_text(encoding="utf-8").split("## Matrix\n", 1)[1].split("**Fixtures.", 1)[0]
    rows, gaps = [], []
    campaign = {"Measurement sufficiency", "RBI screening", "Composite repair selector",
                "Inspection planning"}
    for line in lines.splitlines():
        if not line.startswith("| ") or line.startswith("| Capability"):
            continue
        cells = [cell.strip() for cell in line.strip("|").split("|")]
        name, module, standard, _, workflow, _, validation, _, score = cells
        if name in campaign:
            evidence = ('<a href="../../../examples/workflows/ffs-campaign-demo/input.yml">'
                        'Run campaign</a>; <a href="../../domains/asset-integrity/'
                        'campaign-screening-validation.html">screening record</a>; '
                        'published-case qualification NOT_EVALUATED')
        else:
            evidence = ('<a href="../../domains/asset-integrity/'
                        'ffs-readiness-review-2026-09-25.md">Review evidence and gaps</a>')
            if workflow.startswith("`"):
                workflow_id = workflow.split("`")[1]
                fixture = root / "examples/workflows" / workflow_id / "input.yml"
                if fixture.exists():
                    evidence += (f'; <a href="../../../examples/workflows/'
                                 f'{escape(workflow_id)}/input.yml">Run input</a>')
        if name not in campaign:
            anchor = f"validation-gap-{len(rows)}"
            evidence += f'; <a href="#{anchor}">Validation evidence status</a>'
            gaps.append(f'<h3 id="{anchor}">{escape(name)}</h3>'
                        f'<p>Historical validation evidence: {escape(validation)}. '
                        'Edition-matched published-case validation has not been re-established '
                        'by this campaign: NOT_EVALUATED. The source review records the '
                        'prior evidence; no new qualification claim is made.</p>')
        values = [name, module, standard, score, validation]
        rows.append('<tr data-engine="' + escape(name) + '">' + ''.join(
            f'<td>{escape(value)}</td>' for value in values) + f'<td>{evidence}</td></tr>')
    return '''<!doctype html>
<html lang="en"><meta charset="utf-8"><title>FFS readiness analysis</title>
<body><h1>FFS readiness analysis</h1>
<p>Summary: the complete historical engine matrix is reproduced below. Scores describe
observed evidence at the review date, not current code compliance or release approval.</p>
<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>
<tr><td>0</td><td>2026-10-20</td><td>Historical matrix published with campaign evidence and explicit qualification gaps.</td></tr></table>
<p>Table 1. Revision history.</p>
<p>R4: engine, tests, validation, registered workflow, example, report and page.
R3: workflow and validation without a committed report or page.
R2: engine, tests and validation. R1: engine and tests. R0: engine without effective tests.
Historical scores are retained without upgrades: a hand calculation does not establish published-case validation.</p>
<table><thead><tr><th>Capability</th><th>Module</th><th>Standard basis</th>
<th>Score at 2026-09-25</th><th>Historical validation</th><th>Evidence</th></tr></thead><tbody>
''' + '\n'.join(rows) + '''
</tbody></table><p>Table 2. All engines in the historical readiness review; scores are historical.</p>
<h2>Validation evidence status</h2>
''' + '\n'.join(gaps) + '''
<p>The campaign composes sufficiency, qualitative risk rank, repair candidates and
interval screening. API RP 580 published-case validation and ISO 24817 / ASME PCC-2
category qualification are NOT_EVALUATED. Internal A-E repair labels are not standard categories.</p>
<p>Way forward: obtain edition-matched published cases, qualify the policy and repair
system, then reassess readiness against the stated evidence criteria.</p>
<p><a href="../../domains/asset-integrity/ffs-readiness-review-2026-09-25.md">Source review</a>;
<a href="../../capability-map/capabilities-added.yml">Per-module capability map</a>;
<a href="../../../examples/workflows/ffs-campaign-demo/report.html">Worked campaign report</a>.</p>
</body></html>
'''


if __name__ == "__main__":
    root = Path(__file__).resolve().parents[2]
    (root / "docs/api/ffs/ffs-readiness.html").write_text(render_readiness(root), encoding="utf-8")
