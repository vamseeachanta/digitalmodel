# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build a separate, reproducible fatigue-factor arithmetic example.

Uses the production getter, workflow resolver and damage-verdict function.
Only the named report and its change sidecar are written to --output-dir.
No solver or private reference is accessed. Every input is synthetic.
"""
from __future__ import annotations

import argparse
import hashlib
import html
import json
from pathlib import Path

from digitalmodel.riser_database.getters import get_riser_dff
from digitalmodel.riser_fatigue.workflow import fatigue_damage_verdict, resolve_dff

ROOT = Path(__file__).resolve().parents[2]
DESTINATION = ROOT / "docs/reports/modules/riser_database_dff"
FIXTURE = ROOT / "tests/citations/fixtures"
CHANGES = {
    "document": "Riser fatigue factor analysis",
    "revision": "1",
    "date": "2026-10-22",
    "changes": [
        "Require an explicit safety class and supplied class factor.",
        "Compare synthetic damage with the supplied factor using production arithmetic.",
        "Separate arithmetic results from unevaluated standards adequacy.",
    ],
}


def example() -> dict:
    """Synthetic normal-class F=4, D=.2: hand allowable=.25 and usage=.8.

    Source of arithmetic: workflow.py::fatigue_damage_verdict and
    touchdown.py, step 5 (usage-factor definition). DNV-OS-F201 (2010)
    class table and locator remain NOT_EVALUATED. No published class value
    is asserted. These assumptions test the software interface only.
    """
    settings = {"safety_class": "normal", "class_dff": 4.0}
    damage = 0.2
    cited = get_riser_dff(**settings, repo_root=FIXTURE)
    factor = resolve_dff(settings, repo_root=FIXTURE)
    return {
        **settings, "damage": damage, "factor": factor,
        **fatigue_damage_verdict(damage, factor),
        "citation_note": cited.citation.note,
        "standard_status": "NOT_EVALUATED",
    }


def build(output_dir: Path = DESTINATION) -> None:
    """Generate the example and change record deterministically from code."""
    result = example()
    status = "PASS" if result["passes_dff"] else "FAIL"
    # Independent hand calculation written in example(): 1/4=.25, .2*4=.8.
    allowable_difference = 100 * (result["allowable_damage"] - 0.25) / 0.25
    usage_difference = 100 * (result["usage_factor"] - 0.8) / 0.8
    basis = [
        "src/digitalmodel/riser_database/getters.py",
        "src/digitalmodel/riser_fatigue/workflow.py",
        "src/digitalmodel/riser_fatigue/touchdown.py",
        "tests/citations/fixtures/knowledge/wikis/engineering-standards/wiki/standards/dnv-os-f201.md",
        "scripts/reports/build_riser_dff_example.py",
    ]
    hashes = "\n".join(
        f"<li><code>{path}</code>: <code>"
        f"{hashlib.sha256((ROOT / path).read_bytes().replace(bytes([13, 10]), bytes([10]))).hexdigest()}"
        "</code></li>" for path in basis
    )
    report = f'''<!doctype html>
<html lang="en"><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Riser fatigue factor analysis</title>
<style>body{{font:16px/1.6 system-ui;max-width:58rem;margin:2rem auto;padding:0 1rem}}
table{{border-collapse:collapse}}td,th{{border:1px solid #aaa;padding:.4rem;text-align:left}}
code{{overflow-wrap:anywhere}}svg{{max-width:100%;height:auto}}</style>
<main><h1>Riser fatigue factor analysis</h1>
<p>Draft. Synthetic interface example; physical acceptance is not evaluated.</p>
<h2>Summary</h2>
<p>Explicit safety-class inputs remove the unqualified fatigue-factor default.
Arithmetic verdict: {status}. Standards verdict: NOT_EVALUATED.
The supplied damage is below the supplied allowable; no standard class factor is qualified.</p>
<h2>Revision history</h2>
<table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>
<tr><td>1</td><td>{CHANGES['date']}</td><td>First issue: demonstrate explicit
class-factor inputs and distinguish software arithmetic from standards adequacy.</td></tr></table>
<h2>Inputs</h2>
<p>All values are synthetic software assumptions. The class factor is an explicit
input because the available F201 fixture supplies metadata without a standards table.</p>
<table><tr><th>Quantity</th><th>Input</th><th>Basis</th></tr>
<tr><td>Safety class</td><td>{result['safety_class']}</td><td>Caller selection</td></tr>
<tr><td>Class DFF, F</td><td>{result['factor']:.1f}</td><td>Synthetic caller factor</td></tr>
<tr><td>Design-life damage, D</td><td>{result['damage']:.3f}</td><td>Synthetic dimensionless demand</td></tr>
</table>
<h2>Method and hand result</h2>
<p>The existing touchdown step-5 contract defines allowable damage A = 1/F,
usage U = D/A = D F, and a pass when U ≤ 1. These are definitions for a supplied
criterion, not independently sourced standard requirements. For this example,
A = 1/4 = 0.250; U = 0.200 × 4 = 0.8, so the arithmetic passes.
Damage derivation from stress cycles is outside this example.</p>
<h2>Program result</h2>
<p>The builder calls get_riser_dff, resolve_dff and fatigue_damage_verdict.
It returns A = {result['allowable_damage']:.3f}, U = {result['usage_factor']:.1f},
and {status}. The regression test independently writes out the arithmetic.
The difference from the hand result is {allowable_difference:.1f} % for
allowable damage and {usage_difference:.1f} % for usage.</p>
<p>Citation note: {html.escape(result['citation_note'])}.</p>
<figure><svg viewBox="0 0 500 100" role="img" aria-label="Damage and allowable damage">
<text x="0" y="18">Dimensionless design-life damage</text>
<rect x="0" y="30" width="{result['usage_factor'] * 400:.1f}" height="20" fill="#286b91"/>
<line x1="400" x2="400" y1="25" y2="60" stroke="black" stroke-width="2"/>
<text x="0" y="85">Damage {result['damage']:.3f}; allowable {result['allowable_damage']:.3f}</text>
</svg><figcaption>Figure 1. Generated demand bar against the supplied allowable marker.</figcaption></figure>
<h2>Limits of use and way forward</h2>
<p>This is a conditional arithmetic pass for synthetic inputs. Engineering acceptance
requires a qualified safety class, class factor, damage history and component model.
A missing class or class factor is refused; a supplied override records both values.
Explicit workflow dff inputs retain their previous precedence.</p>
<h2>NOT_EVALUATED</h2>
<ul><li>DNV-OS-F201 (2010) class-table values, exact locator and applicability.</li>
<li>Stress histories, S–N basis, SCF, thickness effects, wave and VIV response.</li>
<li>Private-context sources, component integrity and engineering acceptance.</li></ul>
<h2>Sources and reproducibility</h2>
<p>DNV-OS-F201, Dynamic Risers, 2010, design fatigue factors: metadata from the
committed citation fixture only; table locator unconfirmed. The attempted public
standards page returned HTTP 404 on 2026-10-22, so no numeric table value was used.</p>
<p>Equation source: the repository's touchdown.py step 5 and workflow.py damage
verdict definition. Source hashes below normalize CRLF to LF for text identity.</p>
<ul>{hashes}</ul>
<p>Rebuild with PYTHONPATH=src python scripts/reports/build_riser_dff_example.py.
The accompanying changes.json records this issue. The report is separate from the
reference-database inventory report.</p>
</main></html>
'''
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "fatigue-factor-analysis.html").write_text(report, encoding="utf-8")
    (output_dir / "changes.json").write_text(
        json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8"
    )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=DESTINATION)
    build(parser.parse_args().output_dir)
