# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Rebuild the synthetic AQWA listing completeness report without a solver.

Run with PYTHONPATH=src and --scratch pointing to an external scratch folder.
The standalone section deliberately does not change the module report index.
"""

import argparse
import hashlib
from html import escape
import json
from pathlib import Path
import runpy

from digitalmodel.hydrodynamics.diffraction.aqwa_lis_parser import AQWALISParser
from digitalmodel.hydrodynamics.diffraction.grid_completeness import GridCompletenessError

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / "docs/reports/modules/diff/lis-grid"
SOURCE = "https://openstax.org/books/university-physics-volume-1/pages/15-1-simple-harmonic-motion"
DATE = "2026-10-22"


def numeric_rows(text):
    """Independent token-count replay of the historical complete-row window."""
    rows = []
    for line in text.splitlines():
        tokens = line.split()
        if len(tokens) == 14:
            try:
                rows.append([float(v) for v in tokens])
            except ValueError:
                pass
    return rows


def calculate(scratch):
    generator = runpy.run_path(str(ROOT / "scripts/testing/generate_aqwa_lis.py"))
    # Stated synthetic layout inputs: 33 rows (issue #2107) and 20 page-header
    # lines after row 10 to exceed main's 12-nonmatching-line cutoff.
    path = generator["write_pair"](Path(scratch) / "complete", gap=20)
    parser = AQWALISParser(path)
    parsed = parser.parse_all()
    marker = generator["ADDED"]
    table = parser.content.split(marker, 1)[1].split(generator["DAMPING"], 1)[0]
    # Historical byte extent from issue #2107: content[pos:pos+3000].
    old_rows = numeric_rows((marker + table)[:3000])
    # Replay main's former stop after more than 12 nonmatching lines.
    seen, gap, baseline = False, 0, 0
    for line in table.splitlines():
        if numeric_rows(line):
            baseline += 1
            seen, gap = True, 0
        elif seen:
            gap += 1
            if gap > 12:
                break
    damaged = generator["write_pair"](Path(scratch) / "damaged", added_rows=32)
    try:
        AQWALISParser(damaged).parse_added_mass_table()
    except GridCompletenessError as error:
        refusal = {"reason": error.reason, "expected": error.expected, "actual": error.actual}
    else:
        raise AssertionError("Damaged table must refuse")
    return {
        "declared": parser.declared_frequency_count,
        "parsed": len(parsed["frequencies"]),
        "added_mass": len(parsed["added_mass"]),
        "damping": len(parsed["damping"]),
        "rao_pairs": len(parsed["raos"]),
        "old_window_count": len(old_rows),
        "baseline_gap_count": baseline,
        "damaged": refusal,
        "periods": parsed["periods"],
        "frequencies": parsed["frequencies"],
        "synthetic_listing_sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
        "synthetic_deck_sha256": hashlib.sha256(path.with_suffix(".dat").read_bytes()).hexdigest(),
    }


def render(data):
    bars = []
    # SVG coordinates are presentation geometry; widths follow computed counts.
    for index, (label, key) in enumerate((
            ("Historical window replay", "old_window_count"),
            ("Previous page-gap cutoff", "baseline_gap_count"),
            ("Structural parser", "parsed"))):
        count = data[key]
        y = 30 + index * 50
        bars.append(f'<text x="0" y="{y}">{escape(label)}</text>'
                    f'<rect x="240" y="{y - 15}" width="{300 * count / data["declared"]:.1f}" '
                    f'height="20" fill="#246887" data-count="{count}"/>'
                    f'<text x="550" y="{y}">{count}</text>')
    rows = "".join(f'<tr data-period-row="{i}"><td>{i}</td><td>{period:.1f}</td>'
                   f'<td>{frequency:.1f}</td></tr>'
                   for i, (period, frequency) in enumerate(
                       zip(data["periods"], data["frequencies"]), 1))
    error = data["damaged"]
    return f'''<!doctype html>
<html lang="en"><meta charset="utf-8">
<title>AQWA listing grid completeness analysis</title>
<style>body{{font:17px/1.6 system-ui,sans-serif;max-width:960px;margin:2rem auto;padding:1rem;color:#173042}}
table{{border-collapse:collapse}}td,th{{border:1px solid #ccd;padding:.3rem .8rem;text-align:left}}
svg{{width:100%;height:auto}}code{{overflow-wrap:anywhere}}</style>
<main><h1>AQWA listing grid completeness analysis</h1>
<section><h2>Summary</h2><p>The structural reader returns all {data['parsed']} declared frequencies,
{data['added_mass']} added-mass matrices and {data['damping']} damping matrices.
A damaged table is refused before conversion or comparison. This is a parser analysis;
it establishes no hydrodynamic accuracy or design acceptance.</p></section>
<section><h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>
<tr><td>0</td><td>{DATE}</td><td>Document synthetic completeness and refusal to prevent silent grid loss.</td></tr></table></section>
<section><h2>Inputs</h2><p>All listing text is generated. The explicit deck contains
{data['declared']} indexed HRTZ cards. Synthetic frequencies are ωᵢ = i/10 rad/s,
i = 1…{data['declared']}. Coefficient column j is i×j for added mass and −i×j for damping,
j = 1…12. These are stipulated parser inputs, not physical coefficients. Two synthetic
headings are 0° and 90°. Twenty header lines after row ten exercise pagination.</p></section>
<section><h2>Method and equation sources</h2><p>T = 2π/ω follows
<a href="{SOURCE}">OpenStax, University Physics Volume 1 (2016), §15.1, angular frequency and period</a>.
The acceptance identity Nparsed = Ndeclared is an exact cardinality contract, not a numerical tolerance.
The card layout follows <code>aqwa_backend.py: AQWABackend.build_deck6</code>.
The twelve selected matrix fields retain the existing reader mapping; their physical
units and omitted couplings are NOT_EVALUATED here.</p>
<p>The historical {data['old_window_count']}-row count replays the fixed window described in issue #2107
on this synthetic text. The previous main reader's page-gap cutoff returns {data['baseline_gap_count']}.
These computed counts describe this fixture, not a recovered solver run.</p></section>
<section><h2>Worked example and answer</h2>
<svg viewBox="0 0 600 175" role="img" aria-label="Computed parsed row counts">
<title>Computed counts from the synthetic table</title>{''.join(bars)}</svg>
<p>Every row below is read from the generated listing. Periods and frequencies are displayed to
one decimal place; parsing and checks use the unrounded stored text.</p>
<table><tr><th>Row</th><th>Period (s)</th><th>Angular frequency (rad/s)</th></tr>{rows}</table>
<p>Independent final-row calculation: i×j = {data['declared']}×1 = {data['declared']} for M11;
C11 is its negative. The RAO grid contains {data['rao_pairs']} frequency-heading pairs.
Deleting one added-mass row gives {error['actual']} against {error['expected']} declared:
<code>{escape(error['reason'])}</code>. No padded result is returned.</p></section>
<section><h2>Limits of use and NOT_EVALUATED</h2><ul>
<li>Solver coefficient accuracy, matrix unit normalization, engineering acceptance and real-output qualification: NOT_EVALUATED.</li>
<li>A listing without a matching deck or explicit count has declaration_status = NOT_EVALUATED;
internal grid agreement alone does not prove independent completeness.</li>
<li>Automatic declaration reading supports explicit indexed HRTZ cards. Ranges, other frequency cards,
ambiguous sibling decks and malformed cards refuse; their interpretation is NOT_EVALUATED.</li>
<li>Supported table boundaries are different variation-table titles and stability/mooring section titles.
EOF is refused. Other listing dialects, multibody selection and incomplete RAO value syntax are NOT_EVALUATED.</li>
</ul><p>Supply the matching deck or an independently known expected count, retain the listing terminators,
and correct a refused input before benchmarking. This section is separate from the main diffraction report.</p></section>
<section><h2>Reproduction</h2><p>With the repository runtime dependencies available:</p>
<pre>PYTHONPATH=src uv run --no-project --with numpy --with loguru --with pyyaml --with scipy --with pydantic --with plotly --with pandas --with jinja2 python scripts/reporting/build_aqwa_lis_grid_report.py --scratch &lt;scratch&gt;</pre>
<p>The builder regenerates this page, changes.json and evidence.json. The regression test rebuilds
them in temporary storage and compares every byte. No network or solver is accessed by the builder.</p></section>
</main></html>
'''


def build(output, scratch):
    data = calculate(scratch)
    output = Path(output)
    output.mkdir(parents=True, exist_ok=True)
    (output / "report.html").write_text(render(data), encoding="utf-8")
    (output / "evidence.json").write_text(json.dumps(data, indent=2) + "\n", encoding="utf-8")
    changes = {"document": "aqwa-listing-grid-completeness", "revision": 0, "date": DATE,
               "changes": ["Add generated completeness example and damaged-table refusal."]}
    (output / "changes.json").write_text(json.dumps(changes, indent=2) + "\n", encoding="utf-8")
    return data


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, default=OUTPUT)
    parser.add_argument("--scratch", type=Path, required=True)
    arguments = parser.parse_args()
    build(arguments.output, arguments.scratch)
