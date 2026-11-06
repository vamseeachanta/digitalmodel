# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Generate a synthetic calculation report for table-width review.

Run with PYTHONPATH=src and --output pointing outside the repository. No
browser, PDF renderer, domain calculation or external data source is used.
See docs/domains/reporting/design-data-layout.md for inputs and review steps.
"""

from __future__ import annotations

import argparse
from pathlib import Path
from typing import Sequence

from digitalmodel.reporting import (
    CalcReport,
    Confidence,
    DataRow,
    DesignDataGroup,
    MethodBlock,
    Objective,
    Reference,
    ResultBlock,
    ValidationItem,
    WayForwardStage,
)
from digitalmodel.reporting.calc_report import RevisionEntry


# Synthetic strings exercise prose wrapping, entity escaping and an unbroken
# token. Their lengths are test inputs, not engineering or readability limits.
PROVENANCE = (
    "Synthetic input retained in full, including its provenance note, "
    "regional description and final qualifying phrase."
)
IDENTIFIER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_" * 8
SYMBOLS = 'Range < lower bound & note "retained">'


def build_report() -> CalcReport:
    """Build the complete example without writing files or evaluating layout."""
    return CalcReport(
        report_id="LAYOUT-EXAMPLE",
        title="Design-data table layout example",
        discipline="Reporting",
        organisation="Example",
        lede="Synthetic inputs for reviewing table width and complete value access.",
        revision_history=[
            RevisionEntry(
                revision="A",
                date="2026-10-19",
                description="Add reproducible long-value inputs for layout review.",
            )
        ],
        objective=Objective(
            purpose="Keep every supplied value available on screen and in print.",
            scope=["Presentation only", "Synthetic text"],
        ),
        design_data=[
            DesignDataGroup(
                caption="Descriptive inputs",
                rows=[
                    DataRow(label="Provenance note", value=PROVENANCE),
                    DataRow(label="Symbols & units", value=SYMBOLS, unit="unit"),
                ],
            ),
            DesignDataGroup(
                caption="Unbroken input",
                rows=[
                    DataRow(label="Identifier", value=IDENTIFIER),
                ],
            ),
        ],
        methodology=[
            MethodBlock(
                heading="Table-width policy",
                # Source: calc_report_format.html, table / .datagrid / .kv rules
                # at the revision that removed the exemption (PR #2443, 2026).
                prose=(
                    "The shared table minimum remains 440px. Design-data groups "
                    "stack in one column; prose values wrap and narrower screen "
                    "containers scroll horizontally. Long unbroken inputs may "
                    "require scrolling even when prose fits. The minimum is an "
                    "existing stylesheet policy, not a measured readability limit."
                ),
            )
        ],
        results=[
            ResultBlock(
                heading="Expected content",
                confidence=Confidence.ANALYTICAL,
                prose="Each rendered cell retains the corresponding input text and unit.",
                caption="Text preservation is tested independently of browser geometry.",
            )
        ],
        validation=[
            ValidationItem(
                claim="Rendered geometry: NOT_EVALUATED",
                confidence=Confidence.PENDING,
                basis="HTML generation does not establish screen or printed containment.",
            )
        ],
        way_forward=[
            WayForwardStage(
                heading="Complete the rendered review",
                prose=(
                    "Review at a narrow screen width and A4 print preview. Confirm "
                    "that prose wraps, the complete identifier is reachable through "
                    "local scrolling, and all final words remain visible in print. "
                    "Record the viewport, print settings and observed result."
                ),
            )
        ],
        references=[
            Reference(
                text=(
                    "Packaged calc_report_format.html: table, .datagrid and .kv rules; "
                    "design-data width policy retained by PR #2443 (2026)."
                )
            )
        ],
    )


def main(argv: Sequence[str] | None = None) -> None:
    """Write deterministic HTML to the explicitly supplied destination."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(build_report().render_html(), encoding="utf-8")


if __name__ == "__main__":
    main()
