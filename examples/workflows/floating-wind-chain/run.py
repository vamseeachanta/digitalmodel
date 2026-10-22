"""Build the offline mapping and standard report; output directory is required."""

import argparse
import json
from pathlib import Path

import yaml

from digitalmodel.floating_wind.solver_chain import (
    ChainInput,
    ResultRecord,
    build_chain_report,
    prepare_chain,
)
from digitalmodel.reporting import DocumentMeta, RevisionRow, render_html


def main() -> None:
    """Generate only inputs and a report, optionally comparing stored periods."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--input", type=Path, default=Path(__file__).with_name("input.yml")
    )
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument(
        "--results", type=Path, help="JSON list of normalised ResultRecord objects"
    )
    args = parser.parse_args()
    inputs = ChainInput.model_validate(
        yaml.safe_load(args.input.read_text(encoding="utf-8"))
    )
    bundle = prepare_chain(inputs, args.out)
    records = []
    if args.results:
        records = [
            ResultRecord.model_validate(row)
            for row in json.loads(args.results.read_text(encoding="utf-8"))
        ]
    document = DocumentMeta(
        number="R0000-RPT-001-02",
        revision="02",
        title="Floating wind sizing analysis",
        project="Generic",
        client="Generic",
        date="2026-10-22",
        revision_history=[
            RevisionRow(
                rev="00",
                date="2026-10-20",
                description="Initial offline mapping example; identify data needed for coupled analysis.",
            ),
            RevisionRow(
                rev="01",
                date="2026-10-22",
                description="Require explicit mass basis and spectral waves; publish UTF-8 outputs after validation and record the hull equilibrium gap.",
            ),
            RevisionRow(
                rev="02",
                date="2026-10-22",
                description="Correct screening limits and basis; retain output directory permissions.",
            ),
        ],
    )
    spec = build_chain_report(bundle, document=document, records=records)
    (args.out / "analysis.html").write_text(
        render_html(spec), encoding="utf-8", newline="\n"
    )
    (args.out / "analysis.json").write_text(
        spec.model_dump_json(indent=2) + "\n", encoding="utf-8", newline="\n"
    )
    print("Offline inputs and analysis report written; solver checks NOT_EVALUATED.")


if __name__ == "__main__":
    main()
