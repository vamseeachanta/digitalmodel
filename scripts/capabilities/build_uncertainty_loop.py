# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build the qualitative uncertainty loop under docs/domains/asset-integrity.

Workflow basis: issue #2695 (2026), Purpose and deliverable 1. This schematic
evaluates no engineering equation, coefficient, threshold or acceptance limit.
"""

from __future__ import annotations

import argparse
import hashlib
from html import escape
import json
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
ASSETS = Path(__file__).with_name("uncertainty_loop")
DATA = ASSETS / "workflow.json"
TEMPLATE = ASSETS / "page.html.template"
OUTPUT = ROOT / "docs" / "domains" / "asset-integrity"
PAGE_NAME = "uncertainty-loop.html"
MANIFEST_NAME = "uncertainty-loop.manifest.json"

# The issue defines this qualitative order and the evidence streams, not limits.
STEP_IDS = ("measure", "model", "quantify", "decide", "reinspect")
EVIDENCE_IDS = ("inspection", "survey", "metocean", "operations")


def validate_data(data: dict) -> None:
    """Require complete synthetic workflow content and unevaluated decisions."""
    def require(condition: bool, message: str) -> None:
        if not condition:
            raise ValueError(message)

    def text_fields(record: dict, fields: tuple[str, ...]) -> None:
        require(isinstance(record, dict), "Expected an object")
        for field in fields:
            value = record.get(field)
            require(isinstance(value, str) and bool(value.strip()), f"Missing {field}")

    text_fields(data, ("revision", "date", "change"))
    require(data.get("status") == "synthetic", "Only synthetic context is supported")
    require(data.get("decision_status") == "NOT_EVALUATED", "No decision is evaluated")
    text_fields(data.get("provenance"), ("source", "rights", "basis", "equations", "ai_use"))
    for key, expected, fields in (
        ("steps", STEP_IDS, ("id", "label", "input", "output", "missing", "criterion")),
        ("evidence", EVIDENCE_IDS, ("id", "label", "quantity", "unit", "required", "missing")),
    ):
        records = data.get(key)
        require(isinstance(records, list), f"Missing {key}")
        for record in records:
            text_fields(record, fields)
        require(tuple(row["id"] for row in records) == expected, f"Invalid {key} sequence")
    require(all(row.get("status") == "NOT_EVALUATED" for row in data["evidence"]),
            "Evidence cannot be represented as qualified")
    feedback = data.get("feedback")
    text_fields(feedback, ("source", "target", "trigger", "action"))
    require(feedback["source"] == "process-change" and feedback["target"] == "model",
            "Process change must return to historical trend reassessment in the model")


def build(data_path: Path = DATA, output_dir: Path | None = None) -> None:
    """Write self-contained HTML and reproducible graph/input digest metadata."""
    data = json.loads(data_path.read_text(encoding="utf-8"))
    validate_data(data)
    evidence_rows = []
    for row in data["evidence"]:
        evidence_rows.append(
            f'<tr role="row"><th scope="row" role="rowheader">{escape(row["label"])}</th>'
            f'<td role="cell" data-label="Quantity / unit">{escape(row["quantity"])}'
            f'<br><span class="unit">{escape(row["unit"])}</span></td>'
            f'<td role="cell" data-label="Required input">{escape(row["required"])}</td>'
            f'<td role="cell" data-label="Status / missing evidence">'
            f'<strong>{escape(row["status"])}</strong><br>{escape(row["missing"])}</td></tr>'
        )
    steps = data["steps"]
    cycle = list(zip(STEP_IDS, (*STEP_IDS[1:], STEP_IDS[0])))
    cards = []
    for step, (_, target) in zip(steps, cycle):
        terms = (("Input", "input"), ("Output", "output"),
                 ("Missing evidence", "missing"), ("Decision criterion", "criterion"))
        fields = "".join(f"<dt>{label}</dt><dd>{escape(step[key])}</dd>" for label, key in terms)
        next_label = next(item["label"] for item in steps if item["id"] == target)
        cards.append(
            f'<li><details id="{step["id"]}"><summary>{escape(step["label"])}</summary>'
            f'<dl>{fields}</dl></details><p class="flow">'
            f'<span aria-hidden="true">↓ </span>Next: {escape(next_label)}</p></li>'
        )
    positions = ((90, 40), (550, 40), (550, 310), (320, 450), (90, 310))
    svg_nodes = []
    for step, (x, y) in zip(steps, positions):
        svg_nodes.append(
            f'<g data-node="{step["id"]}" transform="translate({x} {y})">'
            '<rect width="180" height="64" rx="12"/>'
            f'<text x="90" y="38">{escape(step["label"])}</text></g>'
        )
    embedded = json.dumps(data, ensure_ascii=False, indent=2, allow_nan=False)
    embedded = embedded.replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")
    replacements = {
        "@@DATA@@": embedded,
        "@@EVIDENCE_ROWS@@": "\n".join(evidence_rows),
        "@@SVG_NODES@@": "\n".join(svg_nodes),
        "@@STEPS@@": "\n".join(cards),
        "@@REVISION@@": escape(data["revision"]),
        "@@DATE@@": escape(data["date"]),
        "@@CHANGE@@": escape(data["change"]),
        "@@TRIGGER@@": escape(data["feedback"]["trigger"]),
        "@@ACTION@@": escape(data["feedback"]["action"]),
    }
    replacements.update({f"@@{key.upper()}@@": escape(value)
                         for key, value in data["provenance"].items()})
    # Substitute once so literal placeholder-like text in evidence stays inert.
    page = re.sub(r"@@[A-Z_]+@@", lambda match: replacements[match[0]],
                  TEMPLATE.read_text(encoding="utf-8"))
    output_dir = OUTPUT if output_dir is None else output_dir
    output_dir.mkdir(parents=True, exist_ok=True)
    page_path = output_dir / PAGE_NAME
    page_path.write_text(page, encoding="utf-8", newline="\n")
    digests = {(OUTPUT / PAGE_NAME).relative_to(ROOT).as_posix():
               hashlib.sha256(page_path.read_bytes()).hexdigest()}
    for path in (Path(__file__), TEMPLATE, data_path):
        # Replacement inputs outside the repository retain no local address.
        key = path.relative_to(ROOT).as_posix() if path.is_relative_to(ROOT) else path.name
        digests[key] = hashlib.sha256(path.read_bytes()).hexdigest()
    manifest = {
        "revision": data["revision"], "status": data["status"],
        "decision_status": data["decision_status"],
        "method": "Qualitative directed cycle; no numerical model or plotted numerical arrays",
        "cycle": cycle,
        "feedback": {key: data["feedback"][key] for key in ("source", "target")},
        "provenance": data["provenance"], "sha256": digests,
    }
    (output_dir / MANIFEST_NAME).write_text(
        json.dumps(manifest, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
        encoding="utf-8", newline="\n",
    )


def main() -> None:
    """Render optional synthetic workflow input into an optional output folder."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data", type=Path, default=DATA)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    build(data_path=args.data, output_dir=args.output_dir)


if __name__ == "__main__":
    main()
