# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build a standalone discrete installation heading chart under docs/api."""

from __future__ import annotations

import argparse
import hashlib
from html import escape
import json
import math
from pathlib import Path
import re

ROOT = Path(__file__).resolve().parents[2]
ASSETS = Path(__file__).with_name("installation_heading")
DATA = ASSETS / "limits.json"
REVISIONS = ASSETS / "revisions.json"
TEMPLATE = ASSETS / "page.html.template"
PAGE_NAME = "installation-heading-explorer.html"
MANIFEST_NAME = "installation-heading-explorer.manifest.json"
PAGE_REL = "docs/api/installation/" + PAGE_NAME


def load_data(path: Path) -> dict:
    """Read JSON while enforcing the one-decimal numeric representation."""
    def decimal(token: str) -> float:
        if not re.fullmatch(r"-?\d+\.\d", token):
            raise ValueError("Hs numbers require one decimal place")
        return float(token)

    def unique(pairs: list) -> dict:
        result = {}
        for key, value in pairs:
            if key in result:
                raise ValueError("Duplicate JSON key")
            result[key] = value
        return result

    return json.loads(path.read_text(encoding="utf-8"), parse_float=decimal,
                      object_pairs_hook=unique)


def validate_data(data: dict) -> None:
    """Validate explicit signed samples without inventing missing limits.

    Issue #2696 (2026), Deliverable and acceptance criteria, supplies the
    15-degree grid, inclusive ±15-degree band and one-decimal Hs display.
    Null denotes no data; zero is a supplied limit, not a missing sample.
    """
    def require(condition: bool, message: str) -> None:
        if not condition:
            raise ValueError(message)

    headings = data.get("headings_deg")
    require(isinstance(headings, list) and all(type(h) is int for h in headings)
            and headings == list(range(-180, 181, 15)), "Expected signed 15-degree headings")
    require(data.get("head_seas_band_deg") == [-15, 15], "Expected head-seas sector plus or minus 15 degrees")
    require(type(data.get("illustrative")) is bool, "Expected illustrative flag")
    require(data["illustrative"], "Only illustrative data are supported pending reviewed results ingestion")
    for key in ("source", "revision", "condition_basis"):
        require(isinstance(data.get(key), str) and bool(data[key].strip()), f"Missing {key}")
    if data["illustrative"]:
        require("invented" in data["source"].lower(), "Illustrative source must identify invented values")
    sizes = data.get("sizes")
    require(isinstance(sizes, list) and bool(sizes), "Missing sizes")
    require(all(isinstance(size, str) and size.strip() for size in sizes), "Invalid sizes")
    require(len(set(sizes)) == len(sizes), "Duplicate sizes")
    stages = data.get("stages")
    require(isinstance(stages, list) and bool(stages), "Missing stages")
    names = []
    for stage in stages:
        name = stage.get("name")
        require(isinstance(name, str) and bool(name.strip()), "Missing stage name")
        names.append(name)
        limits = stage.get("limits_m")
        require(isinstance(limits, dict) and set(limits) == set(sizes), "Incomplete stage sizes")
        for values in limits.values():
            require(isinstance(values, list) and len(values) == len(headings), "Incomplete heading grid")
            require(any(value is not None for value in values), "Missing all heading samples")
            require(all(value is None or (
                type(value) is float and math.isfinite(value) and value >= 0
                and value == round(value, 1)) for value in values),
                "Expected nonnegative Hs numbers with one decimal or null")
            require(values[0] == values[-1], "Endpoint mismatch at minus/plus 180 degrees")
    require(len(set(names)) == len(names), "Duplicate stages")


def build(data_path: Path = DATA, output_dir: Path | None = None,
          revisions_path: Path = REVISIONS) -> None:
    """Render the page and a digest manifest without time-dependent output."""
    data = load_data(data_path)
    validate_data(data)
    revisions = json.loads(revisions_path.read_text(encoding="utf-8"))
    if not revisions or revisions[-1].get("revision") != data["revision"]:
        raise ValueError("Newest revision must match declared revision")
    rows = []
    for row in revisions:
        for field in ("revision", "date", "change"):
            if not isinstance(row.get(field), str) or not row[field].strip():
                raise ValueError("Incomplete revision record")
        rows.append("<tr>" + "".join(f"<td>{escape(row[key])}</td>"
                                   for key in ("revision", "date", "change")) + "</tr>")
    embedded = json.dumps(data, ensure_ascii=False, indent=2, allow_nan=False)
    # JSON stays inert even if replacement data contains HTML delimiters.
    embedded = embedded.replace("&", "\\u0026").replace("<", "\\u003c").replace(">", "\\u003e")
    replacements = {
        "@@DATA@@": embedded,
        "@@REVISION_ROWS@@": "\n".join(rows),
        "@@REVISION@@": escape(data["revision"]),
        "@@STATUS@@": "illustrative",
        "@@BADGE@@": "ILLUSTRATIVE",
        "@@SOURCE@@": escape(data["source"]),
        "@@CONDITION_BASIS@@": escape(data["condition_basis"]),
        "@@NOTICE@@": "Invented numbers for interaction demonstration; no operational limit is established.",
    }
    text = TEMPLATE.read_text(encoding="utf-8")
    # A single substitution pass prevents data text from being treated as a placeholder.
    text = re.sub(r"@@[A-Z_]+@@", lambda match: replacements[match[0]], text)
    output_dir = output_dir or ROOT / "docs" / "api" / "installation"
    output_dir.mkdir(parents=True, exist_ok=True)
    page = output_dir / PAGE_NAME
    page.write_text(text, encoding="utf-8", newline="\n")
    inputs = {"builder": Path(__file__), "template": TEMPLATE,
              "data": data_path, "revisions": revisions_path}
    digests = {PAGE_REL: hashlib.sha256(page.read_bytes()).hexdigest()}
    for path in inputs.values():
        # Custom inputs outside the repository retain a role, never a local address.
        key = path.relative_to(ROOT).as_posix() if path.is_relative_to(ROOT) else path.name
        digests[key] = hashlib.sha256(path.read_bytes()).hexdigest()
    manifest = {"revision": data["revision"], "illustrative": data["illustrative"],
                "records": sum(value is not None for stage in data["stages"]
                               for values in stage["limits_m"].values() for value in values),
                "sha256": digests}
    (output_dir / MANIFEST_NAME).write_text(
        json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8", newline="\n")


def main() -> None:
    """Build from optional replacement data into an optional output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--data", type=Path, default=DATA)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    build(data_path=args.data, output_dir=args.output_dir)


if __name__ == "__main__":
    main()
