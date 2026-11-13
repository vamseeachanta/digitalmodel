"""Write data/roll_damping/manifest.yaml: row count and SHA-256 of each table."""

from __future__ import annotations

import csv
import hashlib
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parents[2] / "data" / "roll_damping"
TABLES = ("benchmark_cases.csv", "sources.csv", "method_matrix.csv", "wiki_tables.csv", "acceptance_criteria.yaml")


def main() -> None:
    tables = {}
    for name in TABLES:
        raw = (ROOT / name).read_bytes()
        entry = {"file": name, "sha256": hashlib.sha256(raw).hexdigest()}
        if name.endswith(".csv"):
            entry["rows"] = sum(1 for _ in csv.DictReader(raw.decode("utf-8").splitlines()))
        else:
            entry["quantities"] = len(yaml.safe_load(raw)["quantities"])
        tables[name.rsplit(".", 1)[0]] = entry
    doc = {"generated_by": "scripts/roll_damping/build_manifest.py", "route": "public", "tables": tables}
    (ROOT / "manifest.yaml").write_bytes(yaml.safe_dump(doc, sort_keys=False).encode("utf-8"))


if __name__ == "__main__":
    main()
