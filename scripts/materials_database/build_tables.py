"""Build data/materials_database/ from scripts/materials_database/sources.yml (#2157).

Mechanism only: every value, source document and rights note lives in sources.yml. The
output is deterministic (sorted rows, LF line endings, fixed key order), so a rebuild is
byte-identical and the manifest hashes can be checked in CI.

Usage: python scripts/materials_database/build_tables.py
"""

from __future__ import annotations

import csv
import hashlib
import io
from pathlib import Path

import yaml

TABLE = "elevated_temperature_properties"
COLUMNS = [
    "grade",
    "property",
    "temperature_c",
    "value",
    "unit",
    "basis",
    "source_id",
    "source_page",
    "note",
]


def build(sources_path: Path, out_dir: Path) -> None:
    spec = yaml.safe_load(Path(sources_path).read_text(encoding="utf-8"))
    doc_ids = {d["id"] for d in spec["documents"]}
    rows = []
    for raw in spec["rows"]:
        row = dict(zip(COLUMNS, raw))
        if row["source_id"] not in doc_ids:
            raise ValueError(f"row cites unknown source {row['source_id']!r}")
        rows.append(row)
    rows.sort(key=lambda r: (str(r["grade"]), r["property"], float(r["temperature_c"])))

    buf = io.StringIO()
    writer = csv.DictWriter(buf, fieldnames=COLUMNS, lineterminator="\n")
    writer.writeheader()
    for r in rows:
        writer.writerow({k: r[k] for k in COLUMNS})
    csv_bytes = buf.getvalue().encode("utf-8")

    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / f"{TABLE}.csv").write_bytes(csv_bytes)

    manifest = {
        "generated_by": "scripts/materials_database/build_tables.py",
        "issue": 2157,
        "route": "public",
        "rights_decision": " ".join(str(spec["rights_decision"]).split()),
        "sources_spec": "scripts/materials_database/sources.yml",
        "tables": {
            TABLE: {
                "file": f"{TABLE}.csv",
                "rows": len(rows),
                "sha256": hashlib.sha256(csv_bytes).hexdigest(),
            }
        },
    }
    text = yaml.safe_dump(manifest, sort_keys=True, allow_unicode=True, width=100)
    (out_dir / "manifest.yaml").write_bytes(text.replace("\r\n", "\n").encode("utf-8"))


if __name__ == "__main__":
    here = Path(__file__).resolve()
    build(here.with_name("sources.yml"), here.parents[2] / "data" / "materials_database")
