# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Recompute an assumed drillship basis from the public repository table."""

import csv
from hashlib import sha256
import json
from math import isfinite
from pathlib import Path
from statistics import median


ROOT = Path(__file__).resolve().parents[3]
TABLE = ROOT / "data/vessels/processed/particulars.csv"


def test_generic_drillship_uses_measured_public_subset():
    """Complete measured rows determine rounded dimensions and assumption labels."""
    from digitalmodel.hydrodynamics.hull_library import generic_drillship as module

    with TABLE.open(newline="") as handle:
        rows = list(csv.DictReader(handle))
    measured = []
    for row in rows:
        if (row["scope"] != "floating" or "drillship" not in row["vessel_type"].lower()
                or "class-level" in row["name"].lower() or int(row["n_citations"]) <= 0):
            continue
        try:
            values = [float(row[key]) for key in ("loa", "beam", "draft")]
        except ValueError:
            continue
        if all(isfinite(value) and value > 0 for value in values):
            measured.append(row)
    basis = module.generic_drillship_basis()
    assert basis.subset_count == len(measured) == 2
    for key in ("loa", "beam", "draft"):
        dimension = getattr(basis, key)
        assert dimension.value_m == round(median(float(row[key]) for row in measured))
        assert dimension.label == "assumed, based on public fleet data"
    assert basis.depth is None
    assert "omitted" in basis.depth_note
    assert all(not row["depth"] for row in measured)
    assert basis.data_file == "data/vessels/processed/particulars.csv"
    assert basis.data_sha256 == sha256(TABLE.read_bytes()).hexdigest()
    assert basis == module.generic_drillship_basis()
    source = Path(module.__file__).read_text().casefold()
    assert all(row["name"].casefold() not in source for row in rows)
    raw = json.loads((ROOT / "data/vessels/raw/floating__particulars.json").read_text())
    records = {row["name"]: row for row in raw["records"]}
    for row in measured:
        public = [c for c in records[row["name"]]["citations"] if c["access"] == "public"]
        fields = {field for citation in public for field in citation["fields"]}
        assert {"LOA_m", "beam_m", "draft_operating_m"} <= fields
