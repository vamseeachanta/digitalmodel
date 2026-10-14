# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Derive assumed drillship dimensions from repository-held public records.

Data: data/vessels/processed/particulars.csv. The subset contains 2 measured
rows: floating scope, vessel_type containing drillship, no class-level name,
a positive citation count and finite positive loa, beam and draft. Public
origin is recorded in data/vessels/README.md and the citations in
data/vessels/raw/floating__particulars.json.

Medians are rounded to whole metres and labelled assumed. Depth is omitted:
the selected records do not state it. This basis generates no hull or result.
"""

import csv
from dataclasses import dataclass
from hashlib import sha256
from io import StringIO
from math import isfinite
from pathlib import Path
from statistics import median


DATA_FILE = "data/vessels/processed/particulars.csv"
ASSUMED_LABEL = "assumed, based on public fleet data"


@dataclass(frozen=True)
class AssumedDimension:
    """A principal dimension in metres with its assumption label."""

    value_m: float
    label: str = ASSUMED_LABEL


@dataclass(frozen=True)
class GenericDrillshipBasis:
    """Rounded median dimensions and the exact repository data basis."""

    loa: AssumedDimension
    beam: AssumedDimension
    draft: AssumedDimension
    subset_count: int
    data_sha256: str
    data_file: str = DATA_FILE
    depth: None = None
    depth_note: str = "not stated in the selected records; omitted"


def generic_drillship_basis() -> GenericDrillshipBasis:
    """Return labelled whole-metre medians from complete measured drillship rows."""
    table = Path(__file__).resolve().parents[4] / DATA_FILE
    source = table.read_bytes()
    selected = []
    for row in csv.DictReader(StringIO(source.decode("utf-8"))):
        if (row["scope"] != "floating" or "drillship" not in row["vessel_type"].lower()
                or "class-level" in row["name"].lower() or int(row["n_citations"]) <= 0):
            continue
        try:
            values = [float(row[key]) for key in ("loa", "beam", "draft")]
        except ValueError:
            continue
        if all(isfinite(value) and value > 0 for value in values):
            selected.append(values)
    if not selected:
        raise ValueError("No complete measured drillship rows are available")
    dimensions = [
        AssumedDimension(float(round(median(values[axis] for values in selected))))
        for axis in range(3)
    ]
    loa, beam, draft = dimensions
    return GenericDrillshipBasis(
        loa=loa, beam=beam, draft=draft, subset_count=len(selected),
        data_sha256=sha256(source).hexdigest(),
    )
