# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Derive assumed drillship dimensions from a public multi-design sample.

Data: data/vessels/processed/drillship_basis.json retains 50 numeric rows
from the cited public particulars table. Selection requires measured drillship
records, LOA at least 150 m and finite positive LOA, beam and draft. The data
records the upstream revision, digest and selection, without record identities.

At least five distinct (LOA, beam) pairs are required. Repeated designs retain
their row weighting in the medians. Medians are rounded to whole metres and
labelled assumed. Depth is omitted from this derivative. This basis generates
no hull or result and is not a population census.
"""

import json
from dataclasses import dataclass
from hashlib import sha256
from math import isfinite
from pathlib import Path
from statistics import median


DATA_FILE = "data/vessels/processed/drillship_basis.json"
MIN_DISTINCT_HULLS = 5
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
    distinct_loa_beam_count: int
    data_file: str = DATA_FILE
    depth: None = None
    depth_note: str = "not retained in the numeric aggregate; omitted"


def generic_drillship_basis() -> GenericDrillshipBasis:
    """Return assumed medians only when the numeric sample spans several hulls."""
    table = Path(__file__).resolve().parents[4] / DATA_FILE
    source = table.read_bytes()
    selected = []
    for row in json.loads(source)["dimensions_m"]:
        try:
            if len(row) != 3:
                raise ValueError
            values = tuple(float(value) for value in row)
            if not all(isfinite(value) and value > 0 for value in values):
                raise ValueError
        except (TypeError, ValueError) as error:
            raise ValueError("Expected three finite positive dimensions per row") from error
        selected.append(values)
    distinct_count = len({values[:2] for values in selected})
    if distinct_count < MIN_DISTINCT_HULLS:
        raise ValueError(
            f"Generic drillship basis requires at least {MIN_DISTINCT_HULLS} distinct "
            f"(LOA, beam) pairs; found {distinct_count}"
        )
    dimensions = [
        AssumedDimension(float(round(median(values[axis] for values in selected))))
        for axis in range(3)
    ]
    loa, beam, draft = dimensions
    return GenericDrillshipBasis(
        loa=loa, beam=beam, draft=draft, subset_count=len(selected),
        data_sha256=sha256(source).hexdigest(),
        distinct_loa_beam_count=distinct_count,
    )
