# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check the public drillship aggregate and fail closed on insufficient diversity."""

from hashlib import sha256
import json
from pathlib import Path
from statistics import median

import pytest


ROOT = Path(__file__).resolve().parents[3]
TABLE = ROOT / "data/vessels/processed/drillship_basis.json"


def test_generic_drillship_uses_measured_public_subset():
    """A multi-design public sample determines the labelled rounded dimensions."""
    from digitalmodel.hydrodynamics.hull_library import generic_drillship as module

    basis = module.generic_drillship_basis()
    assert basis.distinct_loa_beam_count >= 5
    aggregate = json.loads(TABLE.read_bytes())
    measured = aggregate["dimensions_m"]
    pairs = {tuple(row[:2]) for row in measured}
    assert basis.subset_count == len(measured) == aggregate["row_count"]
    assert basis.distinct_loa_beam_count == len(pairs) == aggregate["distinct_loa_beam_count"]
    for axis, key in enumerate(("loa", "beam", "draft")):
        dimension = getattr(basis, key)
        midpoint = median(row[axis] for row in measured)
        assert midpoint == aggregate["median_dimensions_m"][axis]
        assert dimension.value_m == round(midpoint)
        assert dimension.label == "assumed, based on public fleet data"
    assert basis.depth is None
    assert "omitted" in basis.depth_note
    assert basis.data_file == "data/vessels/processed/drillship_basis.json"
    assert basis.data_sha256 == sha256(TABLE.read_bytes()).hexdigest()
    assert basis == module.generic_drillship_basis()
    assert aggregate["selection"]["rig_type"] == "drillship"
    assert aggregate["selection"]["dimension_confidence"] == "measured"
    assert all(row[0] >= aggregate["selection"]["minimum_loa_m"] for row in measured)
    assert aggregate["source"]["access"] == "public"
    assert aggregate["source"]["sha256"] == (
        "9a38304b32fef1ca92ab598fba6d4388c71901030aad2f8c7eb00542471e5899"
    )
    # Record identities are deliberately absent from the numeric derivative.
    records = json.loads((ROOT / "data/vessels/raw/floating__particulars.json").read_text())
    source = Path(module.__file__).read_text().casefold() + TABLE.read_text().casefold()
    assert all(row["name"].casefold() not in source for row in records["records"])


@pytest.mark.parametrize("distinct_count", [0, 1, 4])
def test_insufficient_distinct_hulls_raise(monkeypatch, distinct_count):
    """Repeated records cannot turn fewer than five designs into a generic basis."""
    from digitalmodel.hydrodynamics.hull_library.generic_drillship import generic_drillship_basis

    rows = [[200.0 + i, 40.0 + i, 12.0] for i in range(distinct_count)] * 10
    monkeypatch.setattr(Path, "read_bytes", lambda self: json.dumps({"dimensions_m": rows}).encode())
    with pytest.raises(ValueError, match="at least 5 distinct.*LOA.*beam"):
        generic_drillship_basis()


def test_five_distinct_hulls_are_sufficient(monkeypatch):
    """The diversity boundary accepts five pairs and keeps record-weighted medians."""
    from digitalmodel.hydrodynamics.hull_library.generic_drillship import generic_drillship_basis

    rows = [[200.0 + i, 40.0 + i, 10.0 + i] for i in range(5)]
    rows.extend([rows[0]] * 10)
    monkeypatch.setattr(Path, "read_bytes", lambda self: json.dumps({"dimensions_m": rows}).encode())
    basis = generic_drillship_basis()
    assert basis.subset_count == len(rows)
    assert basis.distinct_loa_beam_count == 5
    assert (basis.loa.value_m, basis.beam.value_m, basis.draft.value_m) == (200.0, 40.0, 10.0)


@pytest.mark.parametrize("bad_row", [[0, 40, 12], [200, -1, 12], [200, 40, None],
                                      [200, 40, float("nan")], [200, 40, float("inf")],
                                      [200, 40], [200, 40, 12, 1]])
def test_invalid_dimensions_raise(monkeypatch, bad_row):
    """Malformed or nonphysical aggregate rows cannot silently change the sample."""
    from digitalmodel.hydrodynamics.hull_library.generic_drillship import generic_drillship_basis

    rows = [[200.0 + i, 40.0 + i, 12.0] for i in range(5)] + [bad_row]
    monkeypatch.setattr(Path, "read_bytes", lambda self: json.dumps({"dimensions_m": rows}).encode())
    with pytest.raises(ValueError, match="finite positive dimensions"):
        generic_drillship_basis()
