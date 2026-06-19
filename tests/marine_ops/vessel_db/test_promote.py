"""Tests for raw -> processed promotion."""

from __future__ import annotations

import csv
import shutil

import pytest

from digitalmodel.marine_ops.vessel_db.loader import vessels_dir
from digitalmodel.marine_ops.vessel_db.promote import promote


@pytest.fixture
def tmp_base(tmp_path):
    """Copy raw/ into an isolated base so promotion side-effects stay in tmp."""
    src = vessels_dir()
    base = tmp_path / "data" / "vessels"
    (base / "raw").mkdir(parents=True)
    for f in (src / "raw").glob("*.json"):
        shutil.copy(f, base / "raw" / f.name)
    return base


def test_promote_writes_all_outputs(tmp_base):
    result = promote(base=tmp_base)
    proc = tmp_base / "processed"
    for fname in ("particulars.csv", "crane_summary.csv", "rao_datasets.csv",
                  "metocean_criteria.csv", "PROVENANCE_REPORT.md"):
        assert (proc / fname).is_file(), f"missing {fname}"
    assert result["particulars"] > 0
    assert result["crane"] > 0
    assert result["rao"] > 0
    assert result["metocean"] > 0


def test_promote_provenance_clean(tmp_base):
    result = promote(base=tmp_base)
    assert result["provenance_violations"] == 0


def test_estimated_gyradii_carry_basis(tmp_base):
    promote(base=tmp_base)
    rows = list(csv.DictReader(open(tmp_base / "processed" / "particulars.csv")))
    estimated = [r for r in rows if r["kxx_basis"].startswith("estimated:")]
    assert estimated, "expected some estimated gyradii"
    for r in estimated:
        # An estimated gyradius must have both a numeric value and a relation.
        assert r["kxx"], f"{r['name']}: estimated basis but empty value"
        assert "*beam" in r["kxx_basis"]


def test_no_bare_gyradius_without_basis(tmp_base):
    """Every kxx value present must be either cited or estimated (never bare)."""
    promote(base=tmp_base)
    rows = list(csv.DictReader(open(tmp_base / "processed" / "particulars.csv")))
    for r in rows:
        if r["kxx"]:
            assert r["kxx_basis"] in ("cited",) or r["kxx_basis"].startswith("estimated:")
