"""Comparison coverage, independent of the nominal dimensions under review."""

import json
import subprocess
import sys

import pytest
import yaml

from digitalmodel.bha_database import iso_table14
from digitalmodel.bha_database.loader import BhaDatabaseError


def save_case(tmp_path, update):
    """Change comparison metadata without changing any table quantity."""
    data = yaml.safe_load(iso_table14.DEFAULT_PATH.read_text(encoding="utf-8"))
    update(data)
    saved = tmp_path / "table.yml"
    saved.write_text(yaml.safe_dump(data), encoding="utf-8")
    return saved


def test_audit_accounts_for_every_table_cell():
    """Hand count from the supplied YAML, not fresh source verification.

    ISO 10424-1:2004 Table 14 transcription has 23 rows and six published
    fields per row: 23 * 6 = 138 cells. Issue #2629 marks one pending;
    the other 138 - 1 = 137 have no comparison status. Computed weight
    is outside Table 14 and must not enter the count.
    """
    report = iso_table14.audit_table14()
    assert report["status"] == "NOT_EVALUATED"
    assert report["counts"] == {
        "total": 138, "recorded": 0, "pending": 1, "unspecified": 137,
    }
    assert len(report["cells"]) == report["counts"]["total"]
    assert all(cell["status"] == "NOT_EVALUATED" for cell in report["cells"])
    assert all(cell["field"] != "weight_per_length" for cell in report["cells"])
    assert len({(cell["component_id"], cell["field"])
                for cell in report["cells"]}) == report["counts"]["total"]
    pending = next(cell for cell in report["cells"] if cell["verification"] == "pending")
    assert pending["field"] == "bevel_diameter"
    assert pending["issue"] == "#2629"
    assert "licensed" in pending["reason"]
    assert pending["locator"] is None
    assert report["source_key"] == iso_table14.SOURCE_KEY


def test_one_record_does_not_verify_the_table(tmp_path, monkeypatch):
    """Synthetic evidence: 138 total - 1 recorded - 1 pending = 136 unspecified.

    Counts follow the supplied Table 14 transcription, not a licensed copy.
    A locator on one field must not supply evidence for its row or table.
    """
    def update(data):
        data["components"][0]["od"].update(
            verification="verified", verification_locator="Synthetic printed page A")
        data["comparison_report"] = {"status": "COMPARISON_RECORDED"}

    saved = save_case(tmp_path, update)
    monkeypatch.chdir(tmp_path)
    report = iso_table14.audit_table14(saved)
    assert report["status"] == "NOT_EVALUATED"
    assert report["counts"] == {
        "total": 138, "recorded": 1, "pending": 1, "unspecified": 136,
    }
    recorded = [cell for cell in report["cells"] if cell["status"] == "COMPARISON_RECORDED"]
    assert [(cell["field"], cell["locator"]) for cell in recorded] == [
        ("od", "Synthetic printed page A"),
    ]


def test_all_cells_need_individual_comparison_records(tmp_path):
    """Synthetic evidence for 23 * 6 = 138 supplied Table 14 fields.

    This tests coverage accounting only; no test fixture authenticates a PDF.
    """
    def update(data):
        for row in data["components"]:
            for field in iso_table14.UNITS:
                if field != "weight_per_length":
                    row[field].update(verification="verified",
                                      verification_locator="Synthetic printed page A")

    report = iso_table14.audit_table14(save_case(tmp_path, update))
    assert report["status"] == "COMPARISON_RECORDED"
    assert report["counts"] == {
        "total": 138, "recorded": 138, "pending": 0, "unspecified": 0,
    }
    assert all(cell["locator"] == "Synthetic printed page A" for cell in report["cells"])


def test_inherited_source_locator_does_not_verify_cells(tmp_path):
    """Issue #2629 requires individual comparison, beyond inherited metadata."""
    def update(data):
        for row in data["components"]:
            for field in iso_table14.UNITS:
                for key in ("verification", "verification_locator",
                            "verification_reason", "verification_issue"):
                    row[field].pop(key, None)

    report = iso_table14.audit_table14(save_case(tmp_path, update))
    assert report["status"] == "NOT_EVALUATED"
    assert report["counts"]["unspecified"] == report["counts"]["total"]
    assert all(cell["locator"] is None for cell in report["cells"])


@pytest.mark.parametrize("field,key,value", [
    ("od", "unit", "inch"),
    ("id", "source_key", "unresolved"),
    ("bevel_diameter", "verification", "unknown"),
    ("od", "verification", "verified"),
])
def test_audit_validates_before_reporting(tmp_path, field, key, value):
    """Issue #2621: reject relabelled units/sources before using the data."""
    def update(data):
        data["components"][0][field][key] = value

    with pytest.raises(BhaDatabaseError):
        iso_table14.audit_table14(save_case(tmp_path, update))


def test_audit_does_not_write_or_mutate_the_table():
    before = iso_table14.DEFAULT_PATH.read_bytes()
    report = iso_table14.audit_table14()
    report["cells"].clear()
    report["counts"]["total"] = -1
    assert iso_table14.audit_table14()["cells"]
    assert iso_table14.DEFAULT_PATH.read_bytes() == before


def test_audit_cli_produces_json_for_explicit_path(tmp_path):
    saved = save_case(tmp_path, lambda data: None)
    result = subprocess.run(
        [sys.executable, "-m", "digitalmodel.bha_database.iso_table14", str(saved)],
        capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stderr
    assert json.loads(result.stdout) == iso_table14.audit_table14(saved)


def test_audit_cli_reports_missing_data(tmp_path):
    result = subprocess.run(
        [sys.executable, "-m", "digitalmodel.bha_database.iso_table14",
         str(tmp_path / "absent.yml")],
        capture_output=True, text=True, check=False,
    )
    assert result.returncode != 0
    assert result.stdout == ""
    assert "Table 14 data missing" in result.stderr
    assert "Traceback" not in result.stderr


@pytest.mark.parametrize("options", [[], ["-O"]])
def test_audit_cli_rejects_bad_units_with_optimization(tmp_path, options):
    """Issue #2621 requires provenance and unit checks under optimized Python."""
    def update(data):
        data["components"][0]["od"]["unit"] = "inch"

    saved = save_case(tmp_path, update)
    result = subprocess.run(
        [sys.executable, *options, "-m", "digitalmodel.bha_database.iso_table14", str(saved)],
        capture_output=True, text=True, check=False,
    )
    assert result.returncode != 0
    assert result.stdout == ""
    assert "Table 14: od unit" in result.stderr
