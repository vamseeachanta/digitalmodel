# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Execute the documented example with external-only generated files."""

import json
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


def test_worked_example_runs_without_private_data(tmp_path):
    """Independent hand sum: ceil(25/10)*2+25*3+4*5+2*7+1.5*11=131.5."""
    script = ROOT / "examples/riser_bom/worked_example.py"
    completed = subprocess.run(
        [sys.executable, str(script), str(tmp_path / "external")],
        capture_output=True,
        text=True,
        check=True,
    )
    result = json.loads(completed.stdout)
    assert result["total"] == "131.5"
    assert result["data_kind"] == "synthetic"
    assert result["currency"] == "TEST"
    assert len(result["breakdown"]) == 5
    fixture = json.loads((tmp_path / "external/basis.json").read_text(encoding="utf-8"))
    assert [row["value"] for row in fixture["rates"]] == ["2", "3", "5", "7", "11"]
    assert all(
        row["citation"]["code_id"] == "SYNTHETIC-BOM" for row in fixture["rates"]
    )


def test_example_does_not_overwrite_existing_data(tmp_path):
    """Existing external data are preserved byte-for-byte on a second invocation."""
    external = tmp_path / "external"
    external.mkdir()
    target = external / "basis.json"
    original = b"existing data shall remain untouched"
    target.write_bytes(original)
    completed = subprocess.run(
        [
            sys.executable,
            str(ROOT / "examples/riser_bom/worked_example.py"),
            str(external),
        ],
        capture_output=True,
        text=True,
    )
    assert completed.returncode != 0
    assert target.read_bytes() == original


def test_synthetic_manifest_pins_public_data_bytes():
    """The example fixture and its invented source schedule have pinned SHA-256."""
    import hashlib

    example = ROOT / "examples/riser_bom"
    manifest = json.loads((example / "manifest.json").read_text(encoding="utf-8"))
    assert manifest["data_kind"] == "synthetic"
    assert manifest["dataset_id"] == "synthetic-riser-bom"
    assert {row["path"] for row in manifest["files"]} == {
        "synthetic_cost_basis.json",
        "wikis/engineering/concepts/synthetic-bom.md",
    }
    for row in manifest["files"]:
        assert (
            hashlib.sha256((example / row["path"]).read_bytes()).hexdigest()
            == row["sha256"]
        )
