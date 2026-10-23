"""Worked-example oracle: hashlib directly hashes the literal input bytes."""
import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import hashlib

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/provenance/fingerprint_example.py"


def test_worked_example_matches_independent_hashes_and_document(tmp_path):
    """N(b'alpha\\r\\nbeta\\r\\n') = b'alpha\\nbeta\\n'.

    Before: H(LF bytes) and H(CRLF bytes). After: both H(LF bytes).
    H is Python hashlib.sha256; the test constructs each byte stream directly.
    No expected value comes from the normalization function or example script.
    """
    spec = importlib.util.spec_from_file_location("fingerprint_example", SCRIPT)
    example = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(example)
    expected = {
        "lf_sha256_before": hashlib.sha256(b"alpha\nbeta\n").hexdigest(),
        "crlf_sha256_before": hashlib.sha256(b"alpha\r\nbeta\r\n").hexdigest(),
        "sha256_after": hashlib.sha256(b"alpha\nbeta\n").hexdigest(),
    }
    assert example.build_example(tmp_path) == expected
    document = (ROOT / "docs/parametric/provenance-fingerprints.md").read_text(encoding="utf-8")
    for value in expected.values():
        assert f"`{value}`" in document


def test_example_cli(tmp_path):
    result = subprocess.check_output(
        [sys.executable, str(SCRIPT), "--work-dir", str(tmp_path)], text=True,
        env={**__import__("os").environ, "PYTHONPATH": str(ROOT / "src")},
    )
    assert json.loads(result)["sha256_after"] == hashlib.sha256(b"alpha\nbeta\n").hexdigest()


def test_document_inventory_counts_match_committed_report():
    report = json.loads((ROOT / "docs/parametric/atlas-manifest-inventory.json").read_text())
    document = (ROOT / "docs/parametric/provenance-fingerprints.md").read_text()
    assert sum(report["counts"].values()) == len(report["manifests"])
    for status, count in report["counts"].items():
        assert f"| {status} | {count} |" in document
    assert report["inventory_revision"] in document
