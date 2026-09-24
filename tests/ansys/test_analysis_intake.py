"""Intake reconstruction tests; no external host or solver required."""
import hashlib
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_intake import resolve_native, build_intake_package
from digitalmodel.ansys.analysis_intake import _make_case


@pytest.mark.parametrize("name", ["../outside", "a/../../outside", "/absolute", "C:/secret",
                                  "a\\outside"])
def test_native_resolver_refuses_path_escape(tmp_path, name):
    with pytest.raises(ValueError):
        resolve_native(tmp_path, "retained-native/" + name)


def test_no_eligible_real_capture_is_a_blocked_import(tmp_path):
    ledger = {"baseline_commit": "frozen", "candidates": [
        {"candidate_id": "missing", "disposition": "deferred"}]}
    with pytest.raises(ValueError, match="eligible"):
        build_intake_package(ledger, tmp_path, tmp_path, tmp_path / "cache")


def test_native_resolver_does_not_follow_outside_symlink(tmp_path):
    root = tmp_path / "root"
    root.mkdir()
    outside = tmp_path / "outside"
    outside.write_text("outside")
    try:
        (root / "link").symlink_to(outside)
    except OSError:
        pytest.skip("symlink creation unavailable")
    with pytest.raises(ValueError):
        resolve_native(root, "retained-native/link")


def test_intake_verifies_the_bytes_that_are_parsed(tmp_path):
    source = tmp_path / "capture.csv"
    source.write_bytes(b"stress,99")
    ref = {"id": "capture", "sha256": hashlib.sha256(b"stress,12").hexdigest(),
           "role": "output", "required": True}
    candidate = {"candidate_id": "ansys-fixture", "family": "test",
        "model_basis": {"parameters": {"x": "1"}, "load": "fixture", "idealization": "fixture"},
        "disposition_reason": "fixture"}
    with pytest.raises(ValueError, match="changed"):
        _make_case(candidate, [ref], {"stress": "MPa"}, "capture",
                   {"capture": source}, "2026-09-13T00:00:00Z")
