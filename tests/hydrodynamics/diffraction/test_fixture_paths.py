# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Tracked fixtures must resolve relative to the repository, never a checkout."""

import importlib.util
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
SPEC = importlib.util.spec_from_file_location(
    "fixture_path_audit", ROOT / "scripts/testing/audit_fixture_paths.py",
)
AUDIT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(AUDIT)


def test_diffraction_has_no_checkout_literals():
    """All Python test helpers, including nested benchmarks, obey the path rule."""
    failures = []
    for path in Path(__file__).parent.rglob("*.py"):
        relative = path.relative_to(ROOT).as_posix()
        failures.extend((relative, line) for line in AUDIT.violations(
            path.read_text(encoding="utf-8-sig"), relative,
        ))
    assert failures == []


def test_scanner_accepts_relative_fixture():
    """A clean synthetic snippet uses a repository-relative fixture path."""
    assert AUDIT.violations('fixture = Path("tests/fixtures/example.yml")', "test.py") == []


@pytest.mark.parametrize("prefix", ["drive", "home", "mnt", "Users"])
def test_scanner_rejects_absolute_fixture(prefix):
    """Synthetic path syntax, assembled at runtime to avoid retaining locations."""
    location = ("C:" + "/") if prefix == "drive" else "/" + prefix + "/"
    source = f"fixture = Path({(location + 'example.yml')!r})"
    assert AUDIT.violations(source, "test.py") == [1]


def test_allowlist_does_not_exempt_other_paths():
    """Only the documented configuration value is exempt, even in that file."""
    relative, values = next(iter(AUDIT.ALLOWLIST.items()))
    value = next(iter(values))
    assert AUDIT.violations(f"value = {value!r}", relative) == []
    assert AUDIT.violations(f"value = {(value + '.other')!r}", relative) == [1]


def test_skip_guard_inventory_resolves_from_file(tmp_path):
    """A tracked-fixture skip is detected without importing the synthetic test."""
    file = tmp_path / "tests/test_example.py"
    source = '''from pathlib import Path
ROOT = Path(__file__).resolve().parents[1]
FIXTURE = ROOT / "fixtures/example.yml"
@pytest.mark.skipif(not FIXTURE.exists(), reason="absent")
def test_example(): pass
'''
    assert AUDIT.skip_guards(source, file, tmp_path) == [{
        "file": "tests/test_example.py", "line": 4, "path": "fixtures/example.yml",
    }]


@pytest.mark.parametrize("name", ["test_aqwa_parser.py", "test_cli_integration.py"])
def test_listing_path_is_independent_of_cwd(name, tmp_path, monkeypatch):
    """Both consumers must find the tracked listing from an unrelated directory."""
    import runpy

    monkeypatch.chdir(tmp_path)
    namespace = runpy.run_path(str(Path(__file__).parent / name))
    listing = namespace["TEST_DATA_DIR"] / "001_SHIP_RAOS.LIS"
    assert listing.is_relative_to(ROOT)
    assert listing.is_file()
