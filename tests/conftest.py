# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Pytest configuration for marine engineering tests."""
import sys
import importlib.util
from pathlib import Path
from unittest.mock import MagicMock

import pytest


@pytest.fixture
def store_file():
    """Return a fetch helper that skips only when store data is unavailable."""
    root = Path(__file__).resolve().parent.parent
    spec = importlib.util.spec_from_file_location("store_fetch", root / "scripts/data/fetch.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)

    def fetch(store_id):
        reason = (f"store_data: {store_id} not available (store not reachable); "
                  f"run scripts/data/fetch.py {store_id}")
        if not module.store_configured():
            pytest.skip(reason)
        try:
            rows = module.select_rows(module.read_manifest(root), store_id)
            return module.fetch_row(root, rows[0])
        except module.StoreUnavailable:
            pytest.skip(reason)

    return fetch


@pytest.fixture(autouse=True)
def store_data_marker(request):
    """Fetch the optional store identifier attached to a store_data marker."""
    marker = request.node.get_closest_marker("store_data")
    if marker and marker.args:
        request.getfixturevalue("store_file")(marker.args[0])
    elif marker:
        pytest.skip(
            "store_data: <id> not available (store not reachable); "
            "run scripts/data/fetch.py <id>"
        )

# Tests that fail at collection due to missing modules, deleted files, or
# platform-specific imports. Excluded so the rest of the suite can run.
_tests_dir = Path(__file__).parent
collect_ignore = [
    # Missing local modules (standalone scripts, not real tests)
    str(_tests_dir / "solvers/orcaflex/examples_integration/test_converter.py"),
    str(_tests_dir / "solvers/orcaflex/examples_integration/test_single_download.py"),
    str(_tests_dir / "structural/fatigue_analysis/test_reference_seastate_scaling.py"),
    str(_tests_dir / "visualization/design_tools/pilot_program/test_case_1_separator.py"),
    # Missing src modules (not yet implemented)
    str(_tests_dir / "solvers/orcaflex/test_orcaflex_unit.py"),
    str(_tests_dir / "structural/fatigue_apps/test_load_scaling.py"),
    str(_tests_dir / "subsea/pipeline/test_on_bottom_stability.py"),
    str(_tests_dir / "test_plate_capacity.py"),
    # Reasons below were measured, not assumed (#1923). None of these four is
    # blocked by a platform or a missing dependency any more:
    #   - the eager `win32com` import that used to make the whole
    #     digitalmodel.workflows.mcp_server.orcawave package unimportable off
    #     Windows is now lazy, so these modules import fine on Linux;
    #   - test_end_to_end.py no longer pip-installs at collection time; its
    #     `aiohttp` need is declared in the [test] extra.
    # What is left is stale-path rot, and each entry names its own.
    #
    # Collects and runs; 4 of 9 fail on @patch targets 'api.orcawave_com...' and
    # 'core.mcp_server...' -- top-level module names that stopped existing when
    # the tree moved under digitalmodel.workflows.mcp_server. The one genuinely
    # Windows-only test (live COM connect) self-skips.
    str(_tests_dir / "workflows/orcawave/test_com_connection.py"),
    # Collects; 3 of 11 fail importing `src.mcp.orcawave.*` inside test bodies.
    # `src/mcp/` does not exist, and client/cli_client.py is gone entirely.
    str(_tests_dir / "workflows/orcawave/test_end_to_end.py"),
    # Fails at import: `from src.mcp.orcawave.core.integrated_server import ...`
    # -- same removed tree, but at module level so it kills collection.
    str(_tests_dir / "workflows/orcawave/test_integration.py"),
    # `markitdown` is a real PyPI distribution that no extra installs. Until
    # #1923 this entry hid a different bug: tests/workflows/standalone/ had no
    # __init__.py, so the test package WAS `markitdown` and shadowed the real
    # distribution. That is fixed; the honest blocker is the undeclared
    # dependency, plus 3 tests patching 'markitdown.MarkItDown' rather than the
    # name converter.py bound at import.
    str(_tests_dir / "workflows/standalone/markitdown/test_converter.py"),
    # Data file dependencies not in git — hull profiles (WRK-110 undelivered, #1923)
    str(_tests_dir / "hydrodynamics/hull_library/test_hull_library_expansion.py"),
]

# Add src/ to Python path
repo_root = Path(__file__).parent.parent
src_path = repo_root / "src"
if str(src_path) not in sys.path:
    sys.path.insert(0, str(src_path))

assetutilities_path = (repo_root.parent / "assetutilities" / "src").resolve()
if assetutilities_path.exists() and str(assetutilities_path) not in sys.path:
    sys.path.insert(0, str(assetutilities_path))

# Add aceengineercode to path for 'common' module imports
aceengineercode_path = (repo_root.parent / "aceengineercode").resolve()
if aceengineercode_path.exists() and str(aceengineercode_path) not in sys.path:
    sys.path.insert(0, str(aceengineercode_path))


# Mock catenary_riser_summary to prevent module-level code execution during test patching
# The module has module-level code that runs on import and requires config files
def _create_mock_catenary_riser_summary():
    """Create a mock module for catenary_riser_summary."""
    mock_module = MagicMock()
    mock_module.plt = MagicMock()
    mock_module.logging = MagicMock()
    mock_module.CompareTools = MagicMock()
    mock_module.application_configuration = MagicMock(return_value={
        "default": {"config": {"overwrite": {"output": False}}},
        "Analysis": {"file_name": "test", "result_folder": "/tmp/"},
        "ymlFiles": [],
        "plot": {"settings": None}
    })
    return mock_module


# Pre-register the mock module before tests attempt to import via @patch decorators
# This prevents the actual module-level code from running
_mock_catenary_riser_summary = _create_mock_catenary_riser_summary()

# Import the real digitalmodel package first to populate sys.modules properly
# Then only mock the specific module that has problematic module-level code
try:
    import digitalmodel
    import digitalmodel.modules
    import digitalmodel.catenary
except ImportError:
    pass  # Package not installed, tests will fail with clear import errors

# Only register the mock for the specific problematic module
if 'digitalmodel.subsea.catenary_riser.legacy.catenary_riser_summary' not in sys.modules:
    sys.modules['digitalmodel.subsea.catenary_riser.legacy.catenary_riser_summary'] = _mock_catenary_riser_summary


# Quarantine: tests that already fail on main, listed with a reason in
# tests/quarantine.txt and tracked by one GitHub issue. Each listed test still
# runs; it is marked xfail(strict=False), so a failure does not fail the gate and
# a pass is reported as XPASS -- the signal to delete its line. A node id without
# a parameter suffix covers every parametrization of that test; a bare file
# path covers every test in that file.
_QUARANTINE_FILE = Path(__file__).parent / "quarantine.txt"


def _read_quarantine(path: Path = _QUARANTINE_FILE) -> dict[str, str]:
    entries: dict[str, str] = {}
    if not path.exists():
        return entries
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        node_id, _, reason = line.partition(" | ")
        entries[node_id.strip()] = reason.strip() or "quarantined: failing on main"
    return entries


def pytest_collection_modifyitems(config, items):
    quarantine = _read_quarantine()
    if not quarantine:
        return
    for item in items:
        node_id = item.nodeid
        reason = (quarantine.get(node_id)
                  or quarantine.get(node_id.split("[", 1)[0])
                  or quarantine.get(node_id.split("::", 1)[0]))
        if reason:
            item.add_marker(pytest.mark.xfail(reason=f"quarantine: {reason}", strict=False))
