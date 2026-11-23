"""Defect 2 of workspace-hub#3838 -- the OrcaFlex solver version is unpinned.

``OrcFxAPI`` binds to the highest-numbered installed OrcaFlex unless
``OrcFxAPIConfig.setLibPath()`` is called BEFORE ``import OrcFxAPI``. An OrcaFlex
upgrade therefore changes the solver under every campaign with no record of it.

The remedy under test is the facade ``digitalmodel.solvers.orcaflex.orcaflex_api``:
it owns the import, selects the library when a version is requested, and records
what was actually resolved so a run manifest can carry it.

Import state is process-global and cannot be reset inside a pytest session -- a
single earlier ``import OrcFxAPI`` anywhere in the session would decide the
outcome of every case below. Every case that touches ``configure()`` therefore
runs in a fresh subprocess.
"""

from __future__ import annotations

import ast
import importlib.util
import json
import subprocess
import sys
import textwrap
from pathlib import Path

import pytest

REPO_ROOT = Path(__file__).resolve().parents[3]
ORCAFLEX_PKG = REPO_ROOT / "src" / "digitalmodel" / "solvers" / "orcaflex"

ORCFXAPI_INSTALLED = importlib.util.find_spec("OrcFxAPI") is not None
requires_orcfxapi = pytest.mark.skipif(
    not ORCFXAPI_INSTALLED, reason="OrcFxAPI is not installed on this host"
)


def _probe(code: str) -> dict:
    """Run ``code`` in a fresh interpreter and return its RESULT payload.

    The snippet must print exactly one line beginning ``RESULT:`` followed by a
    JSON object. A non-zero exit or a missing marker fails the calling test with
    the full child output attached, so a broken snippet is never read as a pass.
    """
    proc = subprocess.run(
        [sys.executable, "-c", textwrap.dedent(code)],
        capture_output=True,
        text=True,
        timeout=300,
    )
    detail = f"\n--- stdout ---\n{proc.stdout}\n--- stderr ---\n{proc.stderr}"
    assert proc.returncode == 0, f"subprocess exited {proc.returncode}{detail}"
    markers = [ln for ln in proc.stdout.splitlines() if ln.startswith("RESULT:")]
    assert len(markers) == 1, f"expected exactly one RESULT line{detail}"
    return json.loads(markers[0][len("RESULT:") :])


# ---------------------------------------------------------------------------
# Test 8 -- version requested and available
# ---------------------------------------------------------------------------


@pytest.mark.solver
@requires_orcfxapi
def test_requested_version_is_selected_and_recorded():
    out = _probe(
        """
        import json, os
        from digitalmodel.solvers.orcaflex import orcaflex_api

        versions = orcaflex_api.installed_versions()
        if not versions:
            print("RESULT:" + json.dumps({"skip": "no versioned OrcaFlex install"}))
        else:
            wanted = sorted(versions)[-1]
            expected = os.path.normcase(os.path.abspath(versions[wanted]))
            record = orcaflex_api.configure(wanted)
            record["resolved_lib_path"] = os.path.normcase(
                os.path.abspath(record["resolved_lib_path"])
            )
            print("RESULT:" + json.dumps(
                {"record": record, "wanted": wanted, "expected": expected}
            ))
        """
    )
    if "skip" in out:
        pytest.skip(out["skip"])

    record = out["record"]
    assert record["requested"] == out["wanted"]
    assert record["resolved_lib_path"] == out["expected"]
    assert record["resolved_version"], "resolved_version must be populated"


# ---------------------------------------------------------------------------
# Test 9 -- no version requested
# ---------------------------------------------------------------------------


@pytest.mark.solver
@requires_orcfxapi
def test_record_is_complete_when_no_version_is_requested():
    out = _probe(
        """
        import json
        from digitalmodel.solvers.orcaflex import orcaflex_api
        print("RESULT:" + json.dumps({"record": orcaflex_api.configure()}))
        """
    )
    record = out["record"]
    assert record["requested"] is None
    assert record["resolved_lib_path"], "resolved_lib_path must be recorded"
    assert record["resolved_version"], "resolved_version must be recorded"


# ---------------------------------------------------------------------------
# Test 10 -- version requested but absent
# ---------------------------------------------------------------------------


def test_absent_version_raises_naming_the_requested_version():
    out = _probe(
        """
        import json
        from digitalmodel.solvers.orcaflex import orcaflex_api
        try:
            orcaflex_api.configure("99.99-not-installed")
        except Exception as exc:
            print("RESULT:" + json.dumps(
                {"type": type(exc).__name__, "message": str(exc)}
            ))
        else:
            print("RESULT:" + json.dumps({"type": None, "message": ""}))
        """
    )
    assert out["type"] == "OrcaFlexVersionUnavailableError", out
    assert "99.99-not-installed" in out["message"]


# ---------------------------------------------------------------------------
# Test 11 -- configure() after OrcFxAPI is already imported
# ---------------------------------------------------------------------------


def test_configure_after_import_raises_naming_the_earlier_importer():
    """A stand-in module is registered as ``OrcFxAPI`` so this case needs no
    OrcaFlex install and no licence; what is under test is the ordering check,
    not the binding."""
    out = _probe(
        """
        import json, sys, types

        # Import the facade FIRST so the package initialisation is not the thing
        # that trips the check -- the case under test is a LATER importer.
        from digitalmodel.solvers.orcaflex import orcaflex_api

        stand_in = types.ModuleType("OrcFxAPI")
        sys.modules["OrcFxAPI"] = stand_in
        earlier = types.ModuleType("campaign_module_that_imported_first")
        earlier.OrcFxAPI = stand_in
        sys.modules["campaign_module_that_imported_first"] = earlier

        try:
            orcaflex_api.configure("11.6")
        except Exception as exc:
            print("RESULT:" + json.dumps(
                {"type": type(exc).__name__, "message": str(exc)}
            ))
        else:
            print("RESULT:" + json.dumps({"type": None, "message": ""}))
        """
    )
    assert out["type"] == "OrcFxAPIAlreadyImportedError", out
    assert "campaign_module_that_imported_first" in out["message"]


# ---------------------------------------------------------------------------
# Test 12 -- the selected path survives a later module-scope importer
# ---------------------------------------------------------------------------


@pytest.mark.solver
@requires_orcfxapi
def test_selected_lib_path_is_in_force_for_a_later_module_scope_importer():
    out = _probe(
        """
        import json, os
        from digitalmodel.solvers.orcaflex import orcaflex_api

        versions = orcaflex_api.installed_versions()
        if not versions:
            print("RESULT:" + json.dumps({"skip": "no versioned OrcaFlex install"}))
        else:
            wanted = sorted(versions)[-1]
            record = orcaflex_api.configure(wanted)

            # A module that imports OrcFxAPI at module scope, imported AFTER the
            # facade configured the library path.
            from digitalmodel.solvers.orcaflex import run_to_sim  # noqa: F401

            import OrcFxAPIConfig
            print("RESULT:" + json.dumps({
                "selected": os.path.normcase(os.path.abspath(versions[wanted])),
                "in_force": os.path.normcase(
                    os.path.abspath(OrcFxAPIConfig.getLibPath())
                ),
                "recorded": os.path.normcase(
                    os.path.abspath(record["resolved_lib_path"])
                ),
            }))
        """
    )
    if "skip" in out:
        pytest.skip(out["skip"])
    assert out["in_force"] == out["selected"]
    assert out["recorded"] == out["selected"]


# ---------------------------------------------------------------------------
# The ban that makes selection possible at all
# ---------------------------------------------------------------------------


def test_importing_the_orcaflex_package_does_not_import_orcfxapi():
    """The facade can only select a library while ``OrcFxAPI`` is unimported.

    Any module-scope ``import OrcFxAPI`` reachable from the package
    initialisation defeats ``setLibPath()`` for the life of the process, so
    ``import digitalmodel.solvers.orcaflex`` must not pull the binding in.
    """
    out = _probe(
        """
        import json, sys
        import digitalmodel.solvers.orcaflex  # noqa: F401

        binding = sys.modules.get("OrcFxAPI")
        holders = []
        if binding is not None:
            for name, module in list(sys.modules.items()):
                if module is None or module is binding:
                    continue
                namespace = getattr(module, "__dict__", None)
                if not isinstance(namespace, dict):
                    continue
                if any(value is binding for value in list(namespace.values())):
                    holders.append(name)
        print("RESULT:" + json.dumps(
            {"imported": binding is not None, "holders": sorted(holders)}
        ))
        """
    )
    assert out["imported"] is False, (
        "importing the orcaflex package imported OrcFxAPI at module scope via: "
        f"{out['holders']}"
    )


def test_importing_the_facade_does_not_import_orcfxapi():
    out = _probe(
        """
        import json, sys
        from digitalmodel.solvers.orcaflex import orcaflex_api  # noqa: F401
        print("RESULT:" + json.dumps({"imported": "OrcFxAPI" in sys.modules}))
        """
    )
    assert out["imported"] is False


# Modules under solvers/orcaflex that still import OrcFxAPI at module scope.
# The set is a ratchet, not a licence: it may shrink, never grow. Each entry
# names a module that is NOT reachable from the package initialisation, so none
# of them can defeat setLibPath() before the facade runs -- but any new
# module-scope import could be, so a new one must be justified by editing this
# list deliberately.
KNOWN_MODULE_SCOPE_IMPORTERS = {
    "OrcaFlexAnalysis.py",
    "all_vars.py",
    "comprehensive_benchmark.py",
    "orcaflex_custom_analysis.py",
    "orcaflex_iterative_runs.py",
    "orcaflex_modal_analysis.py",
    "orcaflex_optimized_parallel.py",
    "orcaflex_optimized_parallel_v2.py",
    "orcaflex_parallel_analysis.py",
    "orcaflex_yml_converter.py",
    "opp_time_series_v2.py",
    "post_results/postProcess.py",
    "reporting/extractors/aggregator.py",
    "reporting/extractors/boundary_conditions_extractor.py",
    "reporting/extractors/geometry_extractor.py",
    "reporting/extractors/loads_extractor.py",
    "reporting/extractors/materials_extractor.py",
    "reporting/extractors/mesh_extractor.py",
    "reporting/extractors/results_extractor.py",
}

_FUNCTION_NODES = (ast.FunctionDef, ast.AsyncFunctionDef, ast.Lambda)


def _imports_orcfxapi_at_module_scope(tree: ast.Module) -> bool:
    """Whether the module body executes ``import OrcFxAPI`` at import time.

    A ``try:``/``if:``/``with:`` wrapper is still module scope -- the import
    still runs when the module is imported. Only a function body defers it.
    """
    stack = list(tree.body)
    while stack:
        node = stack.pop()
        if isinstance(node, _FUNCTION_NODES):
            continue
        if isinstance(node, ast.Import):
            if any(alias.name.split(".")[0] == "OrcFxAPI" for alias in node.names):
                return True
        elif isinstance(node, ast.ImportFrom):
            if (node.module or "").split(".")[0] == "OrcFxAPI":
                return True
        stack.extend(ast.iter_child_nodes(node))
    return False


def test_module_scope_orcfxapi_imports_do_not_grow():
    found = set()
    for path in sorted(ORCAFLEX_PKG.rglob("*.py")):
        source = path.read_text(encoding="utf-8", errors="replace")
        if "OrcFxAPI" not in source:
            continue
        if _imports_orcfxapi_at_module_scope(ast.parse(source)):
            found.add(path.relative_to(ORCAFLEX_PKG).as_posix())

    new = found - KNOWN_MODULE_SCOPE_IMPORTERS
    assert not new, (
        "new module-scope `import OrcFxAPI` under solvers/orcaflex: "
        f"{sorted(new)} -- route access through orcaflex_api.api() instead"
    )
