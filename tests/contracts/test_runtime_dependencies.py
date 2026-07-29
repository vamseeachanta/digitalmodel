# ABOUTME: Runtime dependency contract (#1632) — [project] dependencies must be
# ABOUTME: exactly what shipped code imports at module level, nothing more.
"""Guard against re-poisoning the runtime dependency list.

The list in ``[project] dependencies`` was once produced by machine-pasting the
vendored dashboard's ``backend/requirements.txt`` into it. The result shipped
Celery, New Relic, Sentry, boto3 and Poetry as *runtime* dependencies of an
engineering-calculation library, while eleven packages the shipped code actually
imports (flask, jinja2, jsonschema, shapely, sqlalchemy, ...) were absent
entirely — so a clean ``pip install digitalmodel`` raised ImportError.

The rule this file enforces, in one sentence: **a runtime dependency is a
distribution that packaged code imports at module level without a guard.**

Two things deliberately do NOT count, and each has its own home:

  * loaded-not-imported tooling (every pytest plugin) -> ``[test]`` extra. An
    AST scan cannot see these, which is exactly how they survived in the runtime
    list for so long.
  * executables (black, ruff, twine, sphinx)          -> ``[dev]`` extra

There used to be a third exemption and a matching ``UNPACKAGED`` skip list: the
vendored ``visualization/orcaflex_dashboard/`` app had no ``__init__.py``, so
setuptools dropped it from the wheel and its imports could not justify a runtime
dependency. That tree was deleted in #1632 -- its nine importable analysis
modules were extracted to ``solvers/orcaflex/results_analysis/``, which IS
packaged and IS scanned like any other shipped code. Nothing under ``src/`` is
exempt from the scan any more.
"""
from __future__ import annotations

import ast
import re
import sys
import tomllib
from pathlib import Path

import pytest

pytestmark = pytest.mark.contracts

REPO = Path(__file__).resolve().parents[2]
SRC = REPO / "src" / "digitalmodel"
PYPROJECT = REPO / "pyproject.toml"

#: import name -> distribution name, where they differ. Only entries the scan
#: actually encounters need to be here; an unmapped name falls back to itself.
IMPORT_TO_DIST = {
    "bs4": "beautifulsoup4",
    "docx": "python-docx",
    "dateutil": "python-dateutil",
    "PIL": "pillow",
    "yaml": "pyyaml",
    "sklearn": "scikit-learn",
    "ruamel": "ruamel.yaml",
    "OpenGL": "pyopengl",
    "fitz": "pymupdf",
    "mpl_toolkits": "matplotlib",  # namespace package shipped BY matplotlib
}

#: Runtime dependencies that shipped code needs but never `import`s -- so no AST
#: scan can see them, and `test_no_runtime_dependency_is_unused` would call them
#: unused. Each entry MUST cite the call site that needs it.
#:
#: This category exists because #1632 removed `xlsxwriter` on exactly that
#: reasoning and broke Excel export: pandas resolves the writer by NAME, and a
#: dependency expressed as a string is invisible to an import scan.
#:
#: Before adding an entry, prefer making the dependency explicit in code. Add
#: here only when the library's own API is string-addressed (pandas engines,
#: matplotlib backends, SQLAlchemy dialects, entry-point plugins).
DYNAMIC_RUNTIME: dict[str, str] = {
    # pd.ExcelWriter(..., engine="xlsxwriter") at:
    #   asset_integrity/common/DataFrame_To_xlsx.py:12
    #   asset_integrity/common/data.py:454
    #   infrastructure/utils/data.py (same helper)
    "xlsxwriter": "pandas ExcelWriter engine, addressed by string not import",
    # xr.open_dataset(..., engine="h5netcdf") at
    #   data_systems/data_procurement/common/stream_handler.py:112
    "h5netcdf": "xarray open_dataset engine, selected by string not import",
    # pd.read_hdf(...) at
    #   signal_processing/signal_analysis/orcaflex/reader.py:248
    # pandas requires PyTables for HDF5; h5py is NOT a substitute.
    "tables": "pandas HDF5 backend (PyTables), required implicitly by read_hdf",
}

#: NOTE (#1924): this dict is hand-maintained, and that is a known weakness.
#: It held exactly ONE entry -- xlsxwriter -- until an independent review found
#: the two above, both of which broke clean installs. A list of invisible
#: dependencies curated from memory is a sample, not a set.
#:
#: The durable fix is a scan that DERIVES candidates from the known
#: string-addressing patterns rather than trusting an author to recall them:
#: engine= / backend= / driver= / dialect= / format= keyword arguments, the
#: pandas read_*/to_* family (hdf, parquet, excel, sql), matplotlib backends,
#: and importlib.metadata entry-point lookups. Until that exists, treat this
#: dict as incomplete rather than authoritative.

#: FROZEN 2026-07-29 (#1632). Module-level imports in shipped code that resolve to
#: nothing installable. Two kinds, both pre-existing and both out of scope for the
#: dependency un-merge -- enumerated here so they are visible debt rather than a
#: silently-weakened contract. Tracked in #1889.
#:
#:   (a) bare sibling names -- `import common`, `import results`. These are not
#:       packages at all; the modules raise ImportError today. A dependency
#:       declaration cannot fix them, only deleting or re-rooting the module can.
#:   (b) real PyPI packages that legacy corners import but that a calculation
#:       library must not pull at runtime (yfinance, pygame, bokeh, ...). Each
#:       needs the import made lazy plus an extra, or the module removed.
#:
#: This list may only SHRINK -- test_known_undeclared_list_has_no_stale_entries
#: fails if an entry stops being an offender, so fixing one forces deleting it.
KNOWN_UNDECLARED: frozenset[str] = frozenset({
    # (a) bare sibling names -- broken imports, not dependencies
    "ai-cad-agent", "apistd2rdmethods", "batch-converter", "common", "custom",
    "data-manager", "datamanager", "dataprovision", "enhanced-downloader",
    "excel-reader", "extractdata", "fatigue-plotraodirection", "filelist",
    "logs", "pilot-dashboard", "plotraodirection", "results", "scrapers",
    "services", "xlsx-to-dataframe",
    # (b) real packages, legacy corners only
    "bokeh", "exchangelib", "flask-cors", "flask-flatpages", "flask-httpauth",
    "finvizfinance", "flask-restful", "flask-wtf", "markitdown", "oyaml",
    "pandas-datareader",
    "pdfplumber", "pygame", "sec-edgar-downloader", "webdriver-manager",
    "wtforms", "yahoo-fin", "yfinance",
})

#: Imports that resolve to no PyPI distribution we control: licensed solver
#: bindings, OS-specific bindings, and first-party names.
NOT_A_DEPENDENCY = {
    "digitalmodel",
    "OrcFxAPI",  # licensed Orcina binding, never installable from PyPI
    "FreeCAD", "Part", "Mesh", "MeshPart", "Draft", "TechDraw",  # FreeCAD embed
    "win32api", "win32com", "win32con", "win32gui", "win32ui", "pythoncom",
    "java",
}


def _dist(mod: str) -> str:
    return IMPORT_TO_DIST.get(mod, mod).lower().replace("_", "-")


def _spec_name(spec: str) -> str:
    return re.split(r"[<>=!\[; ]", spec.strip())[0].lower().replace("_", "-")


def _import_names(node: ast.AST) -> set[str]:
    if isinstance(node, ast.Import):
        return {a.name.split(".")[0] for a in node.names}
    if isinstance(node, ast.ImportFrom) and node.level == 0 and node.module:
        return {node.module.split(".")[0]}
    return set()


def hard_imports() -> set[str]:
    """Distributions imported at MODULE level, outside try/except and outside any
    function or class body, by code that ships in the wheel.

    Only ``tree.body`` is examined, and that is sufficient: a module-level
    ``try: import x`` is an ``ast.Try`` node whose children are NOT direct
    children of ``tree.body``, and a lazy import inside a function is inside an
    ``ast.FunctionDef``. Both are therefore already excluded.

    An earlier version also subtracted a ``guarded`` set collected by walking
    every ``Try``/``FunctionDef`` in the file. That was worse than redundant: it
    CANCELLED a genuine module-level offender whenever the same name was also
    imported lazily somewhere else in the same file, producing false negatives.
    It hid two real offenders, one of them live and under test
    (``structural/pipe_capacity/common/PipeCapacity.py``, 1192 LOC, whose test
    carries a ``sys.path`` hack to work around the broken import).
    """
    out: set[str] = set()
    stdlib = set(sys.stdlib_module_names)
    for path in sorted(SRC.rglob("*.py")):
        try:
            tree = ast.parse(path.read_text(encoding="utf-8", errors="ignore"))
        except SyntaxError:  # pragma: no cover - reported by its own guard
            continue
        top: set[str] = set()
        for node in tree.body:
            top |= _import_names(node)
        for mod in top:
            if mod in stdlib or mod in NOT_A_DEPENDENCY or mod.startswith("_"):
                continue
            out.add(_dist(mod))
    return out


def declared_runtime() -> set[str]:
    doc = tomllib.loads(PYPROJECT.read_text(encoding="utf-8"))
    return {_spec_name(s) for s in doc["project"]["dependencies"]}


def test_dynamic_runtime_entries_are_declared_and_still_used():
    """Every DYNAMIC_RUNTIME entry must be a real runtime dependency AND still be
    referenced by name in shipped code.

    Without the second half, this dict becomes a blanket exemption: an entry
    whose call site was deleted would keep a dependency alive forever with the
    contract asserting nothing. Matching on the string literal is the only check
    available -- that is the whole point of the category.
    """
    runtime = declared_runtime()
    for dist, reason in sorted(DYNAMIC_RUNTIME.items()):
        assert dist in runtime, (
            f"{dist} is listed in DYNAMIC_RUNTIME ({reason}) but is not a runtime "
            "dependency -- either declare it or drop the entry (#1632)"
        )
        found = [
            p.relative_to(SRC).as_posix()
            for p in SRC.rglob("*.py")
            if dist in p.read_text(encoding="utf-8", errors="ignore")
        ]
        assert found, (
            f"{dist} is exempted as a dynamic runtime dependency ({reason}) but no "
            "shipped module mentions it by name any more. Delete the DYNAMIC_RUNTIME "
            "entry and the dependency together (#1632)."
        )


def test_no_runtime_dependency_is_unused():
    """Every runtime dependency is imported by shipped code.

    A failure here usually means a dependency belongs in an extra instead: the
    ``[test]`` extra for anything the test runner needs, ``[dev]`` for tooling.
    """
    unused = sorted(declared_runtime() - hard_imports() - set(DYNAMIC_RUNTIME))
    assert not unused, (
        "runtime dependencies that no shipped module imports at module level "
        f"({len(unused)}): {unused}\n"
        "Move them to [project.optional-dependencies] — do not leave them in "
        "[project] dependencies (#1632)."
    )


def test_every_hard_import_is_declared():
    """Nothing shipped imports a package we forgot to declare.

    This is the direction that actually breaks users: an undeclared hard import
    is an ImportError on a clean install. Eleven of these existed when #1632 was
    written.
    """
    missing = sorted(hard_imports() - declared_runtime() - KNOWN_UNDECLARED)
    assert not missing, (
        f"imported at module level by shipped code but NOT declared ({len(missing)}): "
        f"{missing}\nAdd them to [project] dependencies, or make the import lazy "
        "and declare an extra. Do NOT extend KNOWN_UNDECLARED — that list is "
        "frozen debt and may only shrink (#1632)."
    )


def test_known_undeclared_list_has_no_stale_entries():
    """The frozen debt list may only shrink.

    Fixing a broken import or removing a legacy module must also delete its entry
    here, so the list cannot quietly become a permanent exemption the way an
    unmaintained allowlist does.
    """
    stale = sorted(KNOWN_UNDECLARED - hard_imports())
    assert not stale, (
        f"KNOWN_UNDECLARED entries that are no longer imported ({len(stale)}): "
        f"{stale}\nDelete them from the list — it is a shrinking ratchet (#1632)."
    )


def test_dashboard_stack_is_not_a_runtime_dependency():
    """The specific regression #1632 describes, pinned by name.

    These are an application-server and observability stack. The app that needed
    them, and the ``[orcaflex-dashboard]`` extra that held them, are both gone
    (#1632) -- so nothing in this repo has any use for them. If any reappears in
    [project] dependencies, the requirements.txt paste has happened again.
    """
    poison = {
        "celery", "newrelic", "sentry-sdk", "boto3", "poetry", "gunicorn",
        "hypercorn", "alembic", "aiosqlite", "asyncio-mqtt", "fastapi-users",
        "fastapi-limiter", "fastapi-pagination", "slowapi", "prometheus-client",
    }
    found = sorted(poison & declared_runtime())
    assert not found, (
        f"application-server / observability packages back in the runtime list: "
        f"{found} — nothing in this repo imports them; the app that did was "
        "deleted in #1632"
    )


def test_pytest_plugins_are_not_runtime_dependencies():
    """Test tooling must not ship to library users.

    Called out separately because pytest plugins are LOADED, never imported, so
    the two scans above cannot see them: they would pass
    ``test_no_runtime_dependency_is_unused`` only by being absent, and nothing
    else would notice them creeping back in.
    """
    runtime = declared_runtime()
    found = sorted(
        d for d in runtime
        if d.startswith("pytest") or d in {"coverage", "hypothesis", "faker", "factory-boy"}
    )
    assert not found, (
        f"test-only packages in [project] dependencies: {found} — they belong in "
        "the [test] extra, which CI installs via --with-editable '.[test]' (#1632)"
    )
