# ABOUTME: Derives string-addressed runtime dependencies from the source tree
# ABOUTME: instead of trusting a hand-curated list (#1924). Replaces recall with a scan.
"""String-addressed dependency contract (#1924).

One pattern has now produced **three** production defects in this repo, and each
was found by a human after the fact rather than by CI:

  * ``pd.ExcelWriter(..., engine="xlsxwriter")`` -- #1632 removed ``xlsxwriter``
    as "unused" and broke Excel export (#1906).
  * ``xr.open_dataset(..., engine="h5netcdf")`` -- ``h5netcdf`` undeclared (#1924).
  * ``pd.read_hdf(...)`` -- needs PyTables (``tables``), undeclared (#1924).

They share one shape: **the dependency is selected by a string, not by an
``import``.** ``test_runtime_dependencies.py`` scans imports, so it is blind to
all three by construction. Its answer was ``DYNAMIC_RUNTIME``, a dict of known
offenders -- and that dict held exactly ONE entry until an independent review
found two more. Its own comment concedes the point: *a list of invisible
dependencies curated from memory is a sample, not a set.*

This file replaces recall with derivation. It walks the AST of every shipped
module, matches four documented families of string-addressing, resolves each hit
to the distribution it actually requires through an explicit mapping table, and
asserts that distribution is either declared in ``[project] dependencies`` or
frozen in ``KNOWN_STRING_ADDRESSED`` with a reason.

The four families (each with its own mapping table below):

  (1) SELECTOR KEYWORDS -- ``engine=``/``backend=``/``driver=``/``dialect=``/
      ``writer=`` with a *string literal* value. Table: ``SELECTOR_DIST``.
  (2) IMPLICIT-PLUGIN CALLS -- functions that need a third-party backend even
      with no selector argument at all: ``pd.read_hdf`` needs PyTables, and says
      so nowhere in its call. Table: ``IMPLICIT_PLUGIN_DIST``.
  (3) DATABASE URLS -- ``"postgresql+psycopg2://..."`` and the ``URL.create(
      "postgresql+psycopg2", ...)`` spelling that carries no ``://`` at all. The
      dialect names the DBAPI driver, a separate distribution SQLAlchemy will not
      install for you. Table: ``DBAPI_DIST``.
  (4) MATPLOTLIB BACKENDS -- ``matplotlib.use("Agg")``. Table: ``MPL_BACKEND_DIST``.

The mapping table is the fragile part, and it is deliberately the *only* fragile
part: it is data, it is commented, and an unrecognised selector value FAILS
(``test_no_unrecognised_selector_values``) rather than passing silently. Adding a
new ``engine="..."`` the table has never seen breaks CI until someone writes down
which distribution it needs. That is the property the curated list never had.

VERIFIED MAPPINGS (checked against upstream docs, not assumed -- the #1924 review
found ``h5py`` declared while both real needs, ``h5netcdf`` and ``tables``, were
absent, so "an HDF5 package is installed" is not evidence):

  * pandas HDF5 (``read_hdf``/``to_hdf``/``HDFStore``) requires **PyTables**
    (dist ``tables``). ``h5py`` is NOT a substitute -- different file layout.
  * pandas Parquet accepts **pyarrow OR fastparquet**; either satisfies it.
  * pandas ``.xlsx`` reading is **openpyxl**; ``.xls`` reading is **xlrd**;
    writing is **openpyxl or xlsxwriter**. Four distributions, one API.
  * xarray's ``h5netcdf`` engine is the ``h5netcdf`` distribution, which is not
    ``netCDF4`` and not ``h5py``.
  * matplotlib's ``Agg``/``pdf``/``svg``/``ps``/``pgf`` backends ship *inside*
    matplotlib -- they are string-addressed but need no extra distribution, which
    is why the table maps a value to ``None`` rather than omitting it.

DELIBERATE LIMITS -- what this scan does NOT catch (stated plainly, because
faking completeness is the exact failure #1924 is about):

  a. NON-LITERAL SELECTORS. ``engine=cfg["engine"]`` or ``create_engine(url)``
     where ``url`` is built at runtime resolves to nothing an AST can see. The
     scan reads string constants and f-string prefixes only.
  b. RECEIVER-BLIND MATCHING. Family (2) matches on the *attribute name*, so a
     first-party ``def to_excel`` (``hydrodynamics/passing_ship/exporters.py:115``)
     registers as a pandas call. This direction is safe -- it over-requires a
     dependency that is already declared -- but it means a hit is not proof the
     pandas API is involved.
  c. EXTENSION-DISPATCHED PLUGINS. ``gdf.to_file(path)`` picks a Fiona/pyogrio
     driver from the file extension; PIL picks a codec the same way. No literal,
     no hit. Extension-based inference is left out on purpose -- it would need a
     data-flow analysis, and a wrong guess here is worse than a stated gap.

     This gap has one live consequence worth naming. ``pd.read_excel`` resolves
     to ``openpyxl`` for ``.xlsx`` and to ``xlrd`` for ``.xls``, and the call site
     says neither -- so this scan can only require *one* of them, while
     ``signal_processing/signal_analysis/orcaflex/reader.py:245`` and
     ``marine_ops/vessel_db/hulls_adapter.py:55`` both branch on ``.xls`` and
     genuinely need ``xlrd``. Today ``xlrd`` is also imported outright, so
     ``test_no_runtime_dependency_is_unused`` anchors it; if that import is ever
     removed, neither contract will object to dropping ``xlrd`` and ``.xls``
     reading breaks. Same shape as #1906, one indirection further out.
  d. NON-PYTHON DEPENDENCIES. ``imgkit`` needs the ``wkhtmltopdf`` binary and
     ``report_pack`` needs a Playwright-installed Chromium. Neither is a
     distribution in ``[project] dependencies``, so neither is in scope.
  e. ENTRY-POINT AND ``import_module`` LOOKUPS. Every such call in ``src/`` today
     resolves a *first-party* ``digitalmodel.*`` name (verified by hand: 6
     ``import_module`` sites, 4 ``__import__``, 5 ``find_spec``, 1
     ``entry_points``), so there is nothing third-party to derive. If one ever
     addresses an external plugin, this scan will not see it.
  f. ``method=``/``format=``/``compression=`` KEYWORDS. Scanned and rejected as a
     family: every occurrence in ``src/`` is an algorithm name
     (``griddata(method="linear")``), a log format string, or a codec bundled in
     the backend (``compression="snappy"`` is inside pyarrow). Including them
     would add ~150 hits and zero dependencies. Re-open if that changes.
"""
from __future__ import annotations

import ast
import re
import tomllib
from dataclasses import dataclass
from pathlib import Path

import pytest

pytestmark = pytest.mark.contracts

REPO = Path(__file__).resolve().parents[2]
SRC = REPO / "src" / "digitalmodel"
PYPROJECT = REPO / "pyproject.toml"


# --------------------------------------------------------------------------- #
# Family (1): selector keywords -- engine= / backend= / driver= / writer= ...
# --------------------------------------------------------------------------- #

#: Keyword arguments whose *string literal* value names an implementation.
#: Every literal a scan finds under one of these names MUST appear in
#: ``SELECTOR_DIST`` -- an unknown value is a failure, not a pass.
SELECTOR_KEYWORDS = frozenset({"engine", "backend", "driver", "dialect", "writer"})

#: ``(keyword, literal value)`` -> distribution required, or ``None`` when the
#: implementation ships inside a package that is already declared.
#:
#: Keyed on the pair rather than the value alone because the vocabularies
#: overlap: ``driver="GTiff"`` is a GDAL format name inside rasterio, while
#: ``driver`` in a database URL names a separate PyPI distribution.
SELECTOR_DIST: dict[tuple[str, str], str | None] = {
    # -- pandas Excel engines ------------------------------------------------
    ("engine", "xlsxwriter"): "xlsxwriter",
    ("engine", "openpyxl"): "openpyxl",
    ("engine", "xlrd"): "xlrd",
    ("engine", "odf"): "odfpy",
    ("engine", "pyxlsb"): "pyxlsb",
    ("engine", "calamine"): "python-calamine",
    # -- pandas Parquet / Feather / ORC engines ------------------------------
    ("engine", "pyarrow"): "pyarrow",
    ("engine", "fastparquet"): "fastparquet",
    # -- pandas CSV parser engines (all built into pandas) -------------------
    ("engine", "c"): None,
    ("engine", "python"): None,
    ("engine", "python-fwf"): None,
    # -- pandas eval/query engines -------------------------------------------
    ("engine", "numexpr"): "numexpr",
    ("engine", "numba"): "numba",
    # -- xarray open_dataset / to_netcdf engines -----------------------------
    ("engine", "h5netcdf"): "h5netcdf",
    ("engine", "netcdf4"): "netcdf4",
    ("engine", "scipy"): "scipy",
    ("engine", "zarr"): "zarr",
    ("engine", "cfgrib"): "cfgrib",
    ("engine", "pydap"): "pydap",
    ("engine", "rasterio"): "rasterio",
    # -- SQLAlchemy DBAPI drivers named as a bare `driver=` ------------------
    ("driver", "psycopg2"): "psycopg2",
    ("driver", "pyodbc"): "pyodbc",
    ("driver", "pymysql"): "pymysql",
    # -- rasterio / GDAL format drivers (inside rasterio, not separate) ------
    ("driver", "GTiff"): None,
    ("driver", "ESRI Shapefile"): None,
    ("driver", "GPKG"): None,
    ("driver", "GeoJSON"): None,
    # -- matplotlib animation writers ----------------------------------------
    ("writer", "pillow"): "pillow",
    ("writer", "html"): None,  # built into matplotlib
    ("writer", "ffmpeg"): None,  # external binary, not a distribution (limit d)
    ("writer", "imagemagick"): None,  # external binary (limit d)
}


# --------------------------------------------------------------------------- #
# Family (2): calls that need a backend with NO selector argument at all
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class PluginNeed:
    """One or more distributions, ANY of which satisfies the call."""

    dists: frozenset[str]
    why: str


#: Attribute name of the call -> what it implicitly requires. This is the family
#: that produced two of the three defects: nothing at the call site names the
#: dependency, so neither an import scan nor a reader spots it.
#:
#: Matching is by attribute name only -- see limit (b) in the module docstring.
IMPLICIT_PLUGIN_DIST: dict[str, PluginNeed] = {
    # pandas HDF5 -- PyTables, NOT h5py. This is the #1924 `reader.py:248` defect.
    "read_hdf": PluginNeed(frozenset({"tables"}), "pandas HDF5 read needs PyTables"),
    "to_hdf": PluginNeed(frozenset({"tables"}), "pandas HDF5 write needs PyTables"),
    "HDFStore": PluginNeed(frozenset({"tables"}), "pandas HDFStore needs PyTables"),
    # pandas columnar formats -- either engine satisfies pandas.
    "read_parquet": PluginNeed(
        frozenset({"pyarrow", "fastparquet"}), "pandas Parquet read"
    ),
    "to_parquet": PluginNeed(
        frozenset({"pyarrow", "fastparquet"}), "pandas Parquet write"
    ),
    "read_feather": PluginNeed(frozenset({"pyarrow"}), "pandas Feather read"),
    "to_feather": PluginNeed(frozenset({"pyarrow"}), "pandas Feather write"),
    "read_orc": PluginNeed(frozenset({"pyarrow"}), "pandas ORC read"),
    "to_orc": PluginNeed(frozenset({"pyarrow"}), "pandas ORC write"),
    # pandas Excel -- reading .xlsx is openpyxl, reading .xls is xlrd; the call
    # site does not say which, so any reader distribution satisfies the scan.
    "read_excel": PluginNeed(
        frozenset({"openpyxl", "xlrd", "odfpy", "pyxlsb", "python-calamine"}),
        "pandas Excel read",
    ),
    "ExcelFile": PluginNeed(
        frozenset({"openpyxl", "xlrd", "odfpy", "pyxlsb", "python-calamine"}),
        "pandas Excel read",
    ),
    "to_excel": PluginNeed(
        frozenset({"openpyxl", "xlsxwriter"}), "pandas Excel write"
    ),
    "ExcelWriter": PluginNeed(
        frozenset({"openpyxl", "xlsxwriter"}), "pandas Excel write"
    ),
    # pandas HTML/XML -- lxml, or html5lib+beautifulsoup4.
    "read_html": PluginNeed(
        frozenset({"lxml", "html5lib", "beautifulsoup4"}), "pandas HTML table parse"
    ),
    "read_xml": PluginNeed(frozenset({"lxml"}), "pandas XML read"),
    # pandas SQL -- SQLAlchemy is the connectable; the DRIVER comes from the URL
    # and is handled by family (3).
    "read_sql": PluginNeed(frozenset({"sqlalchemy"}), "pandas SQL read"),
    "read_sql_query": PluginNeed(frozenset({"sqlalchemy"}), "pandas SQL read"),
    "read_sql_table": PluginNeed(frozenset({"sqlalchemy"}), "pandas SQL read"),
    "to_sql": PluginNeed(frozenset({"sqlalchemy"}), "pandas SQL write"),
    # pandas statistical interchange formats.
    "read_spss": PluginNeed(frozenset({"pyreadstat"}), "pandas SPSS read"),
    "read_gbq": PluginNeed(frozenset({"pandas-gbq"}), "pandas BigQuery read"),
    "to_gbq": PluginNeed(frozenset({"pandas-gbq"}), "pandas BigQuery write"),
    # xarray -- default engine resolution, when no engine= is given.
    "open_dataset": PluginNeed(
        frozenset({"netcdf4", "h5netcdf", "scipy"}), "xarray dataset open"
    ),
    "open_mfdataset": PluginNeed(
        frozenset({"netcdf4", "h5netcdf", "scipy"}), "xarray multifile open"
    ),
    "to_netcdf": PluginNeed(
        frozenset({"netcdf4", "h5netcdf", "scipy"}), "xarray netCDF write"
    ),
    "open_zarr": PluginNeed(frozenset({"zarr"}), "xarray zarr open"),
    "to_zarr": PluginNeed(frozenset({"zarr"}), "xarray zarr write"),
    # plotly static export -- resolved by NAME inside plotly, never imported.
    "write_image": PluginNeed(frozenset({"kaleido"}), "plotly static image export"),
}


# --------------------------------------------------------------------------- #
# Family (3): database URLs -- the dialect names a driver distribution
# --------------------------------------------------------------------------- #

#: ``(dialect, driver or None)`` -> distribution SQLAlchemy needs but will never
#: install for you. ``None`` driver = SQLAlchemy's default DBAPI for that dialect.
DBAPI_DIST: dict[tuple[str, str | None], str | None] = {
    ("sqlite", None): None,  # DBAPI is stdlib sqlite3
    ("sqlite", "pysqlite"): None,
    ("postgresql", None): "psycopg2",  # SQLAlchemy's default postgres DBAPI
    ("postgresql", "psycopg2"): "psycopg2",
    ("postgresql", "psycopg"): "psycopg",
    ("postgresql", "asyncpg"): "asyncpg",
    ("mysql", None): "mysqlclient",
    ("mysql", "pymysql"): "pymysql",
    ("mysql", "mysqldb"): "mysqlclient",
    ("mssql", None): "pyodbc",
    ("mssql", "pyodbc"): "pyodbc",
    ("mssql", "pymssql"): "pymssql",
    ("access", "pyodbc"): "pyodbc",
    ("oracle", None): "cx-oracle",
    ("oracle", "cx_oracle"): "cx-oracle",
    ("oracle", "oracledb"): "oracledb",
    ("mongodb", None): "pymongo",  # not SQLAlchemy, same "URL names a driver" shape
    ("snowflake", None): "snowflake-sqlalchemy",
    ("duckdb", None): "duckdb-engine",
}

#: Schemes that look like a database URL but are not. Matched exactly so a new
#: unknown ``xxx://`` is reported rather than assumed harmless.
NON_DATABASE_SCHEMES = frozenset({
    "http", "https", "ws", "wss", "file", "ftp", "s3", "gs", "data", "mailto",
    "orcawave",   # first-party OrcaWave resource URI
    "markitdown", # first-party markitdown resource URI
})

#: ``dialect://`` or ``dialect+driver://`` at the head of a string literal.
_URL_RE = re.compile(r"(?<![\w.\-/])([a-zA-Z][a-zA-Z0-9_]*)(?:\+([a-zA-Z][a-zA-Z0-9_]*))?://")

#: SQLAlchemy's *other* spelling: ``URL.create("postgresql+psycopg2", username=...)``.
#: The drivername is a bare literal with no ``://``, so ``_URL_RE`` cannot see it --
#: and this repo uses that form at ``asset_integrity/common/database.py:158`` and
#: ``:192`` precisely because it escapes credentials safely. Missing it would have
#: made this contract look complete while blind to the safest-written call sites.
URL_CREATE_CALLS = frozenset({"URL.create", "sqlalchemy.engine.URL.create"})

#: ``dialect`` or ``dialect+driver`` as a whole string (the URL.create argument).
_DRIVERNAME_RE = re.compile(r"^([a-zA-Z][a-zA-Z0-9_]*)(?:\+([a-zA-Z][a-zA-Z0-9_]*))?$")


# --------------------------------------------------------------------------- #
# Family (4): matplotlib backends
# --------------------------------------------------------------------------- #

#: Lower-cased backend name -> distribution. ``None`` = ships inside matplotlib
#: (or, for ``tkagg``, inside the stdlib's tkinter).
MPL_BACKEND_DIST: dict[str, str | None] = {
    "agg": None, "cairo": None, "pdf": None, "pgf": None, "ps": None,
    "svg": None, "template": None, "nbagg": None, "notebook": None,
    "inline": None, "macosx": None, "tkagg": None, "tkcairo": None,
    "webagg": "tornado",
    "qtagg": "pyqt6", "qt5agg": "pyqt5", "qtcairo": "pyqt6", "qt5cairo": "pyqt5",
    "gtk3agg": "pygobject", "gtk4agg": "pygobject",
    "gtk3cairo": "pygobject", "gtk4cairo": "pygobject",
    "wxagg": "wxpython", "wx": "wxpython", "wxcairo": "wxpython",
}

#: Calls whose first positional string argument selects a matplotlib backend.
MPL_BACKEND_CALLS = frozenset({"matplotlib.use", "mpl.use", "switch_backend"})


# --------------------------------------------------------------------------- #
# The frozen allowlist -- a shrinking ratchet, exactly like KNOWN_UNDECLARED
# --------------------------------------------------------------------------- #

#: FROZEN 2026-07-29 (#1924). Distributions this scan derives as required that
#: are deliberately NOT in ``[project] dependencies``. Each entry states why.
#:
#: This list may only SHRINK -- ``test_string_addressed_allowlist_has_no_stale_entries``
#: fails once an entry stops being derived, so fixing a call site forces deleting
#: its entry. Do NOT add to it to make a new finding pass: a newly-introduced
#: string-addressed dependency should either be declared or the call site fixed.
KNOWN_STRING_ADDRESSED: dict[str, str] = {
    "pyodbc": (
        "mssql+pyodbc:// and access+pyodbc:// URLs in the legacy database corner "
        "(infrastructure/utils/database.py:176, infrastructure/core/database_legacy.py:115, "
        "infrastructure/persistence/database_legacy.py:115, asset_integrity/common/database.py:109, "
        "infrastructure/core/database_manager.py:210+241). A calculation library must "
        "not pull an ODBC driver that needs system headers to build -- these belong "
        "behind a [database] extra, same disposition as #1900 class (b)."
    ),
    "psycopg2": (
        "postgresql+psycopg2:// URLs at infrastructure/core/database_manager.py:189, "
        "infrastructure/persistence/database_manager.py:189, "
        "infrastructure/core/database_legacy.py:158+182, "
        "infrastructure/persistence/database_legacy.py:158+182, "
        "infrastructure/utils/database.py:219+243, plus the URL.create('postgresql+psycopg2') "
        "spelling at asset_integrity/common/database.py:158+192. "
        "Same disposition as pyodbc: needs a [database] extra, not a runtime dep."
    ),
    "pymongo": (
        "mongodb:// URLs at infrastructure/core/database_manager.py:228+230 and "
        "infrastructure/persistence/database_manager.py:228+230, paired with the "
        "lazy `from pymongo import MongoClient` in the *_legacy/database modules. "
        "Same [database] extra disposition."
    ),
    "kaleido": (
        "plotly resolves its static-image exporter by NAME, so fig.write_image() at "
        "asset_integrity/common/visualizations.py:281, "
        "infrastructure/utils/visualization/visualizations.py:340 and "
        "hydrodynamics/hull_library/rao_lookup_plots.py:311 needs kaleido with nothing "
        "importing it. Declared in the [viz] extra, which is the right home -- "
        "rao_lookup_plots.py documents the ImportError and names the package."
    ),
}


# --------------------------------------------------------------------------- #
# Scanner
# --------------------------------------------------------------------------- #

@dataclass(frozen=True)
class Requirement:
    """One derived requirement: a site, and the distributions that satisfy it."""

    where: str          # "path/to/mod.py:LINENO"
    call: str           # the syntax that triggered the hit
    dists: frozenset[str]  # ANY of these satisfies it; empty = no distribution needed
    why: str

    def satisfied_by(self, available: set[str]) -> bool:
        return not self.dists or bool(self.dists & available)


class UnknownSelector(Exception):
    """Raised for a selector literal absent from the mapping tables."""


def _dotted(func: ast.expr) -> str:
    parts: list[str] = []
    node = func
    while isinstance(node, ast.Attribute):
        parts.append(node.attr)
        node = node.value
    if isinstance(node, ast.Name):
        parts.append(node.id)
    return ".".join(reversed(parts))


def _string_literals(tree: ast.AST) -> list[tuple[int, str]]:
    """Every string constant, plus the leading constant chunk of every f-string.

    The f-string half matters: ``f"postgresql+psycopg2://{user}@{host}"`` is an
    ``ast.JoinedStr``, and its dialect prefix is exactly the part that names a
    driver. Six of the eight postgres sites in this repo are written that way.
    """
    out: list[tuple[int, str]] = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Constant) and isinstance(node.value, str):
            out.append((node.lineno, node.value))
        elif isinstance(node, ast.JoinedStr):
            for part in node.values:
                if isinstance(part, ast.Constant) and isinstance(part.value, str):
                    out.append((node.lineno, part.value))
    return out


def scan_tree(tree: ast.AST, where: str) -> tuple[list[Requirement], list[str]]:
    """Derive requirements from one parsed module.

    Returns ``(requirements, unknowns)``. ``unknowns`` holds selector literals the
    mapping tables do not recognise -- reported, never skipped.
    """
    reqs: list[Requirement] = []
    unknown: list[str] = []

    def need(lineno: int, call: str, dist: str | None, why: str) -> None:
        dists = frozenset() if dist is None else frozenset({dist})
        reqs.append(Requirement(f"{where}:{lineno}", call, dists, why))

    for node in ast.walk(tree):
        if isinstance(node, ast.Call):
            dotted = _dotted(node.func)
            attr = dotted.rsplit(".", 1)[-1] if dotted else ""

            # (1) selector keywords
            for kw in node.keywords:
                if kw.arg not in SELECTOR_KEYWORDS:
                    continue
                if not (isinstance(kw.value, ast.Constant)
                        and isinstance(kw.value.value, str)):
                    continue  # limit (a): non-literal selector
                key = (kw.arg, kw.value.value)
                if key not in SELECTOR_DIST:
                    unknown.append(
                        f"{where}:{node.lineno} {dotted}({kw.arg}={kw.value.value!r})"
                    )
                    continue
                need(node.lineno, f"{dotted}({kw.arg}={kw.value.value!r})",
                     SELECTOR_DIST[key], f"{kw.arg}= selector")

            # (2) implicit-plugin calls
            plugin = IMPLICIT_PLUGIN_DIST.get(attr)
            if plugin is not None:
                reqs.append(Requirement(
                    f"{where}:{node.lineno}", f"{dotted}(...)", plugin.dists, plugin.why
                ))

            # (3b) SQLAlchemy URL.create("dialect+driver", ...)
            if (dotted in URL_CREATE_CALLS or dotted.endswith(".URL.create")) and node.args:
                first = node.args[0]
                if isinstance(first, ast.Constant) and isinstance(first.value, str):
                    match = _DRIVERNAME_RE.match(first.value)
                    if match is None:
                        unknown.append(
                            f"{where}:{node.lineno} URL.create drivername {first.value!r}"
                        )
                    else:
                        key = (
                            match.group(1).lower(),
                            match.group(2).lower() if match.group(2) else None,
                        )
                        if key not in DBAPI_DIST:
                            unknown.append(
                                f"{where}:{node.lineno} URL.create drivername "
                                f"{first.value!r}"
                            )
                        else:
                            need(node.lineno, f"URL.create({first.value!r})",
                                 DBAPI_DIST[key], "SQLAlchemy drivername")

            # (4) matplotlib backends
            if (dotted in MPL_BACKEND_CALLS or attr == "switch_backend") and node.args:
                first = node.args[0]
                if isinstance(first, ast.Constant) and isinstance(first.value, str):
                    name = first.value.lower()
                    if name.startswith("module://"):
                        unknown.append(
                            f"{where}:{node.lineno} matplotlib backend {first.value!r} "
                            "(module:// backends are third-party by definition)"
                        )
                    elif name not in MPL_BACKEND_DIST:
                        unknown.append(
                            f"{where}:{node.lineno} matplotlib backend {first.value!r}"
                        )
                    else:
                        need(node.lineno, f"{dotted}({first.value!r})",
                             MPL_BACKEND_DIST[name], "matplotlib backend")

    # (3) database URLs, over string literals and f-string prefixes
    for lineno, text in _string_literals(tree):
        for match in _URL_RE.finditer(text):
            dialect, driver = match.group(1), match.group(2)
            if dialect.lower() in NON_DATABASE_SCHEMES and driver is None:
                continue
            key = (dialect.lower(), driver.lower() if driver else None)
            if key not in DBAPI_DIST:
                unknown.append(f"{where}:{lineno} database URL {match.group(0)!r}")
                continue
            need(lineno, f'"{match.group(0)}..."', DBAPI_DIST[key], "database URL dialect")

    return reqs, unknown


def _iter_modules():
    for path in sorted(SRC.rglob("*.py")):
        text = path.read_text(encoding="utf-8", errors="ignore")
        try:
            yield path.relative_to(SRC).as_posix(), ast.parse(text)
        except SyntaxError:  # guarded by its own contract elsewhere
            continue


def scan_source_tree() -> tuple[list[Requirement], list[str]]:
    reqs: list[Requirement] = []
    unknown: list[str] = []
    for where, tree in _iter_modules():
        r, u = scan_tree(tree, where)
        reqs.extend(r)
        unknown.extend(u)
    return reqs, unknown


def _spec_name(spec: str) -> str:
    return re.split(r"[<>=!\[; ]", spec.strip())[0].lower().replace("_", "-")


def declared_runtime() -> set[str]:
    doc = tomllib.loads(PYPROJECT.read_text(encoding="utf-8"))
    return {_spec_name(s) for s in doc["project"]["dependencies"]}


# --------------------------------------------------------------------------- #
# The contract
# --------------------------------------------------------------------------- #

def test_every_string_addressed_dependency_is_declared_or_frozen():
    """The whole point: a string-addressed dependency must be declared.

    This is the check that would have caught all three production defects --
    xlsxwriter (#1906), h5netcdf and PyTables (#1924) -- without anyone having to
    remember they existed.
    """
    reqs, _ = scan_source_tree()
    available = declared_runtime() | set(KNOWN_STRING_ADDRESSED)
    missing = [r for r in reqs if not r.satisfied_by(available)]
    if missing:
        lines = sorted(
            f"  {r.where}  {r.call}  needs {sorted(r.dists)}  ({r.why})"
            for r in missing
        )
        raise AssertionError(
            f"string-addressed dependencies that nothing declares ({len(missing)} "
            "call sites):\n" + "\n".join(lines)
            + "\n\nEach is an ImportError on a clean install even though every test "
            "passes -- the dependency is named by a STRING, so no import scan sees "
            "it (#1924). Add it to [project] dependencies (and to DYNAMIC_RUNTIME in "
            "test_runtime_dependencies.py so it is not later deleted as 'unused'), "
            "or make the call site name it explicitly. Do NOT extend "
            "KNOWN_STRING_ADDRESSED -- that list is frozen debt."
        )


def test_no_unrecognised_selector_values():
    """An ``engine=`` the mapping table has never seen FAILS.

    The curated list of #1924 failed by omission -- an unknown was simply absent
    and therefore silent. Here an unknown is loud: someone must write down which
    distribution the value needs before CI goes green, which is what keeps the
    mapping table honest as the source tree grows.
    """
    _, unknown = scan_source_tree()
    assert not unknown, (
        f"string selectors this contract cannot resolve ({len(unknown)}):\n  "
        + "\n  ".join(sorted(unknown))
        + "\n\nAdd each to SELECTOR_DIST / DBAPI_DIST / MPL_BACKEND_DIST with the "
        "distribution it requires, or None when the implementation ships inside a "
        "package already declared. Silence is not an option here -- that is how "
        "h5netcdf and PyTables shipped broken (#1924)."
    )


def test_string_addressed_allowlist_has_no_stale_entries():
    """The frozen allowlist may only shrink (repo ratchet convention).

    Mirrors ``test_known_undeclared_list_has_no_stale_entries``: once a call site
    is fixed or moved into [project] dependencies, its exemption must be deleted,
    so the list cannot decay into a permanent blanket.
    """
    reqs, _ = scan_source_tree()
    derived: set[str] = set()
    for r in reqs:
        derived |= r.dists
    declared = declared_runtime()
    stale = sorted(
        name for name in KNOWN_STRING_ADDRESSED
        if name not in derived or name in declared
    )
    assert not stale, (
        f"KNOWN_STRING_ADDRESSED entries that are no longer needed ({len(stale)}): "
        f"{stale}\nEither the call site is gone or the distribution is now declared. "
        "Delete the entry — this list is a shrinking ratchet (#1924)."
    )


def test_the_three_known_defects_are_still_derived():
    """Pin the three real defects by call site, so a refactor cannot lose them.

    Without this, deleting a scanner family would make the contract vacuously
    green — the same way ``DYNAMIC_RUNTIME`` was green while holding one entry.
    """
    reqs, _ = scan_source_tree()
    by_dist: dict[str, list[str]] = {}
    for r in reqs:
        for d in r.dists:
            by_dist.setdefault(d, []).append(r.where)

    assert any(
        w.startswith("asset_integrity/common/data.py")
        for w in by_dist.get("xlsxwriter", [])
    ), "engine='xlsxwriter' (#1906) no longer derived — the scanner regressed"
    assert any(
        w.startswith("data_systems/data_procurement/common/stream_handler.py")
        for w in by_dist.get("h5netcdf", [])
    ), "engine='h5netcdf' (#1924) no longer derived — the scanner regressed"
    assert any(
        w.startswith("signal_processing/signal_analysis/orcaflex/reader.py")
        for w in by_dist.get("tables", [])
    ), "pd.read_hdf -> PyTables (#1924) no longer derived — the scanner regressed"


def test_dynamic_runtime_is_a_subset_of_what_this_scan_derives():
    """The hand-curated dict must not claim anything the scan cannot see.

    The two files are deliberately redundant: ``DYNAMIC_RUNTIME`` stops a dynamic
    dependency being deleted as 'unused', this scan stops one being forgotten in
    the first place. If an entry there is invisible here, this scanner has a gap
    and the docstring's limits section needs to say so.
    """
    import importlib.util  # noqa: PLC0415

    sibling = Path(__file__).with_name("test_runtime_dependencies.py")
    spec = importlib.util.spec_from_file_location("_dm_runtime_deps_contract", sibling)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    DYNAMIC_RUNTIME = module.DYNAMIC_RUNTIME

    reqs, _ = scan_source_tree()
    derived: set[str] = set()
    for r in reqs:
        derived |= r.dists
    invisible = sorted(set(DYNAMIC_RUNTIME) - derived)
    assert not invisible, (
        f"DYNAMIC_RUNTIME entries this derived scan cannot see ({len(invisible)}): "
        f"{invisible}\nEither add the pattern to a mapping table here, or document "
        "it as a stated limit in this module's docstring. An undocumented gap is "
        "how #1924 happened."
    )


# --------------------------------------------------------------------------- #
# Self-tests: prove the scanner FAILS on a newly-added offender.
#
# These use inline ``ast.parse`` with no filesystem, in the style of
# ``test_force_units_boundary_contract.py``. Without them the contract could be
# broken (regexes that match nothing, a table lookup that swallows misses) and
# stay green forever — the exact failure mode #1924 describes.
# --------------------------------------------------------------------------- #

def _derived(src: str) -> tuple[set[str], list[str]]:
    reqs, unknown = scan_tree(ast.parse(src), "<self-test>")
    dists: set[str] = set()
    for r in reqs:
        dists |= r.dists
    return dists, unknown


@pytest.mark.parametrize(
    "snippet,expected",
    [
        # the three real defects, as a caller would newly write them
        ("pd.ExcelWriter(p, engine='xlsxwriter')", "xlsxwriter"),
        ("xr.open_dataset(b, engine='h5netcdf')", "h5netcdf"),
        ("df = pd.read_hdf(p)", "tables"),
        # the rest of each family
        ("df.to_parquet(p, engine='fastparquet')", "fastparquet"),
        ("pd.read_excel(p)", "openpyxl"),
        ("create_engine('postgresql+psycopg2://u:p@h/db')", "psycopg2"),
        ("create_engine(f'mssql+pyodbc://{h}/{d}')", "pyodbc"),
        ("create_engine(URL.create('postgresql+psycopg2', host=h))", "psycopg2"),
        ("matplotlib.use('Qt5Agg')", "pyqt5"),
        ("fig.write_image(p)", "kaleido"),
        ("anim.save(p, writer='pillow')", "pillow"),
    ],
)
def test_scanner_derives_a_new_offender(snippet: str, expected: str):
    """A freshly-written string-addressed call is caught by the scan."""
    dists, unknown = _derived(snippet)
    assert expected in dists, f"{snippet!r} should require {expected}; got {sorted(dists)}"
    assert not unknown, unknown


def test_a_new_undeclared_offender_fails_the_contract():
    """End-to-end proof: an offender whose distribution nothing declares FAILS.

    ``pyreadstat`` is not declared and not allowlisted, so a new
    ``pd.read_spss(...)`` anywhere in ``src/`` breaks the build — which is the
    behaviour the curated ``DYNAMIC_RUNTIME`` dict could never provide.
    """
    reqs, _ = scan_tree(ast.parse("df = pd.read_spss(path)"), "<self-test>")
    available = declared_runtime() | set(KNOWN_STRING_ADDRESSED)
    unsatisfied = [r for r in reqs if not r.satisfied_by(available)]
    assert unsatisfied, "read_spss should be unsatisfiable — pyreadstat is undeclared"
    assert unsatisfied[0].dists == frozenset({"pyreadstat"})


def test_scanner_flags_an_unrecognised_selector_rather_than_passing():
    """An unknown ``engine=`` must be reported, never silently ignored.

    This is the property that makes the mapping table maintainable: it cannot
    quietly go out of date, because going out of date is a test failure.
    """
    _, unknown = _derived("pd.read_something(p, engine='not_a_real_engine')")
    assert unknown and "not_a_real_engine" in unknown[0]

    _, unknown = _derived("matplotlib.use('module://my_custom_backend')")
    assert unknown and "module://" in unknown[0]

    _, unknown = _derived("create_engine('teradatasql+td://host/db')")
    assert unknown and "teradatasql" in unknown[0]

    _, unknown = _derived("URL.create('teradatasql+td', host=h)")
    assert unknown and "teradatasql" in unknown[0]


def test_scanner_accepts_builtin_implementations_without_a_distribution():
    """Values that need no distribution resolve to an empty requirement.

    ``Agg`` ships inside matplotlib and ``engine='python'`` inside pandas. Mapping
    them to ``None`` rather than omitting them is what lets the unknown-value test
    be strict: everything real is written down, including the zeroes.
    """
    for snippet in (
        "matplotlib.use('Agg')",
        "pd.read_csv(p, engine='python')",
        "rasterio.open(p, 'w', driver='GTiff')",
        "create_engine('sqlite:///:memory:')",
    ):
        dists, unknown = _derived(snippet)
        assert not unknown, f"{snippet!r} produced unknowns: {unknown}"
        assert not dists, f"{snippet!r} should need no distribution; got {sorted(dists)}"


def test_scanner_ignores_non_database_urls():
    """``https://`` and the first-party ``orcawave://`` URIs are not dialects."""
    _, unknown = _derived('URL = "https://example.com/x"\nQ = "orcawave://case/1"')
    assert not unknown


def test_non_literal_selectors_are_a_stated_limit_not_a_silent_pass():
    """Limit (a), pinned: a computed engine name yields nothing, and says so here.

    Written as a test so the gap is visible in the suite rather than only in prose
    — if someone later teaches the scanner to resolve simple constants, this test
    fails and the docstring gets updated with it.

    Note ``pd.ExcelWriter`` would still be caught, by family (2) on the call name.
    The gap only bites where the call name carries no information — a plain
    ``engine=`` on a function the plugin table does not know.
    """
    dists, unknown = _derived("backend.connect(engine=cfg['engine'])")
    assert not dists and not unknown

    # ...whereas the literal spelling of the same call IS resolved.
    dists, unknown = _derived("backend.connect(engine='fastparquet')")
    assert dists == {"fastparquet"} and not unknown
