"""Owner decision C11, review finding 2: a redacted path must not become an
executable placeholder.

The C11 cleanup replaced private absolute paths with the neutral text
``<private-data>``. Where that text sat in documentation it is harmless. Where
it sat in code -- a connection string, ``open()``, ``read_csv``, a default
argument, a ``sys.path`` entry, a batch command -- the program now tries to
open a path that cannot exist, and on Windows cannot even be named.

The rule these tests hold the tree to: a private location is configured by the
caller (an argument or a config value) or by an environment variable, and a
program that has neither says so clearly rather than opening the placeholder.
"""

from __future__ import annotations

import ast
import importlib
import subprocess
from pathlib import Path

import pytest

REPO = Path(__file__).resolve().parents[2]
PLACEHOLDER = "<private-" + "data>"

#: Files that name the placeholder on purpose: the gate's own tests, where it
#: is the replacement text that must pass, and the helper that refuses it.
ALLOWED_PY = {
    "tests/legal/test_check_identifiers_c11.py",
    "tests/legal/test_c11_private_paths.py",
    "src/digitalmodel/infrastructure/utils/private_paths.py",
}

SCRIPT_COMMENT = {
    ".bat": ("rem ", "rem\t", "::", "@rem "),
    ".cmd": ("rem ", "rem\t", "::", "@rem "),
    ".ps1": ("#",),
    ".sh": ("#",),
}


def _tracked(*patterns: str) -> list[str]:
    out = subprocess.run(
        ["git", "ls-files", "-z", "--", *patterns],
        cwd=REPO,
        capture_output=True,
        check=True,
    )
    return [p for p in out.stdout.decode("utf-8", "surrogateescape").split("\0") if p]


def _docstring_nodes(tree: ast.AST) -> set[int]:
    ids = set()
    for node in ast.walk(tree):
        body = getattr(node, "body", None)
        if isinstance(body, list):
            # Any bare string statement is documentation, not a value.
            for stmt in body:
                if isinstance(stmt, ast.Expr) and isinstance(
                    getattr(stmt, "value", None), ast.Constant
                ):
                    ids.add(id(stmt.value))
    return ids


def _placeholder_values(path: Path) -> list[int]:
    """Line numbers of string VALUES (not comments, not docstrings) holding
    the placeholder."""
    src = path.read_text(encoding="utf-8", errors="replace")
    if PLACEHOLDER not in src:
        return []
    try:
        tree = ast.parse(src)
    except SyntaxError:
        # Python 2 legacy script: fall back to non-comment lines.
        return [
            n
            for n, line in enumerate(src.splitlines(), start=1)
            if PLACEHOLDER in line and not line.lstrip().startswith("#")
        ]
    docs = _docstring_nodes(tree)
    return sorted(
        node.lineno
        for node in ast.walk(tree)
        if isinstance(node, ast.Constant)
        and isinstance(node.value, str)
        and PLACEHOLDER in node.value
        and id(node) not in docs
    )


class TestNoExecutablePlaceholder:
    def test_no_python_value_is_the_placeholder(self):
        offenders = []
        for rel in _tracked("*.py"):
            if rel in ALLOWED_PY:
                continue
            lines = _placeholder_values(REPO / rel)
            offenders.extend(f"{rel}:{n}" for n in lines)
        assert not offenders, (
            "a string value in code is the redaction placeholder; resolve the "
            "path from the caller's config/argument or an environment variable "
            "instead:\n  " + "\n  ".join(offenders)
        )

    def test_no_script_command_uses_the_placeholder(self):
        offenders = []
        for rel in _tracked("*.bat", "*.cmd", "*.ps1", "*.sh"):
            ext = Path(rel).suffix.lower()
            text = (REPO / rel).read_text(encoding="utf-8", errors="replace")
            for n, line in enumerate(text.splitlines(), start=1):
                if PLACEHOLDER not in line:
                    continue
                if line.lstrip().lower().startswith(SCRIPT_COMMENT[ext]):
                    continue
                offenders.append(f"{rel}:{n}")
        assert not offenders, (
            "a script command runs against the redaction placeholder; read the "
            "location from an environment variable and stop when it is unset:"
            "\n  " + "\n  ".join(offenders)
        )


@pytest.fixture()
def private_paths(monkeypatch):
    monkeypatch.delenv("DIGITALMODEL_ACCESS_DB", raising=False)
    monkeypatch.delenv("DIGITALMODEL_PRIVATE_DATA", raising=False)
    return importlib.import_module("digitalmodel.infrastructure.utils.private_paths")


class TestPrivatePathHelper:
    def test_unset_access_database_is_a_clear_error(self, private_paths):
        with pytest.raises(private_paths.PrivatePathNotConfigured) as exc:
            private_paths.access_connection_string(None)
        assert "DIGITALMODEL_ACCESS_DB" in str(exc.value)

    def test_the_caller_database_argument_wins(self, private_paths, tmp_path):
        db = tmp_path / "atlas.accdb"
        s = private_paths.access_connection_string(str(db))
        assert s.startswith("Driver={Microsoft Access Driver (*.mdb, *.accdb)};")
        assert f"DBQ={db};" in s

    def test_the_environment_variable_is_used(
        self, private_paths, monkeypatch, tmp_path
    ):
        db = tmp_path / "from_env.accdb"
        monkeypatch.setenv("DIGITALMODEL_ACCESS_DB", str(db))
        assert f"DBQ={db};" in private_paths.access_connection_string(None)

    @pytest.mark.parametrize("where", ["argument", "environment"])
    def test_the_placeholder_itself_is_refused(self, private_paths, monkeypatch, where):
        value = PLACEHOLDER + "\\atlas.accdb"
        if where == "environment":
            monkeypatch.setenv("DIGITALMODEL_ACCESS_DB", value)
            value = None
        with pytest.raises(private_paths.PrivatePathNotConfigured):
            private_paths.access_connection_string(value)

    def test_private_data_path_needs_its_variable(self, private_paths):
        with pytest.raises(private_paths.PrivatePathNotConfigured) as exc:
            private_paths.private_data_path("TimeLine.csv")
        assert "DIGITALMODEL_PRIVATE_DATA" in str(exc.value)

    def test_private_data_path_joins_under_its_variable(
        self, private_paths, monkeypatch, tmp_path
    ):
        monkeypatch.setenv("DIGITALMODEL_PRIVATE_DATA", str(tmp_path))
        got = private_paths.private_data_path("sub", "TimeLine.csv")
        assert got == tmp_path / "sub" / "TimeLine.csv"

    def test_a_configured_value_wins_over_the_variable(
        self, private_paths, monkeypatch, tmp_path
    ):
        monkeypatch.setenv("DIGITALMODEL_PRIVATE_DATA", str(tmp_path / "env"))
        got = private_paths.private_data_path("a.csv", configured=str(tmp_path / "cfg"))
        assert got == tmp_path / "cfg" / "a.csv"


def _import_quietly(modname: str):
    """Import a legacy module whose unrelated invalid escape sequences raise
    under the suite's warnings-as-errors setting."""
    import warnings

    with warnings.catch_warnings():
        warnings.simplefilter("ignore", SyntaxWarning)
        warnings.simplefilter("ignore", DeprecationWarning)
        return importlib.import_module(modname)


ACCESS_MODULES = [
    "digitalmodel.infrastructure.utils.database",
    "digitalmodel.asset_integrity.common.database",
    "digitalmodel.infrastructure.core.database_legacy",
    "digitalmodel.infrastructure.persistence.database_legacy",
]


class TestAccessDatabaseConnection:
    """The Access branch used to overwrite the caller's database with a fixed
    private path; after C11 that path was the placeholder. It must use the
    caller's database, and without one stop with a clear error -- before any
    driver import, so the message is not lost to an ImportError."""

    @pytest.mark.parametrize("modname", ACCESS_MODULES)
    def test_no_database_is_a_clear_error(self, modname, private_paths):
        try:
            mod = _import_quietly(modname)
        except Exception as exc:  # noqa: BLE001
            pytest.skip(f"{modname} does not import here: {exc}")
        db = object.__new__(mod.Database)
        db.init_assign_db_properties({"server_type": "accdb", "database": None})
        with pytest.raises(private_paths.PrivatePathNotConfigured):
            db.enable_connection_and_cursor()

    @pytest.mark.parametrize("modname", ACCESS_MODULES)
    def test_the_callers_database_reaches_the_driver(
        self, modname, private_paths, monkeypatch, tmp_path
    ):
        import sys
        import types

        try:
            mod = _import_quietly(modname)
        except Exception as exc:  # noqa: BLE001
            pytest.skip(f"{modname} does not import here: {exc}")
        seen = []
        fake = types.SimpleNamespace(connect=lambda s: seen.append(s) or object())
        monkeypatch.setitem(sys.modules, "pyodbc", fake)
        monkeypatch.setitem(
            sys.modules, "pypyodbc", types.SimpleNamespace(connect=fake.connect)
        )
        target = tmp_path / "caller.accdb"
        db = object.__new__(mod.Database)
        db.init_assign_db_properties({"server_type": "accdb", "database": str(target)})
        db.enable_connection_and_cursor()
        assert seen, "no connection was attempted"
        assert all(f"DBQ={target};" in s for s in seen), seen
        assert not any(PLACEHOLDER in s for s in seen), seen
