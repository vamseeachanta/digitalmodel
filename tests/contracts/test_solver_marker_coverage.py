"""Every test that reaches the licensed OrcFxAPI must carry ``pytest.mark.solver``.

``tests/conftest.py`` deselects ``solver``-marked tests unless the ``-m``
expression names ``solver``.  That protects a host where OrcFxAPI is installed
only if the marker is present: a test gated on ``import OrcFxAPI`` alone starts
licensed solver work (``Model()``, ``LoadData``, ``CalculateStatics``,
``Diffraction().Calculate()``) as soon as the import succeeds.

This is a static scan; nothing under ``tests/`` is imported.  For each collected
test module and ``conftest.py`` it finds the names bound to the real OrcFxAPI
(``import OrcFxAPI [as x]``, ``from OrcFxAPI import ...``,
``x = pytest.importorskip("OrcFxAPI")``, ``importlib.import_module``).  A test
*reaches* OrcFxAPI when its body or decorators, a module-level or same-class
helper it uses, or a fixture it requests (in the module or in a ``conftest.py``
above it) loads one of those names or calls ``importorskip("OrcFxAPI")``.  A
``find_spec("OrcFxAPI")`` availability probe and an import whose name is never
used are not reaches.  An OrcFxAPI call at import time cannot be deselected and
is reported on its own.

Limits: reaches through ``src`` helpers are not traced, so tests that reach the
solver only through a ``src`` helper are marked by hand; a fixture is matched
to a test by parameter name and a helper by name.
"""

from __future__ import annotations

import ast
from dataclasses import dataclass, field
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
TESTS = ROOT / "tests"
API = "OrcFxAPI"
DYNAMIC_IMPORTS = {"importorskip", "import_module", "__import__"}
FUNCS = (ast.FunctionDef, ast.AsyncFunctionDef)


def _is_api_call(node: ast.AST) -> bool:
    """``importorskip("OrcFxAPI")`` and the like: binds the real module."""
    if not isinstance(node, ast.Call) or not node.args:
        return False
    first = node.args[0]
    func = node.func
    name = func.attr if isinstance(func, ast.Attribute) else getattr(func, "id", "")
    return name in DYNAMIC_IMPORTS and isinstance(first, ast.Constant) and first.value == API


def _is_solver_mark(node: ast.AST, solver_names: set[str]) -> bool:
    """``pytest.mark.solver`` anywhere in an expression, or a name bound to one."""
    for sub in ast.walk(node):
        if (
            isinstance(sub, ast.Attribute)
            and sub.attr == "solver"
            and isinstance(sub.value, ast.Attribute)
            and sub.value.attr == "mark"
        ):
            return True
        if isinstance(sub, ast.Name) and sub.id in solver_names:
            return True
    return False


def _api_names(tree: ast.Module) -> set[str]:
    names: set[str] = set()
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            for alias in node.names:
                if alias.name.split(".")[0] == API:
                    names.add((alias.asname or alias.name).split(".")[0])
        elif isinstance(node, ast.ImportFrom) and (node.module or "").split(".")[0] == API:
            names.update(alias.asname or alias.name for alias in node.names)
        elif isinstance(node, ast.Assign) and _is_api_call(node.value):
            names.update(t.id for t in node.targets if isinstance(t, ast.Name))
    return names


def _direct_reach(node: ast.AST, api: set[str]) -> bool:
    return any(
        (isinstance(sub, ast.Name) and isinstance(sub.ctx, ast.Load) and sub.id in api)
        or _is_api_call(sub)
        for sub in ast.walk(node)
    )


def _names(node: ast.AST) -> set[str]:
    """Bare names and ``self.x`` / ``cls.x`` attributes loaded in a body."""
    out: set[str] = set()
    for sub in ast.walk(node):
        if isinstance(sub, ast.Name) and isinstance(sub.ctx, ast.Load):
            out.add(sub.id)
        elif (
            isinstance(sub, ast.Attribute)
            and isinstance(sub.value, ast.Name)
            and sub.value.id in {"self", "cls"}
        ):
            out.add(sub.attr)
    return out


def _params(fn: ast.AST) -> set[str]:
    a = fn.args
    return {p.arg for p in [*a.posonlyargs, *a.args, *a.kwonlyargs]}


def _is_fixture(fn: ast.AST) -> bool:
    return any("fixture" in ast.unparse(d) for d in fn.decorator_list)


def _close(defs: dict[str, ast.AST], api: set[str], helpers: set[str], fixtures: set[str]):
    """Grow the (helpers, fixtures) that reach OrcFxAPI over ``defs`` to a fixed point.

    A helper is matched to a use by name; a fixture only by parameter name.
    """
    helpers, fixtures = set(helpers), set(fixtures)
    grown = True
    while grown:
        grown = False
        for name, fn in defs.items():
            if name in helpers or name in fixtures:
                continue
            if _direct_reach(fn, api) or _names(fn) & helpers or _params(fn) & fixtures:
                (fixtures if _is_fixture(fn) else helpers).add(name)
                grown = True
    return helpers, fixtures


def _is_main_guard(node: ast.AST) -> bool:
    """``if __name__ == "__main__":`` does not run when pytest imports the module."""
    return (
        isinstance(node, ast.If)
        and isinstance(node.test, ast.Compare)
        and isinstance(node.test.left, ast.Name)
        and node.test.left.id == "__name__"
        and any(isinstance(c, ast.Constant) and c.value == "__main__" for c in node.test.comparators)
    )


def _module_scope(body: list[ast.stmt]):
    """Statements executed at import time, outside function and class bodies."""
    for node in body:
        if isinstance(node, (*FUNCS, ast.ClassDef)) or _is_main_guard(node):
            continue
        yield node
        for attr in ("body", "orelse", "finalbody"):
            yield from _module_scope(getattr(node, attr, None) or [])
        for handler in getattr(node, "handlers", None) or []:
            yield from _module_scope(handler.body)


@dataclass
class Module:
    path: Path
    tree: ast.Module
    api: set[str]
    solver_names: set[str] = field(default_factory=set)

    @classmethod
    def parse(cls, path: Path) -> "Module":
        tree = ast.parse(path.read_text(encoding="utf-8"), filename=str(path))
        mod = cls(path, tree, _api_names(tree))
        for node in tree.body:
            if isinstance(node, ast.Assign) and _is_solver_mark(node.value, set()):
                mod.solver_names.update(t.id for t in node.targets if isinstance(t, ast.Name))
        return mod

    def _defs(self, body: list[ast.stmt]) -> dict[str, ast.AST]:
        return {n.name: n for n in body if isinstance(n, FUNCS) and not n.name.startswith("test")}

    def reaching_fixtures(self, outer: set[str]) -> set[str]:
        return _close(self._defs(self.tree.body), self.api, set(), outer)[1]

    def violations(self, outer: set[str], base: Path) -> list[str]:
        """Unmarked tests that reach OrcFxAPI, and import-time OrcFxAPI calls."""
        rel = self.path.relative_to(base).as_posix()
        found = [
            f"{rel}:{node.lineno} calls OrcFxAPI at import time"
            for node in _module_scope(self.tree.body)
            if isinstance(node, (ast.Expr, ast.Assign, ast.AnnAssign, ast.AugAssign))
            and any(
                isinstance(sub, ast.Call) and _direct_reach(sub.func, self.api)
                for sub in ast.walk(node)
            )
        ]
        if "pytestmark" in self.solver_names:
            return found
        helpers, fixtures = _close(self._defs(self.tree.body), self.api, set(), outer)
        for node in self.tree.body:
            if isinstance(node, FUNCS) and node.name.startswith("test"):
                if self._reaches(node, helpers, fixtures) and not self._marked(node):
                    found.append(f"{rel}::{node.name}")
            elif isinstance(node, ast.ClassDef) and node.name.startswith("Test"):
                found.extend(self._class_violations(node, helpers, fixtures, rel))
        return found

    def _class_violations(self, cls_node, helpers, fixtures, rel) -> list[str]:
        if self._marked(cls_node) or any(
            isinstance(n, ast.Assign)
            and any(isinstance(t, ast.Name) and t.id == "pytestmark" for t in n.targets)
            and _is_solver_mark(n.value, self.solver_names)
            for n in cls_node.body
        ):
            return []
        helpers, fixtures = _close(self._defs(cls_node.body), self.api, helpers, fixtures)
        return [
            f"{rel}::{cls_node.name}::{n.name}"
            for n in cls_node.body
            if isinstance(n, FUNCS)
            and n.name.startswith("test")
            and self._reaches(n, helpers, fixtures)
            and not self._marked(n)
        ]

    def _reaches(self, fn, helpers: set[str], fixtures: set[str]) -> bool:
        return _direct_reach(fn, self.api) or bool(_names(fn) & helpers) or bool(_params(fn) & fixtures)

    def _marked(self, node) -> bool:
        return any(_is_solver_mark(d, self.solver_names) for d in node.decorator_list)


def _collected(path: Path) -> bool:
    name = path.name
    return name == "conftest.py" or (
        name.endswith(".py") and (name.startswith("test_") or name.endswith("_test.py"))
    )


def scan(root: Path = TESTS, base: Path = ROOT) -> list[str]:
    """Return every unmarked OrcFxAPI reach in test modules under ``root``."""
    paths = [p for p in sorted(root.rglob("*.py")) if "__pycache__" not in p.parts and _collected(p)]
    texts = {p: p.read_text(encoding="utf-8", errors="replace") for p in paths}
    # Reaching fixtures of a conftest.py apply to its directory subtree; shallow
    # conftest files go first so a deeper one sees its ancestors' fixtures.
    inherited: dict[Path, set[str]] = {}

    def outer_for(directory: Path) -> set[str]:
        return set().union(
            *(names for d, names in inherited.items() if d == directory or d in directory.parents)
        )

    for path in sorted((p for p in paths if p.name == "conftest.py"), key=lambda p: len(p.parts)):
        outer = outer_for(path.parent)
        if API in texts[path] or outer:
            fixtures = Module.parse(path).reaching_fixtures(outer) - outer
            if fixtures:
                inherited[path.parent] = fixtures
    found: list[str] = []
    for path in paths:
        outer = set() if path.name == "conftest.py" else outer_for(path.parent)
        if API not in texts[path] and not outer:
            continue
        found.extend(Module.parse(path).violations(outer, base))
    return found


def test_every_test_that_reaches_orcfxapi_is_solver_marked():
    """A licensed host must never start solver work without ``-m solver``."""
    missing = scan()
    assert not missing, (
        "These tests reach OrcFxAPI without pytest.mark.solver (mark the test, "
        "its class, or the module's pytestmark):\n  " + "\n  ".join(missing)
    )


# --- the scan's own rules, on constructed modules -------------------------


def _scan_text(tmp_path: Path, files: dict[str, str]) -> list[str]:
    for rel, text in files.items():
        target = tmp_path / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding="utf-8")
    return [v.split("::", 1)[-1] for v in scan(tmp_path / "tests", tmp_path)]


GATED = "import pytest\ntry:\n    import OrcFxAPI\n    HAS = True\nexcept ImportError:\n    HAS = False\n"


@pytest.mark.parametrize(
    "body, expected",
    [
        # The pattern that started licensed work: gated on the import only.
        ("@pytest.mark.skipif(not HAS, reason='x')\ndef test_a():\n    OrcFxAPI.Model()\n", ["test_a"]),
        ("@pytest.mark.solver\ndef test_a():\n    OrcFxAPI.Model()\n", []),
        ("pytestmark = pytest.mark.solver\ndef test_a():\n    OrcFxAPI.Model()\n", []),
        ("pytestmark = [pytest.mark.solver]\ndef test_a():\n    OrcFxAPI.Model()\n", []),
        # A mark list bound to a module-level name.
        ("lic = [pytest.mark.solver]\n@lic[0]\ndef test_a():\n    OrcFxAPI.Model()\n", []),
        # Through a helper, a helper chain, a fixture and a fixture chain.
        ("def _h():\n    return OrcFxAPI.Model()\ndef test_a():\n    _h()\n", ["test_a"]),
        ("def _h():\n    OrcFxAPI.Model()\ndef _g():\n    _h()\ndef test_a():\n    _g()\n", ["test_a"]),
        (
            "@pytest.fixture\ndef m():\n    return OrcFxAPI.Model()\n"
            "@pytest.fixture\ndef n(m):\n    return m\ndef test_a(n):\n    pass\n",
            ["test_a"],
        ),
        # Classes: an unmarked reaching method fails, a marked class passes,
        # a same-class helper counts, a non-reaching sibling is not reported.
        (
            "class TestA:\n    def _h(self):\n        OrcFxAPI.Model()\n"
            "    def test_x(self):\n        self._h()\n    def test_y(self):\n        pass\n",
            ["TestA::test_x"],
        ),
        ("@pytest.mark.solver\nclass TestA:\n    def test_x(self):\n        OrcFxAPI.Model()\n", []),
        ("class TestA:\n    pytestmark = pytest.mark.solver\n    def test_x(self):\n        OrcFxAPI.Model()\n", []),
        # importorskip binds the real module, also inside a decorator.
        ("def test_a():\n    api = pytest.importorskip('OrcFxAPI')\n    api.Model()\n", ["test_a"]),
        ("@pytest.mark.skipif(not pytest.importorskip('OrcFxAPI'), reason='x')\ndef test_a():\n    pass\n", ["test_a"]),
        ("from OrcFxAPI import Model as M\ndef test_a():\n    M()\n", ["test_a"]),
        # Probes, unused imports and tests that never touch the API pass.
        ("def test_a():\n    assert HAS in (True, False)\n", []),
        ("def test_a():\n    import importlib.util\n    importlib.util.find_spec('OrcFxAPI')\n", []),
        # A local variable named like a helper elsewhere is not a fixture match.
        ("def test_a(tmp_path):\n    model = 1\n    assert model\n", []),
    ],
)
def test_scan_rules(tmp_path, body, expected):
    assert _scan_text(tmp_path, {"tests/test_m.py": GATED + body}) == expected


def test_scan_reports_import_time_calls_even_in_a_marked_module(tmp_path):
    found = _scan_text(
        tmp_path,
        {"tests/test_m.py": GATED + "pytestmark = pytest.mark.solver\nM = OrcFxAPI.Model()\n"},
    )
    assert found == ["tests/test_m.py:8 calls OrcFxAPI at import time"]


def test_scan_ignores_the_main_guard(tmp_path):
    body = GATED + "if __name__ == '__main__':\n    OrcFxAPI.Model()\n"
    assert _scan_text(tmp_path, {"tests/test_m.py": body}) == []


def test_scan_follows_conftest_fixtures_into_their_subtree_only(tmp_path):
    found = _scan_text(
        tmp_path,
        {
            "tests/a/conftest.py": (
                "import pytest\nimport OrcFxAPI\n@pytest.fixture\ndef model():\n"
                "    return OrcFxAPI.Model()\n"
            ),
            "tests/a/b/conftest.py": "import pytest\n@pytest.fixture\ndef wrapped(model):\n    return model\n",
            "tests/a/b/test_m.py": "def test_a(wrapped):\n    pass\ndef test_b():\n    pass\n",
            "tests/c/test_m.py": "def test_a(model):\n    pass\n",
        },
    )
    assert found == ["test_a"]


def test_scan_reads_the_named_test_file_patterns_only(tmp_path):
    body = GATED + "def test_a():\n    OrcFxAPI.Model()\n"
    found = _scan_text(
        tmp_path,
        {"tests/smoke_test.py": body, "tests/helper.py": body, "tests/scripts/run.py": body},
    )
    assert found == ["test_a"]
