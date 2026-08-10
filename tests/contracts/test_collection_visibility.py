"""Collection-visibility contract for recursive ``pytest tests/`` runs (#1977).

``norecursedirs`` matches a *separator-free* entry against a directory BASENAME
at any depth.  ``pytest.ini`` once listed ``docs``, ``projects`` and ``scripts``
there, intending the three top-level directories of those names; the entries
also pruned ``tests/docs/`` and ``tests/scripts/``, so a recursive run silently
skipped them while reporting a clean sweep.  CI never noticed, because
``norecursedirs`` does not filter a directory named explicitly on the command
line and every workflow passes explicit paths.

The property guarded here:

    No separator-free ``norecursedirs`` entry may prune a directory under
    ``tests/`` that contains at least one file matching ``python_files``.

Entries that contain a path separator are deliberate, targeted exclusions and
are exempt.  Directories holding no collectible files are irrelevant -- pruning
them costs nothing.  Both halves of that wording are load-bearing: an earlier
formulation ("no entry may prune any directory under tests/") is RED even
against a correct config, because ``__pycache__`` is itself a separator-free
entry and ``tests/**/__pycache__`` exists throughout the tree.

Root-only exclusions now live in the repository-root ``conftest.py`` as
``collect_ignore``, whose entries resolve relative to the directory holding the
conftest and are therefore root-anchored by construction.  The tests below also
guard that mechanism, so the exclusion cannot be quietly dropped now that it no
longer appears in ``pytest.ini``.

Second property, added by #1983
-------------------------------

``collect_ignore`` protects recursive runs, but it is *not* consulted for a
directory named explicitly on the command line -- ``pytest scripts/`` collects
regardless.  Thirty files under ``scripts/`` were named ``test_*.py`` or
``*_test.py``, and because pytest collects by importing, naming that path ran
them: collection alone wrote ``phase_convention_test_results.txt`` into the
repository root, created three ``temp_*/`` trees, and died with
``INTERNALERROR> mainloop: caught unexpected SystemExit!``.

    No file under ``scripts/`` may match ``python_files`` (except a commented,
    fully-consumed exemption list), and nothing still collectible under
    ``scripts/`` may reach an interpreter exit at module scope.

The two halves compose.  The first keeps ``scripts/`` out of pytest's reach by
name.  The second makes the *exemption mechanism* of the first safe: an exempt
file is, by definition, still imported by pytest, so it must not be able to
take the whole run down.  A module-scope exit in a file pytest never imports is
harmless, which is why the second guard is scoped to collectible files only --
22 non-collectible scripts under ``scripts/`` exit at module scope by design.
"""

from __future__ import annotations

import ast
import importlib.util
import os
from pathlib import Path

import pytest
from _pytest.pathlib import fnmatch_ex

# Entries known to be present in this repository's norecursedirs.  They exist
# only as a tripwire: pytest silently falls back to its own built-in default
# (['*.egg', '.*', '_darcs', 'build', 'CVS', 'dist', 'node_modules', 'venv',
# '{arch}']) when the ini file is not resolved, and that default contains
# nothing capable of pruning under tests/ -- so a config-resolution failure
# would otherwise turn every test in this module confidently green.
SENTINEL_ENTRIES = (".venv", "htmlcov")

# Known-positive control for the matcher itself.  fnmatch_ex is exactly what
# pytest uses for norecursedirs; if this pair ever stops matching, the matcher
# is not doing what this module assumes and every "no offenders" result below
# is meaningless.
CONTROL_ENTRY = "scripts"
CONTROL_DIR = "tests/scripts"


def _has_separator(entry: str) -> bool:
    return "/" in entry or os.sep in entry


def _matches_python_files(name: str, patterns: list[str]) -> bool:
    return any(fnmatch_ex(pattern, Path(name)) for pattern in patterns)


def _walk_tests_tree(
    tests_root: Path, patterns: list[str]
) -> tuple[list[Path], list[Path]]:
    """Return (every directory under tests/, those holding a collectible file).

    The walk is deliberately unfiltered -- it must see directories that pytest
    itself prunes, since those are precisely what this module reasons about.
    """
    all_dirs: list[Path] = []
    candidates: list[Path] = []
    for dirpath, _dirnames, filenames in os.walk(tests_root):
        directory = Path(dirpath)
        all_dirs.append(directory)
        if any(_matches_python_files(name, patterns) for name in filenames):
            candidates.append(directory)
    return all_dirs, candidates


def _root_collect_ignore(rootpath: Path) -> list[str]:
    """Load ``collect_ignore`` from the repository-root conftest.py.

    Loaded from its path under a private module name rather than read off the
    live plugin manager, so the assertion is about the file on disk and cannot
    be satisfied by some other conftest that happens to define the name.
    """
    conftest = rootpath / "conftest.py"
    assert conftest.is_file(), (
        f"expected a repository-root conftest.py at {conftest}; it carries the "
        "root-anchored collect_ignore that replaced the bare norecursedirs "
        "basenames (#1977)"
    )
    spec = importlib.util.spec_from_file_location("_dm1977_root_conftest", conftest)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return list(getattr(module, "collect_ignore", []))


@pytest.fixture(scope="module")
def rootpath(pytestconfig: pytest.Config) -> Path:
    return Path(pytestconfig.rootpath)


def test_no_bare_norecursedirs_entry_prunes_a_collectible_tests_directory(
    pytestconfig: pytest.Config,
) -> None:
    """The guard (#1977 G3).

    RED before the fix, naming tests/docs, tests/scripts and
    tests/solvers/orcaflex/mooring-tension-iteration/fsts-l015-test-cases/scripts.
    GREEN after.
    """
    root = Path(pytestconfig.rootpath)
    norecursedirs = list(pytestconfig.getini("norecursedirs"))

    # --- vacuity guard 1: the live config was actually loaded -------------
    inipath = pytestconfig.inipath
    assert inipath is not None and inipath.name == "pytest.ini", (
        "norecursedirs was not read from this repository's pytest.ini "
        f"(inipath={inipath!r}); getini() falls back to pytest's built-in "
        "default without raising, and that default can never prune anything "
        "under tests/ -- so this test would pass while checking nothing"
    )

    # --- vacuity guard 2: the value is this repo's, not a default ---------
    missing_sentinels = [e for e in SENTINEL_ENTRIES if e not in norecursedirs]
    assert not missing_sentinels, (
        f"norecursedirs is missing sentinel entries {missing_sentinels}; the "
        f"value read back was {norecursedirs!r}, which does not look like this "
        "repository's configuration"
    )

    bare_entries = [e for e in norecursedirs if not _has_separator(e)]
    exempt_entries = [e for e in norecursedirs if _has_separator(e)]

    # --- vacuity guard 3: there is something to check ---------------------
    assert bare_entries, (
        "no separator-free norecursedirs entries found; the main assertion "
        f"below would iterate over nothing (norecursedirs={norecursedirs!r})"
    )

    patterns = list(pytestconfig.getini("python_files"))
    assert patterns, "python_files is empty; no file could be judged collectible"

    tests_root = root / "tests"
    assert tests_root.is_dir(), f"expected a tests/ directory at {tests_root}"
    all_dirs, candidates = _walk_tests_tree(tests_root, patterns)

    # --- vacuity guard 4: the candidate set is non-empty ------------------
    assert candidates, (
        f"no directory under {tests_root} holds a file matching {patterns!r}; "
        "the main assertion below would have nothing to judge"
    )

    # --- vacuity guard 5: known-positive control on the matcher -----------
    control_dir = root / CONTROL_DIR
    assert control_dir.is_dir(), (
        f"control directory {control_dir} is missing; the matcher control "
        "below cannot be trusted without it"
    )
    assert fnmatch_ex(CONTROL_ENTRY, control_dir) is True, (
        f"fnmatch_ex({CONTROL_ENTRY!r}, {control_dir}) returned False; the "
        "matcher is not behaving as pytest's norecursedirs matching does, so "
        "a 'no offenders' result below would prove nothing"
    )

    # --- exemptions must be fully consumed --------------------------------
    # A path-form entry that matches no existing directory is dead weight; left
    # in place it could later be read as licence for an offender it does not
    # actually cover.  ('projects' was exactly such a dead entry.)
    unconsumed = [
        entry
        for entry in exempt_entries
        if not any(fnmatch_ex(entry, d) for d in all_dirs)
    ]
    assert not unconsumed, (
        f"norecursedirs path-form entries {unconsumed} match no directory that "
        "exists; delete them rather than leaving a stale exemption that could "
        "mask a future offender"
    )

    # --- the property -----------------------------------------------------
    offenders = [
        (entry, directory.relative_to(root).as_posix())
        for directory in sorted(candidates)
        for entry in bare_entries
        if fnmatch_ex(entry, directory)
    ]
    detail = "\n".join(
        f"  {path}  <- pruned by separator-free norecursedirs entry {entry!r}"
        for entry, path in offenders
    )
    assert not offenders, (
        f"{len(offenders)} directory/entry pair(s) under tests/ are pruned from "
        "recursive collection by a separator-free norecursedirs entry, so "
        "`pytest tests/` silently skips tests that CI runs (#1977):\n"
        f"{detail}\n"
        f"(checked {len(candidates)} candidate directories against "
        f"{bare_entries!r}). Fix: exclude the intended top-level directory via "
        "collect_ignore in the repository-root conftest.py, which is "
        "root-anchored by construction, not via a bare basename here."
    )


def test_top_level_scripts_is_not_collected(
    pytestconfig: pytest.Config, rootpath: Path
) -> None:
    """The root exclusion of scripts/ survives its move out of pytest.ini."""
    collect_ignore = _root_collect_ignore(rootpath)
    assert "scripts" in collect_ignore, (
        f"root conftest.py collect_ignore={collect_ignore!r} no longer excludes "
        "'scripts'; the top-level scripts/ tree is not part of the test suite "
        "and collecting it raises collection errors (#1977 G2)"
    )
    target = rootpath / "scripts"
    assert target.is_dir(), f"{target} does not exist; the exclusion is dead"
    patterns = list(pytestconfig.getini("python_files"))
    collectible = [
        p for p in target.rglob("*.py") if _matches_python_files(p.name, patterns)
    ]
    assert collectible, (
        f"{target} holds no file matching {patterns!r}, so excluding it is a "
        "no-op; re-check whether the exclusion is still needed"
    )


def test_top_level_docs_is_not_collected(
    pytestconfig: pytest.Config, rootpath: Path
) -> None:
    """The root exclusion of docs/ survives its move out of pytest.ini."""
    collect_ignore = _root_collect_ignore(rootpath)
    assert "docs" in collect_ignore, (
        f"root conftest.py collect_ignore={collect_ignore!r} no longer excludes "
        "'docs'; the top-level docs/ tree is not part of the test suite "
        "(#1977 G2)"
    )
    target = rootpath / "docs"
    assert target.is_dir(), f"{target} does not exist; the exclusion is dead"
    patterns = list(pytestconfig.getini("python_files"))
    collectible = [
        p for p in target.rglob("*.py") if _matches_python_files(p.name, patterns)
    ]
    assert collectible, (
        f"{target} holds no file matching {patterns!r}, so excluding it is a "
        "no-op; re-check whether the exclusion is still needed"
    )


def test_collect_ignore_entries_exist(rootpath: Path) -> None:
    """No root collect_ignore entry may name a path that does not exist.

    'projects' sat in norecursedirs for years naming nothing at all.  A dead
    exclusion is invisible, so a rename can silently un-exclude a tree.
    """
    collect_ignore = _root_collect_ignore(rootpath)
    assert collect_ignore, "root conftest.py defines no collect_ignore entries"
    missing = [entry for entry in collect_ignore if not (rootpath / entry).exists()]
    assert not missing, (
        f"root conftest.py collect_ignore entries {missing} name paths that do "
        f"not exist under {rootpath}; delete or repoint them"
    )


# ---------------------------------------------------------------------------
# #1983: scripts/ must not wear a test name, and whatever remains collectible
# under scripts/ must not exit the interpreter at module scope.
# ---------------------------------------------------------------------------

# Files under scripts/ permitted to keep a name matching python_files.
#
# Every entry must (a) exist and (b) actually match python_files, both asserted
# below -- a stale exemption is worse than no exemption, because it reads as
# licence for an offender it does not cover.  ('projects' sat in norecursedirs
# for years naming nothing at all; see #1977.)
SCRIPT_TEST_NAME_EXEMPTIONS = (
    # scripts/solver_smoke_test.py is live operator tooling, not a stray script.
    # The deckhand licensed-run lane invokes it unattended over SSH; it was last
    # touched 2026-07-31 ("Add end-to-end solver smoke test (OrcaFlex + AQWA
    # licence probe)").  Renaming it can break a scheduled task on a licensed
    # host this repository cannot see, and the fleet sweep that would clear that
    # risk cannot be run from a worktree, so #1983 Stage 5 deferred the rename
    # rather than mitigating it with `git log --follow`, which does nothing for
    # a scheduled task on another machine.
    #
    # Note the name is ALSO a live engine dispatch key -- src/digitalmodel/
    # engine.py:834 `elif basename == "solver_smoke_test":`, the shipped
    # base_configs/modules/solver_smoke_test/, the solver_smoke_test.json report
    # name, and the solver-smoke CI domain.  Those are a different thing from
    # this script and must not be swept into any future rename of it.
    #
    # It is safe to leave collectible: it collects 0 items, and the guard below
    # proves it reaches no module-scope exit.
    "scripts/solver_smoke_test.py",
)

# Known-positive controls for the name matcher.  If `python_files` is ever read
# back as something that does not match these, a "no offenders" result proves
# nothing.  The negative control pins the other half of the fix: `check_*` is
# only a safe prefix for as long as it matches no pattern in the live config.
NAME_CONTROL_POSITIVE = ("test_control_probe.py", "control_probe_test.py")
NAME_CONTROL_NEGATIVE = ("check_control_probe.py",)

_EXIT_CALL_NAMES = frozenset({"exit", "quit", "sys.exit", "os._exit"})

_SCOPE_BOUNDARIES = (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef, ast.Lambda)

# Source used as the known-positive control for the module-scope exit scan.
# Lines marked EXPECT-EXIT must be found; every other exit here must not be.
# A shallow scan of `tree.body` finds NONE of the expected lines, which is
# exactly how the first two drafts of this guard measured zero offenders
# against a tree containing three (#1983).
_EXIT_CONTROL_SOURCE = '''\
import os
import sys

try:
    import OrcFxAPI
except ImportError:
    sys.exit(1)  # EXPECT-EXIT nested in an except handler

for _item in range(1):
    while True:
        with open(__file__) as _fh:
            if _fh:
                exit(2)  # EXPECT-EXIT bare exit nested five levels deep

try:
    pass
finally:
    os._exit(3)  # EXPECT-EXIT nested in a finally block

try:
    pass
except Exception:
    pass
else:
    quit(4)  # EXPECT-EXIT nested in a try/else

if os.environ:
    raise SystemExit(5)  # EXPECT-EXIT raise nested in an if


def _helper():
    sys.exit(6)  # must NOT be found: function body


class _Klass:
    os._exit(7)  # must NOT be found: class body


_lam = lambda: exit(8)  # must NOT be found: lambda body

if __name__ == "__main__":
    sys.exit(9)  # must NOT be found: __main__ guard
else:
    os._exit(10)  # EXPECT-EXIT the else branch of a __main__ guard DOES run
'''


def _is_main_guard(node: ast.AST) -> bool:
    """True for ``if __name__ == "__main__":`` (the only excluded If)."""
    if not isinstance(node, ast.If):
        return False
    test = node.test
    return (
        isinstance(test, ast.Compare)
        and isinstance(test.left, ast.Name)
        and test.left.id == "__name__"
        and len(test.ops) == 1
        and isinstance(test.ops[0], ast.Eq)
        and len(test.comparators) == 1
        and isinstance(test.comparators[0], ast.Constant)
        and test.comparators[0].value == "__main__"
    )


def _call_name(node: ast.Call) -> str | None:
    func = node.func
    if isinstance(func, ast.Name):
        return func.id
    if isinstance(func, ast.Attribute) and isinstance(func.value, ast.Name):
        return f"{func.value.id}.{func.attr}"
    return None


def _collect_exits(node: ast.AST, found: list[tuple[int, str]]) -> None:
    """Recursively collect exits reachable when ``node``'s module is imported.

    Descent is by ``iter_child_nodes``, which reaches through ``If``, ``Try``
    (body, handlers, orelse, finalbody), ``With``, ``For``, ``While`` and
    ``Match`` bodies without needing each spelled out.  Three things are pruned:
    function/class/lambda bodies (not run at import), and the *body* of a
    ``__main__`` guard.  A ``__main__`` guard's ``orelse`` is NOT pruned -- it
    runs on import, which is the whole point of the guard.
    """
    for child in ast.iter_child_nodes(node):
        if isinstance(child, _SCOPE_BOUNDARIES):
            # Decorators and argument defaults still evaluate at module scope
            # even though the body does not.
            for side in getattr(child, "decorator_list", []) or []:
                _collect_exits_in_expression(side, found)
            args = getattr(child, "args", None)
            if isinstance(args, ast.arguments):
                for default in [*args.defaults, *(args.kw_defaults or [])]:
                    if default is not None:
                        _collect_exits_in_expression(default, found)
            for base in getattr(child, "bases", []) or []:
                _collect_exits_in_expression(base, found)
            continue

        if _is_main_guard(child):
            _collect_exits_in_expression(child.test, found)
            for stmt in child.orelse:
                _record_exit(stmt, found)
                _collect_exits(stmt, found)
            continue

        _record_exit(child, found)
        _collect_exits(child, found)


def _collect_exits_in_expression(node: ast.AST, found: list[tuple[int, str]]) -> None:
    _record_exit(node, found)
    _collect_exits(node, found)


def _record_exit(node: ast.AST, found: list[tuple[int, str]]) -> None:
    if isinstance(node, ast.Call):
        name = _call_name(node)
        if name in _EXIT_CALL_NAMES:
            found.append((node.lineno, name))
    elif isinstance(node, ast.Raise):
        exc = node.exc
        target = exc.func if isinstance(exc, ast.Call) else exc
        if isinstance(target, ast.Name) and target.id == "SystemExit":
            found.append((node.lineno, "raise SystemExit"))


def module_scope_exits(source: str) -> list[tuple[int, str]]:
    """Exits reachable at import time, sorted by line."""
    found: list[tuple[int, str]] = []
    _collect_exits(ast.parse(source), found)
    return sorted(set(found))


def _shallow_body_exits(source: str) -> list[tuple[int, str]]:
    """The naive scan: only top-level statements, no descent.  Control only."""
    found: list[tuple[int, str]] = []
    for stmt in ast.parse(source).body:
        _record_exit(stmt, found)
    return sorted(set(found))


def _scripts_python_files(rootpath: Path) -> list[Path]:
    scripts_root = rootpath / "scripts"
    assert scripts_root.is_dir(), (
        f"expected a scripts/ directory at {scripts_root}; both guards below "
        "would have nothing to judge without it"
    )
    return sorted(p for p in scripts_root.rglob("*.py") if p.is_file())


def _assert_live_python_files(pytestconfig: pytest.Config) -> list[str]:
    """Return python_files, having proved it came from this repo's pytest.ini.

    Measured during #1977: with the ini key absent, ``getini`` silently returns
    pytest's own built-in default and every assertion downstream passes having
    checked nothing.
    """
    inipath = pytestconfig.inipath
    assert inipath is not None and inipath.name == "pytest.ini", (
        "python_files was not read from this repository's pytest.ini "
        f"(inipath={inipath!r}); getini() falls back to pytest's built-in "
        "default without raising, so this test would pass while checking nothing"
    )
    patterns = list(pytestconfig.getini("python_files"))
    assert patterns, "python_files is empty; no file could be judged collectible"

    for name in NAME_CONTROL_POSITIVE:
        assert _matches_python_files(name, patterns), (
            f"control filename {name!r} does not match python_files={patterns!r}; "
            "the matcher is not behaving as pytest's collection does, so a "
            "'no offenders' result below would prove nothing"
        )
    for name in NAME_CONTROL_NEGATIVE:
        assert not _matches_python_files(name, patterns), (
            f"control filename {name!r} now MATCHES python_files={patterns!r}. "
            "The check_* prefix is the whole basis of the #1983 rename; if it "
            "has become collectible, 27 renamed scripts are collectible again."
        )
    return patterns


def test_no_script_is_named_like_a_test(pytestconfig: pytest.Config, rootpath: Path):
    """The #1983 G2 guard: nothing under scripts/ wears a collectible name.

    RED before the rename, naming 30 files.  GREEN after, with a single
    commented exemption.
    """
    patterns = _assert_live_python_files(pytestconfig)
    candidates = _scripts_python_files(rootpath)

    # --- vacuity guard: there is something to judge -----------------------
    assert candidates, (
        f"no .py file found under {rootpath / 'scripts'}; the assertion below "
        "would iterate over nothing"
    )

    exemptions = set(SCRIPT_TEST_NAME_EXEMPTIONS)
    offenders = []
    consumed = set()
    for path in candidates:
        rel = path.relative_to(rootpath).as_posix()
        if not _matches_python_files(path.name, patterns):
            continue
        if rel in exemptions:
            consumed.add(rel)
            continue
        offenders.append(rel)

    # --- exemptions must be fully consumed --------------------------------
    unconsumed = sorted(exemptions - consumed)
    assert not unconsumed, (
        f"SCRIPT_TEST_NAME_EXEMPTIONS entries {unconsumed} do not correspond to "
        "an existing file under scripts/ that actually matches "
        f"{patterns!r}. Delete them: a stale exemption is invisible licence, "
        "and it can mask a genuine offender that a later edit moves onto the "
        "same path."
    )

    detail = "\n".join(f"  {path}" for path in offenders)
    assert not offenders, (
        f"{len(offenders)} file(s) under scripts/ match python_files "
        f"{patterns!r}, so pointing pytest at them imports and therefore RUNS "
        "them (#1983):\n"
        f"{detail}\n"
        f"(checked {len(candidates)} .py files; "
        f"{len(consumed)} exemption(s) consumed). Fix: rename to check_*, "
        "which matches no pattern in python_files and is already the local "
        "convention (scripts/check_generated_html.py, "
        "scripts/legal/check_protected_identifiers.py). If the file is a real "
        "test, move it under tests/<domain>/ instead."
    )


def test_module_scope_exit_scan_descends(rootpath: Path):
    """Known-positive control for the exit scan, run before the guard below.

    This test is the reason the guard can be believed.  It pins three things:
    the scan finds exits nested inside try/except/finally/else, if, for, while
    and with; it does not find exits in function, class or lambda bodies or in
    a __main__ guard; and it DOES find them in a __main__ guard's else branch.
    It also demonstrates that the naive `tree.body` scan finds none of them.
    """
    expected = {
        lineno
        for lineno, line in enumerate(_EXIT_CONTROL_SOURCE.splitlines(), start=1)
        if "EXPECT-EXIT" in line
    }
    assert len(expected) == 6, (
        f"the control source should mark 6 reachable exits, marked {expected}; "
        "the control has drifted from what it is asserting"
    )

    found = module_scope_exits(_EXIT_CONTROL_SOURCE)
    found_lines = {lineno for lineno, _ in found}
    assert found_lines == expected, (
        f"module_scope_exits() found lines {sorted(found_lines)} on the control "
        f"source but should have found {sorted(expected)}. "
        f"Missing={sorted(expected - found_lines)} "
        f"Spurious={sorted(found_lines - expected)}. The guard below cannot be "
        "trusted while this disagrees."
    )
    assert {name for _, name in found} >= {"sys.exit", "exit", "os._exit"}, (
        f"the control found only {sorted({n for _, n in found})}; a matcher that "
        "recognises sys.exit but not bare exit() misses 17 of the 18 real call "
        "sites under scripts/ (#1983)"
    )

    shallow = _shallow_body_exits(_EXIT_CONTROL_SOURCE)
    assert not shallow, (
        f"a shallow tree.body scan found {shallow} on the control source. It is "
        "supposed to find NOTHING -- that is the point of the control, and the "
        "reason two earlier drafts of this guard measured zero offenders "
        "against a tree containing three."
    )


def test_no_module_scope_exit_under_scripts(
    pytestconfig: pytest.Config, rootpath: Path
):
    """The #1983 G1 guard: nothing pytest still imports under scripts/ exits.

    Scoped to collectible files.  An exit at module scope only matters if
    something imports the module, and after the rename the only importer is
    pytest.  RED before the rename, naming scripts/test_orcaflex_loading.py
    (sys.exit inside except ImportError, which takes the entire run down with
    INTERNALERROR), scripts/testing/test_complete_workflow.py and
    scripts/testing/test_model_generator_basic.py (bare exit() inside
    except Exception).  GREEN after.
    """
    patterns = _assert_live_python_files(pytestconfig)
    collectible = [
        path
        for path in _scripts_python_files(rootpath)
        if _matches_python_files(path.name, patterns)
    ]

    # --- vacuity guard: the scanned set is non-empty -----------------------
    # It holds because SCRIPT_TEST_NAME_EXEMPTIONS keeps exactly one file
    # collectible.  If that exemption is ever retired, nothing under scripts/ is
    # importable by pytest and this guard has nothing left to protect -- delete
    # it together with the exemption rather than leaving it silently vacuous.
    assert collectible, (
        f"no file under scripts/ matches python_files {patterns!r}, so this "
        "guard is scanning nothing. That is the ideal end state, but a passing "
        "vacuous test is not evidence: retire this test alongside the last "
        "entry in SCRIPT_TEST_NAME_EXEMPTIONS."
    )

    offenders = []
    for path in collectible:
        source = path.read_text(encoding="utf-8", errors="replace")
        try:
            exits = module_scope_exits(source)
        except SyntaxError as exc:  # pragma: no cover - defensive
            pytest.fail(f"{path.relative_to(rootpath)} does not parse: {exc}")
        if exits:
            rel = path.relative_to(rootpath).as_posix()
            shown = ", ".join(f"{name} at line {lineno}" for lineno, name in exits[:5])
            more = f" (+{len(exits) - 5} more)" if len(exits) > 5 else ""
            offenders.append(f"  {rel}: {shown}{more}")

    detail = "\n".join(offenders)
    assert not offenders, (
        f"{len(offenders)} file(s) under scripts/ are collectible by pytest AND "
        "reach an interpreter exit at module scope. pytest does not treat "
        "SystemExit as a collection error, so importing one of these kills the "
        "entire run with 'INTERNALERROR> mainloop: caught unexpected "
        "SystemExit!' -- taking every other result with it (#1983):\n"
        f"{detail}\n"
        f"(scanned {len(collectible)} collectible file(s) of "
        f"{len(_scripts_python_files(rootpath))} .py files under scripts/). "
        "Fix: rename the file to check_* so pytest never imports it, or move "
        "the exit under an `if __name__ == \"__main__\":` guard."
    )
