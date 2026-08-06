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
"""

from __future__ import annotations

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


def _walk_tests_tree(tests_root: Path, patterns: list[str]) -> tuple[list[Path], list[Path]]:
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


def test_top_level_scripts_is_not_collected(rootpath: Path) -> None:
    """The root exclusion of scripts/ survives its move out of pytest.ini."""
    collect_ignore = _root_collect_ignore(rootpath)
    assert "scripts" in collect_ignore, (
        f"root conftest.py collect_ignore={collect_ignore!r} no longer excludes "
        "'scripts'; the top-level scripts/ tree is not part of the test suite "
        "and collecting it raises collection errors (#1977 G2)"
    )
    target = rootpath / "scripts"
    assert target.is_dir(), f"{target} does not exist; the exclusion is dead"
    patterns = ["test_*.py", "*_test.py"]
    collectible = [
        p for p in target.rglob("*.py") if _matches_python_files(p.name, patterns)
    ]
    assert collectible, (
        f"{target} holds no file matching {patterns!r}, so excluding it is a "
        "no-op; re-check whether the exclusion is still needed"
    )


def test_top_level_docs_is_not_collected(rootpath: Path) -> None:
    """The root exclusion of docs/ survives its move out of pytest.ini."""
    collect_ignore = _root_collect_ignore(rootpath)
    assert "docs" in collect_ignore, (
        f"root conftest.py collect_ignore={collect_ignore!r} no longer excludes "
        "'docs'; the top-level docs/ tree is not part of the test suite "
        "(#1977 G2)"
    )
    target = rootpath / "docs"
    assert target.is_dir(), f"{target} does not exist; the exclusion is dead"
    patterns = ["test_*.py", "*_test.py"]
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
