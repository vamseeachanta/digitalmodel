"""Repository-root pytest configuration: root-anchored collection exclusions.

``collect_ignore`` entries resolve relative to the directory holding the
conftest that declares them.  Declared here, at the repository root, they are
root-anchored *by construction* -- ``scripts`` below means this repository's
top-level ``scripts/``, and nothing else, at any depth, ever.

That property is why these two exclusions live here rather than in
``pytest.ini``.  ``norecursedirs`` cannot express it.  Its matcher compares a
separator-free entry against a directory *basename at any depth*, so the bare
entry ``scripts`` pruned ``tests/scripts/`` alongside the top-level
``scripts/`` and silently removed 177 tests from every recursive run -- the
defect this file exists to fix (#1977).  No portable ``norecursedirs`` pattern
anchors to the repository root; 32 candidates were checked.  The most tempting
of them, ``./scripts``, is the most dangerous: because it contains a separator
pytest switches to full-path matching and prefixes ``*/``, yielding
``*/./scripts``, which never matches a normalised path.  It reads as
"anchored" and is silently equivalent to deleting the entry.

Both directories below hold files matching ``python_files`` that are not part
of the test suite; collecting the top-level ``scripts/`` yields 75 items and 8
collection errors, and has filesystem side effects.  The exclusion is
load-bearing for any invocation naming a path above ``tests/``.

Keep this file declarative.  A root conftest is imported by every pytest
invocation in the repository, so anything executable here is executed
everywhere.

``scripts/ci/detect_touched_domains.py`` lists this file in
``FULL_MATRIX_PATHS``: it is a collection-control surface, so editing it must
dispatch the full CI matrix exactly as editing ``pytest.ini`` does.
"""

collect_ignore = ["scripts", "docs"]
