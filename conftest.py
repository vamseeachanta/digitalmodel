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

Every directory below holds files matching ``python_files`` that are not part
of this repository's test suite.  Collecting the top-level ``scripts/`` used to
yield 75 items and 8 collection errors and to write into the working tree; the
rename in #1983 reduced that to 0 items, and this exclusion keeps recursive
runs away from it regardless.  The exclusion is load-bearing for any invocation
naming a path above ``tests/``.

``.codex`` and ``.gemini`` are added by #1983.  Each contains exactly one
tracked entry -- a ``skills`` *symlink* pointing outside this repository, at
``workspace-hub/.claude/skills``.  Collecting either pulls in 163 tests that
belong to another repository and immediately errors, because those tests
declare markers (``cloud``) that this repository's ``--strict-markers``
configuration does not know.  Note that plain ``find`` reports zero matching
files under both, since it does not traverse the symlink; ``find -L`` reports
seven each.  A survey that does not follow symlinks will conclude, wrongly,
that there is nothing there.

``examples`` is deliberately NOT excluded.  It holds 27 tracked files matching
``python_files``, and they collect to 315 real tests.  Excluding it would
silently remove them from every recursive run -- precisely the #1977 defect
this file was created to fix, at a larger scale than the original 177.  They
are, separately, owned by no row in ``tests/DOMAINS.md`` and therefore run in
no CI shard; that gap is real but it is a different problem, and hiding the
tests is not a fix for it.

Keep this file declarative.  A root conftest is imported by every pytest
invocation in the repository, so anything executable here is executed
everywhere.

``scripts/ci/detect_touched_domains.py`` lists this file in
``FULL_MATRIX_PATHS``: it is a collection-control surface, so editing it must
dispatch the full CI matrix exactly as editing ``pytest.ini`` does.
"""

collect_ignore = ["scripts", "docs", ".codex", ".gemini"]
