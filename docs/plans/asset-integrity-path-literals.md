# Plan-lite: asset-integrity path literals

Scope: remove invalid escape warnings from the existing path literals in
`common/ApplicationManager.py`, `common/data.py`, and `common/database.py`
under `src/digitalmodel/asset_integrity/`.

Approach: add raw-string prefixes only to the 23 affected literals. Preserve
their runtime values and the surrounding calculations. Keep the package-wide
compilation regression in `tests/asset_integrity/test_source_compilation.py`.

Acceptance criteria: forced package compilation succeeds with both
`SyntaxWarning` and `DeprecationWarning` treated as errors on Python 3.11 and
3.12. Parsed-source equality against the merge base confirms unchanged
runtime values in all three source files. The compilation regression passes
without importing engineering dependencies or invoking a solver.

Limits: compilation and parsed-source equality establish syntax compatibility
and preservation of the edited literals. They do not establish full-suite
dependency compatibility or validate engineering calculations.
