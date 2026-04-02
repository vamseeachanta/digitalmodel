# ABOUTME: Tests for digitalmodel.reservoir package — imports and module verification.
# ABOUTME: Part of SKELETON → DEVELOPMENT coverage uplift (#1589).
# ABOUTME: reservoir.stratigraphic is a raw plotting script (not importable as-is),
# ABOUTME: so tests focus on package importability and module existence.
"""
Tests for digitalmodel.reservoir

The reservoir package is minimal:
- __init__.py (package docstring only)
- stratigraphic.py (a raw matplotlib plotting script, not a callable module)

Since stratigraphic.py uses undefined globals (WELL1, df_logs, etc.),
we cannot import it directly. Tests verify:
- Package importability
- Module file existence
- __init__.py docstring presence
"""

import os
import importlib
import pytest


class TestPackageImport:
    """Verify reservoir package is importable."""

    def test_import_reservoir_package(self):
        import digitalmodel.reservoir
        assert digitalmodel.reservoir is not None

    def test_package_has_docstring(self):
        import digitalmodel.reservoir
        assert digitalmodel.reservoir.__doc__ is not None
        assert "reservoir" in digitalmodel.reservoir.__doc__.lower()


class TestModuleExistence:
    """Verify expected source files exist on disk."""

    def _package_dir(self) -> str:
        import digitalmodel.reservoir
        return os.path.dirname(digitalmodel.reservoir.__file__)

    def test_init_exists(self):
        pkg_dir = self._package_dir()
        assert os.path.isfile(os.path.join(pkg_dir, "__init__.py"))

    def test_stratigraphic_module_exists(self):
        pkg_dir = self._package_dir()
        assert os.path.isfile(os.path.join(pkg_dir, "stratigraphic.py"))


class TestStratigraphicScript:
    """Verify stratigraphic.py content without importing it (it has undefined globals)."""

    def _read_source(self) -> str:
        import digitalmodel.reservoir
        path = os.path.join(
            os.path.dirname(digitalmodel.reservoir.__file__), "stratigraphic.py"
        )
        with open(path) as f:
            return f.read()

    def test_script_uses_matplotlib(self):
        src = self._read_source()
        assert "matplotlib" in src

    def test_script_uses_pandas(self):
        src = self._read_source()
        assert "pandas" in src or "pd" in src

    def test_script_references_well_data(self):
        src = self._read_source()
        assert "UWI" in src or "wells_list" in src

    def test_script_not_importable_due_to_globals(self):
        """stratigraphic.py references undefined globals (WELL1, df_logs) — importing should fail."""
        with pytest.raises((NameError, ImportError, Exception)):
            importlib.import_module("digitalmodel.reservoir.stratigraphic")
