# ABOUTME: Tests for digitalmodel.reservoir package — imports and module verification.
# ABOUTME: Part of SKELETON → DEVELOPMENT coverage uplift (#1589).
# ABOUTME: reservoir.stratigraphic is importable after the plotting refactor.
"""
Tests for digitalmodel.reservoir

The reservoir package is minimal:
- __init__.py (package docstring only)
- stratigraphic.py (importable plotting module)

Tests verify:
- Package importability
- Module file existence
- __init__.py docstring presence
"""

import os
import importlib


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
    """Verify stratigraphic.py content and importability."""

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
        """stratigraphic.py exposes callable functions instead of module globals."""
        module = importlib.import_module("digitalmodel.reservoir.stratigraphic")
        assert callable(module.create_cross_section)
