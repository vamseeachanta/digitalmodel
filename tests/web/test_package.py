# ABOUTME: Import and structure tests for digitalmodel.web package.
# ABOUTME: Part of SKELETON → DEVELOPMENT coverage uplift (#1584).
"""
Tests for digitalmodel.web — package importability and structure.

The web package is a Flask/Dash application tier. Flask is an optional
dependency so tests that require Flask are conditionally skipped.
"""

import os
import pytest


class TestPackageImport:
    """Verify web package is importable at the top level."""

    def test_import_web_package(self):
        import digitalmodel.web
        assert digitalmodel.web is not None

    def test_web_has_docstring(self):
        import digitalmodel.web
        assert digitalmodel.web.__doc__ is not None
        assert "web" in digitalmodel.web.__doc__.lower()


class TestPackageDirectoryStructure:
    """Verify expected sub-packages and files exist on disk."""

    def _web_root(self) -> str:
        import digitalmodel.web
        return os.path.dirname(digitalmodel.web.__file__)

    def test_init_exists(self):
        assert os.path.isfile(os.path.join(self._web_root(), "__init__.py"))

    def test_dtf_subpackage_exists(self):
        dtf_dir = os.path.join(self._web_root(), "dtf")
        assert os.path.isdir(dtf_dir)
        assert os.path.isfile(os.path.join(dtf_dir, "dtf.py"))

    def test_template_code_subpackage_exists(self):
        tc_dir = os.path.join(self._web_root(), "template_code")
        assert os.path.isdir(tc_dir)
        assert os.path.isfile(os.path.join(tc_dir, "TemplateCode.py"))

    def test_example_blueprint_exists(self):
        eb_dir = os.path.join(self._web_root(), "example_blueprint")
        assert os.path.isdir(eb_dir)
        assert os.path.isfile(os.path.join(eb_dir, "example_blueprint.py"))

    def test_example_spa_exists(self):
        spa_dir = os.path.join(self._web_root(), "example_SPA")
        assert os.path.isdir(spa_dir)
        assert os.path.isfile(os.path.join(spa_dir, "example_SPA.py"))

    def test_digitaltwinfeed_app_exists(self):
        dtf_dir = os.path.join(self._web_root(), "digitaltwinfeed")
        assert os.path.isdir(dtf_dir)
        assert os.path.isfile(os.path.join(dtf_dir, "app.py"))
