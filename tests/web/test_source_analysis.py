# ABOUTME: Source file analysis tests for digitalmodel.web modules.
# ABOUTME: Verifies Flask blueprint patterns, route definitions, and code quality
# ABOUTME: without importing Flask itself.
"""
Tests for web package source file analysis.

Reads source files directly and validates structural patterns:
- Blueprint declarations
- Route definitions
- Auth handlers
- Template references
"""

import os
import re
import pytest


def _web_root() -> str:
    import digitalmodel.web
    return os.path.dirname(digitalmodel.web.__file__)


def _read_source(relpath: str) -> str:
    path = os.path.join(_web_root(), relpath)
    with open(path) as f:
        return f.read()


class TestDtfBlueprint:
    """Analyze dtf/dtf.py source for Flask patterns."""

    def test_creates_blueprint(self):
        src = _read_source("dtf/dtf.py")
        assert "Blueprint" in src
        assert 'Blueprint("dtf"' in src or "Blueprint('dtf'" in src

    def test_has_index_route(self):
        src = _read_source("dtf/dtf.py")
        assert '@dtf.route("/")' in src or '@dtf.route("/", methods=["GET"])' in src

    def test_has_multiple_routes(self):
        src = _read_source("dtf/dtf.py")
        routes = re.findall(r'@dtf\.route\(', src)
        assert len(routes) >= 3, f"Expected >=3 routes, found {len(routes)}"

    def test_renders_templates(self):
        src = _read_source("dtf/dtf.py")
        templates = re.findall(r'render_template\(["\']dtf/', src)
        assert len(templates) >= 2


class TestExampleBlueprint:
    """Analyze example_blueprint source."""

    def test_creates_blueprint(self):
        src = _read_source("example_blueprint/example_blueprint.py")
        assert "Blueprint" in src

    def test_has_show_route(self):
        src = _read_source("example_blueprint/example_blueprint.py")
        assert "def show(" in src

    def test_handles_template_not_found(self):
        src = _read_source("example_blueprint/example_blueprint.py")
        assert "TemplateNotFound" in src
        assert "abort(404)" in src


class TestExampleSPA:
    """Analyze example_SPA/example_SPA.py source for data logic."""

    def test_defines_food_data(self):
        src = _read_source("example_SPA/example_SPA.py")
        assert "food" in src
        assert "fruit" in src
        assert "vegetables" in src
        assert "meat" in src

    def test_defines_context(self):
        src = _read_source("example_SPA/example_SPA.py")
        assert "context" in src
        assert "brand" in src

    def test_has_food_endpoint(self):
        src = _read_source("example_SPA/example_SPA.py")
        assert "get_food" in src
        assert "foodkind" in src

    def test_has_sidebar_items(self):
        src = _read_source("example_SPA/example_SPA.py")
        assert "sidebar_subitems_alias" in src


class TestAppModule:
    """Analyze digitaltwinfeed/app.py source."""

    def test_creates_flask_app(self):
        src = _read_source("digitaltwinfeed/app.py")
        assert "Flask(__name__)" in src

    def test_has_register_blueprint_function(self):
        src = _read_source("digitaltwinfeed/app.py")
        assert "def register_blueprint" in src

    def test_registers_multiple_blueprints(self):
        src = _read_source("digitaltwinfeed/app.py")
        # Check for the service blueprint list
        assert "todorestful" in src
        assert "bseedata" in src
        assert "GoMFields" in src

    def test_has_blog_routes(self):
        src = _read_source("digitaltwinfeed/app.py")
        assert "def blog(" in src or "@app.route('/blog'" in src

    def test_has_main_guard(self):
        src = _read_source("digitaltwinfeed/app.py")
        assert "__main__" in src
