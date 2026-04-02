# ABOUTME: Tests for template_code/TemplateCode.py — REST API template patterns.
# ABOUTME: Part of SKELETON → DEVELOPMENT coverage uplift (#1584).
"""
Tests for the TemplateCode REST API template module.

Analyzes the source for Flask-RESTful patterns without requiring Flask.
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


class TestTemplateCodeSource:
    """Analyze template_code/TemplateCode.py REST API patterns."""

    def test_uses_flask_restful(self):
        src = _read_source("template_code/TemplateCode.py")
        assert "flask_restful" in src or "Api" in src

    def test_creates_api_object(self):
        src = _read_source("template_code/TemplateCode.py")
        assert "Api(" in src

    def test_defines_app_index_resource(self):
        src = _read_source("template_code/TemplateCode.py")
        assert "class AppIndex" in src

    def test_registers_resource_endpoint(self):
        src = _read_source("template_code/TemplateCode.py")
        assert "api.add_resource" in src

    def test_has_auth_handler(self):
        src = _read_source("template_code/TemplateCode.py")
        assert "get_password" in src


class TestDigitaltwinfeedTemplateCode:
    """Analyze digitaltwinfeed/TemplateCode/TemplateCode.py."""

    def test_uses_flask_restful(self):
        src = _read_source("digitaltwinfeed/TemplateCode/TemplateCode.py")
        assert "flask_restful" in src

    def test_creates_blueprint(self):
        src = _read_source("digitaltwinfeed/TemplateCode/TemplateCode.py")
        assert "Blueprint" in src

    def test_defines_app_index_resource(self):
        src = _read_source("digitaltwinfeed/TemplateCode/TemplateCode.py")
        assert "class AppIndex" in src

    def test_registers_resource_endpoint(self):
        src = _read_source("digitaltwinfeed/TemplateCode/TemplateCode.py")
        assert "api.add_resource" in src
