# ABOUTME: Tests for the todo modules' data structures and patterns.
# ABOUTME: Analyzes todobasic and todorestful source without Flask runtime.
"""
Tests for web todo modules data structures.

Validates the in-memory task data structure and REST API patterns
used in todobasic.py and todorestful.py source files.
"""

import ast
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


class TestTodoBasicStructure:
    """Analyze todobasic data and API patterns."""

    def test_defines_tasks_list(self):
        src = _read_source("digitaltwinfeed/todobasic/todobasic.py")
        assert "tasks = [" in src

    def test_tasks_have_expected_fields(self):
        src = _read_source("digitaltwinfeed/todobasic/todobasic.py")
        assert "'id'" in src
        assert "'title'" in src
        assert "'description'" in src
        assert "'done'" in src

    def test_has_crud_routes(self):
        src = _read_source("digitaltwinfeed/todobasic/todobasic.py")
        assert "methods=['GET']" in src or 'methods=["GET"]' in src
        assert "methods=['POST']" in src or 'methods=["POST"]' in src
        assert "methods=['PUT']" in src or 'methods=["PUT"]' in src

    def test_has_auth_handler(self):
        src = _read_source("digitaltwinfeed/todobasic/todobasic.py")
        assert "auth.get_password" in src or "get_password" in src

    def test_has_error_handler(self):
        src = _read_source("digitaltwinfeed/todobasic/todobasic.py")
        assert "errorhandler(404)" in src or "abort(404)" in src


class TestTodoRestfulStructure:
    """Analyze todorestful REST API patterns."""

    def test_defines_tasks_list(self):
        src = _read_source("digitaltwinfeed/todorestful/todorestful.py")
        assert "tasks = [" in src

    def test_uses_flask_restful(self):
        src = _read_source("digitaltwinfeed/todorestful/todorestful.py")
        assert "flask_restful" in src
        assert "Resource" in src

    def test_defines_task_fields(self):
        src = _read_source("digitaltwinfeed/todorestful/todorestful.py")
        assert "task_fields" in src
        assert "fields.String" in src
        assert "fields.Boolean" in src

    def test_has_task_list_api_class(self):
        src = _read_source("digitaltwinfeed/todorestful/todorestful.py")
        assert "class TaskListAPI" in src

    def test_has_request_parser(self):
        src = _read_source("digitaltwinfeed/todorestful/todorestful.py")
        assert "reqparse.RequestParser" in src or "RequestParser" in src
