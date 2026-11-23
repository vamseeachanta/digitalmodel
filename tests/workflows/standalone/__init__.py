"""Tests for the standalone MCP servers.

This ``__init__.py`` is load-bearing, not decoration. Without it,
``tests/workflows/standalone/`` was the first ancestor with no ``__init__.py``,
so pytest derived the top-level package name ``markitdown`` for
``tests/workflows/standalone/markitdown/`` and put that directory on the import
path. Any ``from markitdown import MarkItDown`` in shipped code then resolved to
the TEST package instead of the PyPI distribution:

    ImportError: cannot import name 'MarkItDown' from 'markitdown'
                 (tests/workflows/standalone/markitdown/__init__.py)

With this file present the package chain reaches ``tests`` (which has its own
``__init__.py``), the test package is ``tests.workflows.standalone.markitdown``,
and nothing test-side occupies a top-level name. See #1923.
"""
