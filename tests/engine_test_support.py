"""Scoped engine doubles for routing tests; never import engineering backends."""

import ast
from contextlib import contextmanager
import importlib.util
from pathlib import Path
import sys
from types import ModuleType
from unittest.mock import MagicMock

import pytest


class MockAttributeDict(dict):
    """Dictionary with attribute access, sufficient for engine routing tests."""

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.__dict__ = self


@contextmanager
def isolated_engine():
    """Execute the real engine with temporary imports and fresh global bindings.

    Source: engine.py's import statements define the dependency boundary.
    Mock both sys.modules and parent-package attributes, restoring their exact
    prior identities on success, import failure or a failing test. Reloading a
    cached engine in place would retain mocked globals in other importers.
    """
    import digitalmodel

    source = Path(digitalmodel.__file__).with_name("engine.py")
    tree = ast.parse(source.read_text(encoding="utf-8"))
    imports = {}
    for node in tree.body:
        if (
            isinstance(node, ast.ImportFrom)
            and node.module
            and node.module.startswith(("assetutilities.", "digitalmodel.", "loguru"))
        ):
            imports.setdefault(node.module, []).extend(
                alias.name for alias in node.names
            )
    # Lazy routes exercised by these unit tests also stay behind doubles.
    imports.update(
        {
            "digitalmodel.subsea.catenary_riser.legacy.catenary": ["Catenary"],
            "digitalmodel.specialized.rigging.rigging": ["Rigging"],
            "digitalmodel.specialized.gis": ["GISModule"],
            "digitalmodel.subsea.mooring_analysis.cli": ["cli"],
        }
    )

    with pytest.MonkeyPatch.context() as patcher:

        def install(name, module):
            parent_name, _, attribute = name.rpartition(".")
            if parent_name:
                parent = sys.modules.get(parent_name)
                if parent is None:
                    parent = ModuleType(parent_name)
                    parent.__path__ = []
                    install(parent_name, parent)
                # Read the namespace directly: getattr can execute a package's
                # lazy imports before its dependencies have been replaced.
                patcher.setitem(vars(parent), attribute, module)
            patcher.setitem(sys.modules, name, module)

        for name, attributes in imports.items():
            module = ModuleType(name)
            for attribute in attributes:
                setattr(module, attribute, MagicMock(name=f"{name}.{attribute}"))
            install(name, module)

        sys.modules[
            "assetutilities.common.update_deep"
        ].AttributeDict = MockAttributeDict
        spec = importlib.util.spec_from_file_location("digitalmodel.engine", source)
        module = importlib.util.module_from_spec(spec)
        install(spec.name, module)
        spec.loader.exec_module(module)
        yield module


@pytest.fixture
def mocked_engine():
    """Provide a fresh engine for each requesting routing test."""
    with isolated_engine() as module:
        yield module


def engine(*args, **kwargs):
    """Call the engine installed by the active mocked_engine fixture."""
    return sys.modules["digitalmodel.engine"].engine(*args, **kwargs)
