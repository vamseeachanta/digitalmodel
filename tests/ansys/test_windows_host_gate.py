"""Keep the Windows-host test gate (tests/ansys/conftest.py, owner decision M05) exact."""
import ast
from pathlib import Path
from types import SimpleNamespace

import pytest

from tests.ansys import conftest as gate

HERE = Path(gate.__file__).resolve().parent


def _test_functions(module_name):
    tree = ast.parse((HERE / module_name).read_text(encoding='utf-8'))
    return {node.name for node in ast.walk(tree)
            if isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef))
            and node.name.startswith('test_')}


@pytest.mark.parametrize('module_name', sorted(gate.WINDOWS_HOST_ONLY))
def test_every_gated_test_exists(module_name):
    missing = set(gate.WINDOWS_HOST_ONLY[module_name]) - _test_functions(module_name)
    assert not missing, f'stale gate entries in {module_name}: {sorted(missing)}'


def test_gate_is_explicit_and_every_entry_has_a_reason():
    reasons = {gate.IS_JUNCTION, gate.JUNCTION_GUARDED_PATH, gate.CREATE_NO_WINDOW,
               gate.ST_FILE_ATTRIBUTES, gate.PPID_MAP, gate.WINDOWS_HOST_SEMANTICS}
    entries = [r for module in gate.WINDOWS_HOST_ONLY.values() for r in module.values()]
    assert entries and set(entries) <= reasons
    assert 'test_windows_host_gate.py' not in gate.WINDOWS_HOST_ONLY
    assert 'test_path_compat.py' not in gate.WINDOWS_HOST_ONLY


def test_reason_lookup():
    module, functions = next(iter(sorted(gate.WINDOWS_HOST_ONLY.items())))
    function = next(iter(sorted(functions)))
    assert gate.windows_host_reason(module, function).startswith(
        'requires the licensed Windows ANSYS host: ')
    assert gate.windows_host_reason(module, 'test_not_listed') is None
    assert gate.windows_host_reason('test_unlisted_module.py', function) is None


def _item(module, name, parent=HERE):
    marks = []
    return SimpleNamespace(path=parent / module, name=name + '[case]', originalname=name,
                           add_marker=marks.append, marks=marks)


@pytest.mark.parametrize('platform, expect_skip', [('linux', True), ('win32', False)])
def test_hook_skips_only_listed_tests_off_windows(monkeypatch, platform, expect_skip):
    monkeypatch.setattr(gate.sys, 'platform', platform)
    module, functions = next(iter(sorted(gate.WINDOWS_HOST_ONLY.items())))
    gated = _item(module, next(iter(sorted(functions))))
    ungated = _item(module, 'test_not_listed')
    elsewhere = _item(module, gated.originalname, parent=HERE.parent / 'other')
    gate.pytest_collection_modifyitems(None, [gated, ungated, elsewhere])
    assert bool(gated.marks) is expect_skip
    if expect_skip:
        assert gated.marks[0].kwargs['reason'].startswith('requires the licensed Windows ANSYS host')
    assert not ungated.marks and not elsewhere.marks
