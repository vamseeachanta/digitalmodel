"""Offline contracts for finance credential handling."""

import ast
import re
from pathlib import Path

import pytest


ROOT = Path(__file__).resolve().parents[2]
PACKAGE = ROOT / "src/digitalmodel/specialized/finance"
SOURCE = PACKAGE / "stock_analysis/finance_components_get_data.py"
CREDENTIAL_NAME = re.compile(r"key|token|secret|password|passwd|pwd|credential|auth", re.I)
NON_CREDENTIALS = {
    # Database column name used for the upsert primary key, not authentication.
    ("src/digitalmodel/specialized/finance/stock_analysis/finance_components.py", "primary_key"),
}


def _named_values(tree):
    for node in ast.walk(tree):
        if isinstance(node, (ast.Assign, ast.AnnAssign)):
            targets = node.targets if isinstance(node, ast.Assign) else [node.target]
            for target in targets:
                for part in ast.walk(target):
                    if isinstance(part, (ast.Name, ast.Attribute)):
                        name = part.id if isinstance(part, ast.Name) else part.attr
                        yield name, node.value
                    elif isinstance(part, ast.Subscript) and isinstance(part.slice, ast.Constant):
                        yield part.slice.value, node.value
        elif isinstance(node, ast.keyword):
            yield node.arg, node.value
        elif isinstance(node, ast.Dict):
            for key, value in zip(node.keys, node.values):
                if isinstance(key, ast.Constant):
                    yield key.value, value
        elif isinstance(node, (ast.FunctionDef, ast.AsyncFunctionDef, ast.Lambda)):
            args = node.args.posonlyargs + node.args.args
            for arg, value in zip(args[-len(node.args.defaults):], node.args.defaults):
                yield arg.arg, value
            for arg, value in zip(node.args.kwonlyargs, node.args.kw_defaults):
                if value is not None:
                    yield arg.arg, value


def _load_helper():
    """Compile only the helper AST; optional market-data imports are not executed."""
    tree = ast.parse(SOURCE.read_text(encoding="utf-8"))
    helper = next(
        (node for node in tree.body if isinstance(node, ast.FunctionDef)
         and node.name == "_get_tiingo_api_key"), None,
    )
    assert helper is not None, "Required environment credential helper is missing"
    namespace = {}
    exec(compile(ast.Module(body=[helper], type_ignores=[]), "<finance-helper>", "exec"), namespace)
    return namespace["_get_tiingo_api_key"]


def test_no_credential_literals_in_finance_package():
    findings = []
    for path in sorted(PACKAGE.rglob("*.py")):
        relative = path.relative_to(ROOT).as_posix()
        tree = ast.parse(path.read_text(encoding="utf-8"))
        for name, value in _named_values(tree):
            if (isinstance(name, str) and CREDENTIAL_NAME.search(name)
                    and isinstance(value, ast.Constant) and isinstance(value.value, str)
                    and value.value and (relative, name) not in NON_CREDENTIALS):
                findings.append(f"{relative}:{value.lineno}:{name}")
    assert not findings, "Credential literals: " + ", ".join(findings)


def test_missing_variable_raises_error_naming_it(monkeypatch):
    monkeypatch.delenv("TIINGO_API_KEY", raising=False)
    helper = _load_helper()
    with pytest.raises(RuntimeError, match="TIINGO_API_KEY.*must be set"):
        helper()


def test_empty_variable_is_treated_as_unset(monkeypatch):
    monkeypatch.setenv("TIINGO_API_KEY", "")
    helper = _load_helper()
    with pytest.raises(RuntimeError, match="TIINGO_API_KEY.*must be set"):
        helper()


def test_value_is_read_from_environment(monkeypatch):
    monkeypatch.setenv("TIINGO_API_KEY", "dummy")
    helper = _load_helper()
    assert helper() == "dummy"
    monkeypatch.setenv("TIINGO_API_KEY", "changed")
    assert helper() == "changed"
    tree = ast.parse(SOURCE.read_text(encoding="utf-8"))
    method = next(node for node in ast.walk(tree)
                  if isinstance(node, ast.FunctionDef) and node.name == "get_data_from_tiingo")
    assignment = next(node for node in method.body if isinstance(node, ast.Assign)
                      and any(isinstance(t, ast.Name) and t.id == "api_key" for t in node.targets))
    assert isinstance(assignment.value, ast.Call)
    assert isinstance(assignment.value.func, ast.Name)
    assert assignment.value.func.id == "_get_tiingo_api_key"
    provider_call = next(node for node in ast.walk(method) if isinstance(node, ast.Call)
                         and isinstance(node.func, ast.Attribute) and node.func.attr == "get_data_tiingo")
    argument = next(keyword.value for keyword in provider_call.keywords if keyword.arg == "api_key")
    assert isinstance(argument, ast.Name) and argument.id == "api_key"
