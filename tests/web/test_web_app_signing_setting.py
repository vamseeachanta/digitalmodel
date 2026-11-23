"""Offline contracts for the web application's required signing setting."""

import ast
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
PACKAGE = ROOT / "src/digitalmodel/web/digitaltwinfeed"
APP = PACKAGE / "app.py"
VARIABLE = "DIGITALTWINFEED_SECRET_KEY"


def _name(node):
    if isinstance(node, ast.Name):
        return node.id
    if isinstance(node, ast.Attribute):
        return node.attr
    if isinstance(node, ast.Subscript):
        return _name(node.slice)
    if isinstance(node, ast.Constant) and isinstance(node.value, str):
        return node.value
    return ""


def _bindings(tree):
    for node in ast.walk(tree):
        if isinstance(node, (ast.Assign, ast.AnnAssign, ast.NamedExpr)):
            targets = node.targets if isinstance(node, ast.Assign) else [node.target]
            for target in targets:
                form = ("attribute" if isinstance(target, ast.Attribute)
                        else "subscript" if isinstance(target, ast.Subscript)
                        else "name")
                yield _name(target), node.value, node, form
        elif isinstance(node, ast.keyword):
            yield node.arg, node.value, node, "keyword"
        elif isinstance(node, ast.Dict):
            for key, value in zip(node.keys, node.values):
                yield _name(key), value, node, "dictionary"
        elif isinstance(node, ast.arguments):
            args = node.posonlyargs + node.args
            for arg, value in zip(args[-len(node.defaults):], node.defaults):
                yield arg.arg, value, arg, "parameter"
            for arg, value in zip(node.kwonlyargs, node.kw_defaults):
                if value is not None:
                    yield arg.arg, value, arg, "parameter"


def _signing_bindings(path):
    tree = ast.parse(path.read_text(encoding="utf-8"))
    bindings = [
        (value, node, form)
        for name, value, node, form in _bindings(tree)
        if isinstance(name, str) and name.casefold() == "secret_key"
    ]
    return tree, bindings


def _load_helper():
    tree = ast.parse(APP.read_text(encoding="utf-8"))
    helper = next(
        (node for node in tree.body if isinstance(node, ast.FunctionDef)
         and node.name == "load_signing_key"), None,
    )
    if helper is None:
        pytest.fail("Required signing-setting loader is missing")
    namespace = {}
    exec(compile(ast.Module(body=[helper], type_ignores=[]),
                 "<web-signing-helper>", "exec"), namespace)
    return namespace["load_signing_key"]


def _assert_refusal(helper):
    with pytest.raises(RuntimeError) as error:
        helper()
    message = str(error.value)
    for instruction in (
        VARIABLE,
        "application does not start",
        "repository holds no value",
        "python -c",
        "secrets.token_hex(32)",
        "export",
        "development run",
    ):
        if instruction not in message:
            pytest.fail("Start-up refusal lacks a required instruction")


def test_no_literal_bound_to_signing_setting():
    for path in (APP,):
        tree, bindings = _signing_bindings(path)
        if path == APP and not bindings:
            pytest.fail("app.py: signing-setting binding is missing")
        for value, node, form in bindings:
            position = f"{path.relative_to(ROOT)}:{node.lineno}:{form}"
            if any(
                isinstance(part, ast.JoinedStr)
                or isinstance(part, ast.Constant)
                and isinstance(part.value, (str, bytes))
                for part in ast.walk(value)
            ):
                pytest.fail(f"Literal bound to signing setting: {position}")
            if not (
                isinstance(value, ast.Call)
                and isinstance(value.func, ast.Name)
                and value.func.id == "load_signing_key"
                and not value.args
                and not value.keywords
            ):
                pytest.fail(f"Signing setting must call required loader: {position}")
            if node not in tree.body:
                pytest.fail(f"Signing setting must be bound at module start-up: {position}")
        helper = next(
            (node for node in tree.body if isinstance(node, ast.FunctionDef)
             and node.name == "load_signing_key"), None,
        )
        if helper is None or any(helper.lineno >= node.lineno
                                 for _, node, _ in bindings):
            pytest.fail("app.py: required loader must be defined before start-up binding")


def test_missing_variable_refuses_startup(monkeypatch):
    monkeypatch.delenv(VARIABLE, raising=False)
    _assert_refusal(_load_helper())


def test_empty_or_blank_variable_refuses_startup(monkeypatch):
    helper = _load_helper()
    for value in ("", " ", "\t\n"):
        monkeypatch.setenv(VARIABLE, value)
        _assert_refusal(helper)


def test_value_comes_from_environment(monkeypatch):
    monkeypatch.setenv(VARIABLE, "dummy")
    helper = _load_helper()
    if helper() != "dummy":
        pytest.fail("Signing-setting loader did not return the environment value")
    monkeypatch.setenv(VARIABLE, " dummy ")
    if helper() != " dummy ":
        pytest.fail("Signing-setting loader must preserve nonblank input")
