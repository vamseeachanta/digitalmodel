# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline contracts for environment-based web authentication."""
import ast
import importlib.util
import logging
import re
from pathlib import Path
import pytest
CALLBACKS = {'get_password', 'get_pw', 'verify_password', 'check_password', 'verify_token', 'check_auth'}
DECORATORS = {'get_password', 'verify_password', 'verify_token', 'hash_password'}
WORDS = {'secret', 'token', 'password', 'passwd', 'pwd', 'credential', 'credentials', 'auth', 'salt', 'signature', 'apikey', 'dsn'}

def last(n):
    if isinstance(n, ast.Call):
        return last(n.func)
    return n.id if isinstance(n, ast.Name) else n.attr if isinstance(n, ast.Attribute) else ''

def credential(name):
    spelled = re.sub(r'([A-Z]+)([A-Z][a-z])', r'\1_\2', str(name))
    words = re.sub(r'([a-z0-9])([A-Z])', r'\1_\2', spelled).lower().split('_')
    return bool(WORDS.intersection(words)) or words[-1] == 'key'

def has_literal(n):
    if n is None:
        return False
    return any((isinstance(x, ast.JoinedStr) or (isinstance(x, ast.Constant) and isinstance(x.value, (str, bytes))) for x in ast.walk(n)))

def literal(n):
    if n is None:
        return False
    if isinstance(n, ast.Call) and isinstance(n.func, ast.Attribute):
        func = n.func
        env_get = func.attr == 'get' and isinstance(func.value, ast.Attribute) and (func.value.attr == 'environ') and isinstance(func.value.value, ast.Name) and (func.value.value.id == 'os')
        getenv = func.attr == 'getenv' and isinstance(func.value, ast.Name) and (func.value.id == 'os')
        if (env_get or getenv) and n.args and isinstance(n.args[0], ast.Constant):
            return any((literal(a) for a in n.args[1:])) or any((literal(k.value) for k in n.keywords))
    if isinstance(n, ast.Subscript) and isinstance(n.value, ast.Attribute) and (n.value.attr == 'environ') and isinstance(n.value.value, ast.Name) and (n.value.value.id == 'os'):
        return False
    if isinstance(n, ast.JoinedStr) or (isinstance(n, ast.Constant) and isinstance(n.value, (str, bytes))):
        return True
    return any((literal(c) for c in ast.iter_child_nodes(n)))

def named(tree):
    for n in ast.walk(tree):
        if isinstance(n, (ast.Assign, ast.AnnAssign, ast.AugAssign, ast.NamedExpr)):
            targets = n.targets if isinstance(n, ast.Assign) else [n.target]
            for t in targets:
                parts = [p for p in ast.walk(t)
                         if isinstance(p, (ast.Name, ast.Attribute, ast.Subscript)) and isinstance(p.ctx, ast.Store)]
                for p in parts:
                    name = last(p)
                    if isinstance(p, ast.Subscript) and isinstance(p.slice, ast.Constant):
                        name = p.slice.value
                    if name:
                        yield (str(name), n.value, n)
        elif isinstance(n, ast.keyword) and n.arg is not None:
            yield (n.arg, n.value, n)
        elif isinstance(n, ast.Dict):
            for k, v in zip(n.keys, n.values):
                if isinstance(k, ast.Constant):
                    yield (str(k.value), v, k)
        elif isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef, ast.Lambda)):
            args = n.args.posonlyargs + n.args.args
            for a, v in zip(args[len(args) - len(n.args.defaults):], n.args.defaults):
                yield (a.arg, v, a)
            for a, v in zip(n.args.kwonlyargs, n.args.kw_defaults):
                if v is not None:
                    yield (a.arg, v, a)
        elif isinstance(n, ast.Call) and last(n.func) == 'setattr' and (len(n.args) > 2) and isinstance(n.args[1], ast.Constant):
            yield (str(n.args[1].value), n.args[2], n)

def callbacks(tree):
    for n in ast.walk(tree):
        if isinstance(n, (ast.FunctionDef, ast.AsyncFunctionDef)) and (n.name.lower() in CALLBACKS or any((last(d).lower() in DECORATORS for d in n.decorator_list))):
            yield n

def locations(tree, fn):
    for n in ast.walk(fn):
        if isinstance(n, ast.Return) and has_literal(n.value):
            yield (n.value, 'return', '')
        if isinstance(n, (ast.Compare, ast.MatchValue, ast.MatchMapping)) and has_literal(n):
            names = [last(x) for x in ast.walk(n) if isinstance(x, (ast.Name, ast.Attribute))]
            yield (n, 'comparison', ','.join(names))
        if isinstance(n, ast.Call) and last(n.func) in {'compare_digest', 'startswith', 'endswith', 'eq', 'ne', 'contains'} and has_literal(n):
            yield (n, 'comparison-call', last(n.func))
    parents = {c: p for p in ast.walk(tree) for c in ast.iter_child_nodes(p)}
    scopes = [fn]
    p = parents.get(fn)
    while p:
        if isinstance(p, (ast.Module, ast.ClassDef, ast.FunctionDef, ast.AsyncFunctionDef)):
            scopes.append(p)
        p = parents.get(p)
    reads = {x.id for x in ast.walk(fn) if isinstance(x, ast.Name) and isinstance(x.ctx, ast.Load)}
    reads |= {x.attr for x in ast.walk(fn) if isinstance(x, ast.Attribute) and isinstance(x.ctx, ast.Load)}
    for scope in scopes:
        for name, value, node in named(scope):
            parent = node
            while parent in parents and parent is not scope:
                parent = parents[parent]
            cur = node
            nested = False
            while cur in parents and parents[cur] is not scope:
                cur = parents[cur]
                if isinstance(cur, (ast.FunctionDef, ast.AsyncFunctionDef, ast.ClassDef)):
                    nested = True
            if not nested and name in reads and literal(value):
                yield (value, 'bound-name', name)
ROOT = Path(__file__).resolve().parents[2]
WEB = ROOT / 'src/digitalmodel/web'
APP = WEB / 'digitaltwinfeed/app.py'
HELPER = WEB / 'digitaltwinfeed/basic_auth.py'
FOLDERS = {'digitaltwinfeed': 'application', 'dtf': 'separate example', 'example_SPA': 'separate example', 'example_blueprint': 'separate example', 'template_code': 'local template'}
NON_CREDENTIALS = {}
NON_CALLBACKS = {}

def _trees():
    paths = sorted(WEB.rglob('*.py'))
    assert paths, 'web folder: enumeration empty'
    for path in paths:
        relative = path.relative_to(ROOT).as_posix()
        try:
            tree = ast.parse(path.read_bytes())
        except SyntaxError as error:
            pytest.fail(f'{relative}:{error.lineno}:syntax')
        yield (relative, tree)

def _folder(relative):
    return Path(relative).relative_to('src/digitalmodel/web').parts[0]

def _imported_helper(tree):
    return any(isinstance(n, ast.ImportFrom) and n.module == 'basic_auth' and n.level == 0
               and any(a.name == 'basic_auth_password_for' and a.asname in (None, a.name)
                       for a in n.names) for n in tree.body)

def test_no_callback_literal_in_web_folder():
    findings = []
    count = 0
    literal_count = 0
    for path, tree in _trees():
        if FOLDERS.get(_folder(path)) != 'application':
            for node in ast.walk(tree):
                if isinstance(node, ast.ImportFrom) and ((node.module or '').endswith('basic_auth') or any((a.name == 'basic_auth' for a in node.names))):
                    findings.append(f'{path}:{node.lineno}:shared-helper-import')
                elif isinstance(node, ast.Import) and any((a.name.endswith('basic_auth') for a in node.names)):
                    findings.append(f'{path}:{node.lineno}:shared-helper-import')
        for fn in callbacks(tree):
            count += 1
            if (path, fn.name) in NON_CALLBACKS:
                continue
            positions = [f'{path}:{node.lineno}:{form}' for node, form, name in locations(tree, fn)]
            findings.extend(positions)
            literal_count += len(positions)
            kind = FOLDERS.get(_folder(path))
            if kind is None:
                findings.append(f'{path}:{fn.lineno}:unclassified-folder')
            if not any((last(d) == 'get_password' for d in fn.decorator_list)):
                continue
            body = fn.body[1:] if ast.get_docstring(fn) is not None else fn.body
            expected = 'basic_auth_password_for' if kind == 'application' else '_basic_auth_password_for'
            valid = (len(body) == 1 and isinstance(body[0], ast.Return)
                     and isinstance(body[0].value, ast.Call)
                     and isinstance(body[0].value.func, ast.Name)
                     and body[0].value.func.id == expected
                     and not body[0].value.keywords
                     and len(body[0].value.args) == 1
                     and isinstance(body[0].value.args[0], ast.Name)
                     and bool(fn.args.args)
                     and body[0].value.args[0].id == fn.args.args[0].arg)
            if not valid:
                findings.append(f'{path}:{fn.lineno}:credential-provider')
            if (kind == 'application') != _imported_helper(tree):
                findings.append(f'{path}:{fn.lineno}:helper-import')
    assert count, 'web folder: no credential callback found'
    assert not findings, f'{literal_count} literal places; {len(findings) - literal_count} provider places: ' + ', '.join(findings)

def test_no_credential_binding_literal_in_web_folder():
    """Regression pin: no additional credential binding was present at the base."""
    findings = []
    for path, tree in _trees():
        for name, value, node in named(tree):
            if credential(name) and literal(value) and ((path, name) not in NON_CREDENTIALS):
                findings.append(f'{path}:{value.lineno}:credential-binding')
        for node in ast.walk(tree):
            if not isinstance(node, ast.Call) or not node.args:
                continue
            first = node.args[0]
            if isinstance(first, ast.Constant) and isinstance(first.value, str) and (first.value.startswith('DIGITALTWINFEED_') or '_BASIC_AUTH_' in first.value) and (last(node.func) in {'get', 'getenv'}):
                if len(node.args) != 1 or node.keywords:
                    findings.append(f'{path}:{node.lineno}:environment-default')
        for node in ast.walk(tree):
            if isinstance(node, ast.BoolOp) and isinstance(node.op, ast.Or):
                for part in ast.walk(node):
                    if isinstance(part, ast.Call) and part.args and isinstance(part.args[0], ast.Constant) and isinstance(part.args[0].value, str) and ('_BASIC_AUTH_' in part.args[0].value):
                        findings.append(f'{path}:{part.lineno}:environment-fallback')
    assert not findings, ', '.join(findings)

def test_no_signing_literal_in_web_folder():
    """Regression pin covering every module and setattr as well as assignments."""
    findings = []
    for path, tree in _trees():
        for name, value, node in named(tree):
            if name in {'secret_key', 'SECRET_KEY'} and has_literal(value):
                findings.append(f'{path}:{value.lineno}:signing-binding')
    assert not findings, ', '.join(findings)

def _load_helper():
    assert HELPER.is_file(), 'basic_auth.py:helper-missing'
    spec = importlib.util.spec_from_file_location('web_basic_auth_contract', HELPER)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module

def _unset(monkeypatch, prefix='DIGITALTWINFEED'):
    for suffix in ('USER', 'PASSWORD'):
        monkeypatch.delenv(f'{prefix}_BASIC_AUTH_{suffix}', raising=False)

def _assert_closed(provider):
    for presented in ('dummy-user', 'other-user', '', None):
        assert provider(presented) is None, 'basic authentication:closed'

def test_helper_absent(monkeypatch):
    _unset(monkeypatch)
    module = _load_helper()
    assert module.basic_auth_user() is None, 'user:absent'
    assert module.basic_auth_password() is None, 'password:absent'
    _assert_closed(module.basic_auth_password_for)

def test_helper_blank(monkeypatch):
    module = _load_helper()
    for suffix in ('USER', 'PASSWORD'):
        for blank in ('', ' ', '\t\n'):
            _unset(monkeypatch)
            monkeypatch.setenv('DIGITALTWINFEED_BASIC_AUTH_USER', 'dummy-user')
            monkeypatch.setenv('DIGITALTWINFEED_BASIC_AUTH_PASSWORD', 'dummy')
            monkeypatch.setenv(f'DIGITALTWINFEED_BASIC_AUTH_{suffix}', blank)
            accessor = module.basic_auth_user if suffix == 'USER' else module.basic_auth_password
            assert accessor() is None, 'blank:absent'
            _assert_closed(module.basic_auth_password_for)

def test_helper_set_and_live_environment(monkeypatch):
    module = _load_helper()
    _unset(monkeypatch)
    monkeypatch.setenv('DIGITALTWINFEED_BASIC_AUTH_USER', 'dummy-user')
    monkeypatch.setenv('DIGITALTWINFEED_BASIC_AUTH_PASSWORD', 'dummy')
    assert module.basic_auth_user() == 'dummy-user', 'user:environment'
    assert module.basic_auth_password() == 'dummy', 'password:environment'
    assert module.basic_auth_password_for('dummy-user') == 'dummy', 'matching-user'
    for presented in ('other-user', '', None):
        assert module.basic_auth_password_for(presented) is None, 'nonmatching-user'
    for suffix in ('USER', 'PASSWORD'):
        _unset(monkeypatch)
        monkeypatch.setenv(f'DIGITALTWINFEED_BASIC_AUTH_{suffix}', 'dummy-user' if suffix == 'USER' else 'dummy')
        _assert_closed(module.basic_auth_password_for)
    monkeypatch.setenv('DIGITALTWINFEED_BASIC_AUTH_USER', ' '.join(('', 'dummy-user', '')))
    monkeypatch.setenv('DIGITALTWINFEED_BASIC_AUTH_PASSWORD', ' '.join(('', 'dummy', '')))
    assert module.basic_auth_user() == ' '.join(('', 'dummy-user', '')), 'user:unchanged'
    assert module.basic_auth_password() == ' '.join(('', 'dummy', '')), 'password:unchanged'
    assert module.basic_auth_password_for(' '.join(('', 'dummy-user', ''))) == ' '.join(('', 'dummy', '')), 'unchanged'

def test_startup_message(monkeypatch, caplog):
    module = _load_helper()
    monkeypatch.setattr(module, '_startup_checked', False)
    _unset(monkeypatch)
    with caplog.at_level(logging.WARNING):
        module.warn_if_basic_auth_closed()
        module.warn_if_basic_auth_closed()
    assert len(caplog.records) == 1, 'startup:one-warning'
    record = caplog.records[0]
    assert record.levelno == logging.WARNING, 'startup:warning-level'
    for suffix in ('USER', 'PASSWORD'):
        assert f'DIGITALTWINFEED_BASIC_AUTH_{suffix}' in record.message, 'missing-name'
    assert 'repository holds no value' in record.message, 'startup:no-repository-value'
    assert 'every request' in record.message, 'startup:closed-requests'
    caplog.clear()
    monkeypatch.setattr(module, '_startup_checked', False)
    monkeypatch.setenv('DIGITALTWINFEED_BASIC_AUTH_USER', 'dummy-user')
    module.warn_if_basic_auth_closed()
    assert len(caplog.records) == 1, 'startup:missing-password-warning'
    assert all(('dummy-user' not in r.message for r in caplog.records)), 'startup:no-value'
    caplog.clear()
    monkeypatch.setattr(module, '_startup_checked', False)
    monkeypatch.setenv('DIGITALTWINFEED_BASIC_AUTH_PASSWORD', 'dummy')
    module.warn_if_basic_auth_closed()
    assert not caplog.records, 'startup:configured-no-warning'

def test_startup_call_after_signing_binding():
    tree = ast.parse(APP.read_bytes())
    calls = [n for n in ast.walk(tree) if isinstance(n, ast.Call) and last(n.func) == 'warn_if_basic_auth_closed']
    assert len(calls) == 1, 'app.py:one-startup-call'
    statements = [n for n in tree.body if isinstance(n, ast.Expr) and n.value is calls[0]]
    assert len(statements) == 1, 'app.py:module-level-startup'
    signing = [n for name, value, n in named(tree) if name == 'secret_key']
    assert len(signing) == 1, 'app.py:one-signing-binding'
    assert statements[0].lineno > signing[0].lineno, 'app.py:signing-first'
    assert any((isinstance(n, ast.ImportFrom) and n.module == 'basic_auth' and any((a.name == 'warn_if_basic_auth_closed' for a in n.names)) for n in tree.body)), 'app.py:startup-import'

def _local_providers():
    providers = []
    for path, tree in _trees():
        kind = FOLDERS.get(_folder(path))
        registered = [fn for fn in callbacks(tree) if any((last(d) == 'get_password' for d in fn.decorator_list))]
        if not registered or kind == 'application':
            continue
        fn = next((n for n in tree.body if isinstance(n, ast.FunctionDef) and n.name == '_basic_auth_password_for'), None)
        assert fn is not None, f'{path}:local-read-missing'
        assert not _imported_helper(tree), f'{path}:shared-helper-import'
        namespace = {}
        exec(compile(ast.Module(body=[fn], type_ignores=[]), '<local-read>', 'exec'), namespace)
        prefix = re.sub('[^A-Za-z0-9]', '_', _folder(path)).upper()
        providers.append((namespace['_basic_auth_password_for'], prefix))
    assert providers, 'web folder:local-reader-enumeration-empty'
    return providers

def test_local_reads(monkeypatch):
    for provider, prefix in _local_providers():
        _unset(monkeypatch, prefix)
        _assert_closed(provider)
        for suffix in ('USER', 'PASSWORD'):
            for blank in ('', ' ', '\t\n'):
                monkeypatch.setenv(f'{prefix}_BASIC_AUTH_USER', 'dummy-user')
                monkeypatch.setenv(f'{prefix}_BASIC_AUTH_PASSWORD', 'dummy')
                monkeypatch.setenv(f'{prefix}_BASIC_AUTH_{suffix}', blank)
                _assert_closed(provider)
            _unset(monkeypatch, prefix)
            monkeypatch.setenv(f'{prefix}_BASIC_AUTH_{suffix}', 'dummy-user' if suffix == 'USER' else 'dummy')
            _assert_closed(provider)
        monkeypatch.setenv(f'{prefix}_BASIC_AUTH_USER', 'dummy-user')
        monkeypatch.setenv(f'{prefix}_BASIC_AUTH_PASSWORD', 'dummy')
        assert provider('dummy-user') == 'dummy', 'local-read:matching-user'
        for presented in ('other-user', '', None):
            assert provider(presented) is None, 'local-read:nonmatching-user'
