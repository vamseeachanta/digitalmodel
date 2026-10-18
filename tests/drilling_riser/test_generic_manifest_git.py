"""Git provenance regressions for issue 2732; all repositories are disposable."""

import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import unittest

import pytest

from . import test_generic_manifest as contract


SPEC_PATH = 'docs/domains/orcaflex/library/templates/drilling_riser/spec.yml'
MANIFEST = json.loads((contract.BASE / 'manifest.campaign.json').read_text())


def git(root, *args):
    env = {key: value for key, value in os.environ.items()
           if key not in {'GIT_DIR', 'GIT_WORK_TREE', 'GIT_COMMON_DIR'}}
    return subprocess.check_output(['git', '-C', str(root), *args], env=env)


@pytest.fixture
def repository(tmp_path):
    root = tmp_path / 'repo'
    root.mkdir()
    git(root, 'init', '-q')
    git(root, 'config', 'core.autocrlf', 'false')
    git(root, 'config', 'user.name', 'Test Maintainer')
    git(root, 'config', 'user.email', 'maintainer@example.invalid')
    git(root, 'commit', '-q', '--allow-empty', '-m', 'Empty reference')
    empty_ref = git(root, 'rev-parse', 'HEAD').decode().strip()
    spec = root / SPEC_PATH
    spec.parent.mkdir(parents=True)
    spec.write_bytes(b'component: first\r\n')
    git(root, 'add', SPEC_PATH)
    git(root, 'commit', '-q', '-m', 'First specification')
    first_ref = git(root, 'rev-parse', 'HEAD').decode().strip()
    first_digest = hashlib.sha256(spec.read_bytes()).hexdigest()
    spec.write_bytes(b'component: second\n')
    git(root, 'add', SPEC_PATH)
    git(root, 'commit', '-q', '-m', 'Second specification')
    data = json.loads(json.dumps(MANIFEST))
    data['ref'] = first_ref
    data['env']['SOURCE_SHA256'] = first_digest
    return root, data, empty_ref


def run_contract(root, data, monkeypatch):
    base = root / 'manifest'
    base.mkdir(exist_ok=True)
    (base / 'manifest.campaign.json').write_text(json.dumps(data))
    monkeypatch.setattr(contract, 'ROOT', root)
    monkeypatch.setattr(contract, 'BASE', base)
    case = contract.ManifestTests('test_required_contract_and_placeholders')
    case.test_required_contract_and_placeholders()


def current_digest(root):
    return hashlib.sha256((root / SPEC_PATH).read_bytes()).hexdigest()


def test_fabricated_ref_is_rejected(repository, monkeypatch):
    root, data, _ = repository
    data['ref'] = 'f' * 40
    data['env']['SOURCE_SHA256'] = current_digest(root)
    with pytest.raises(AssertionError, match='commit'):
        run_contract(root, data, monkeypatch)


def test_digest_from_different_commit_is_rejected(repository, monkeypatch):
    root, data, _ = repository
    data['env']['SOURCE_SHA256'] = current_digest(root)
    with pytest.raises(AssertionError, match='SOURCE_SHA256'):
        run_contract(root, data, monkeypatch)


def test_pinned_raw_bytes_ignore_current_and_dirty_spec(repository, monkeypatch):
    root, data, _ = repository
    # CRLF at the pin is hashed as received, without text newline translation.
    run_contract(root, data, monkeypatch)
    (root / SPEC_PATH).write_bytes(b'uncommitted: change\n')
    run_contract(root, data, monkeypatch)


def test_blob_ref_is_not_a_commit(repository, monkeypatch):
    root, data, _ = repository
    data['ref'] = git(root, 'rev-parse', f'HEAD:{SPEC_PATH}').decode().strip()
    data['env']['SOURCE_SHA256'] = current_digest(root)
    with pytest.raises(AssertionError, match='commit'):
        run_contract(root, data, monkeypatch)


def test_missing_pinned_spec_is_rejected(repository, monkeypatch):
    root, data, empty_ref = repository
    data['ref'] = empty_ref
    data['env']['SOURCE_SHA256'] = current_digest(root)
    with pytest.raises(AssertionError, match='blob'):
        run_contract(root, data, monkeypatch)


def test_no_git_metadata_skips_pin_validation(repository, monkeypatch):
    root, data, _ = repository
    shutil.rmtree(root / '.git')
    with pytest.raises(unittest.SkipTest, match='Git metadata'):
        run_contract(root, data, monkeypatch)


def test_no_git_metadata_still_checks_required_fields(repository, monkeypatch):
    root, data, _ = repository
    shutil.rmtree(root / '.git')
    data['progress']['total'] = 0
    with pytest.raises(AssertionError):
        run_contract(root, data, monkeypatch)


def test_inherited_git_bindings_do_not_redirect_or_mutate(repository, tmp_path, monkeypatch):
    root, data, _ = repository
    caller = tmp_path / 'caller'
    caller.mkdir()
    git(caller, 'init', '-q')
    sentinel = caller / 'sentinel.txt'
    sentinel.write_bytes(b'preserve caller files\n')
    git(caller, 'add', 'sentinel.txt')
    paths = [caller / '.git/index', caller / '.git/HEAD', sentinel]
    before = {path: path.read_bytes() for path in paths}
    for key, path in [('GIT_DIR', caller / '.git'), ('GIT_WORK_TREE', caller),
                      ('GIT_COMMON_DIR', caller / '.git')]:
        monkeypatch.setenv(key, str(path))
    run_contract(root, data, monkeypatch)
    assert {path: path.read_bytes() for path in paths} == before
    data['ref'] = 'f' * 40
    data['env']['SOURCE_SHA256'] = current_digest(root)
    with pytest.raises(AssertionError, match='commit'):
        run_contract(root, data, monkeypatch)
    assert {path: path.read_bytes() for path in paths} == before


def test_corrupt_metadata_fails_instead_of_skipping(repository, monkeypatch):
    root, data, _ = repository
    shutil.rmtree(root / '.git')
    (root / '.git').write_text('gitdir: missing-metadata\n')
    with pytest.raises(AssertionError, match='commit'):
        run_contract(root, data, monkeypatch)


def test_worktree_git_file_is_supported(repository, tmp_path, monkeypatch):
    root, data, _ = repository
    worktree = tmp_path / 'linked'
    git(root, 'worktree', 'add', '--quiet', '--detach', str(worktree), 'HEAD')
    assert (worktree / '.git').is_file()
    try:
        run_contract(worktree, data, monkeypatch)
    finally:
        git(root, 'worktree', 'remove', '--force', str(worktree))
