"""Git provenance regressions for issue 2732; all repositories are disposable."""

import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import unittest
import warnings

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


def run_contract(root, data, monkeypatch, method='test_required_contract_and_placeholders'):
    base = root / 'manifest'
    base.mkdir(exist_ok=True)
    (base / 'manifest.campaign.json').write_text(json.dumps(data))
    monkeypatch.setattr(contract, 'ROOT', root)
    monkeypatch.setattr(contract, 'BASE', base)
    case = contract.ManifestTests(method)
    getattr(case, method)()


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
    with pytest.raises(unittest.SkipTest, match='pinned blob NOT verified: Git metadata unavailable'):
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


def test_shallow_clone_requires_fetching_pin(repository, tmp_path, monkeypatch):
    source, data, _ = repository
    shallow = tmp_path / 'shallow'
    # file:// makes --depth effective; a local-path clone would copy all objects.
    git(tmp_path, 'clone', '--quiet', '--depth=1', source.as_uri(), str(shallow))
    assert git(shallow, 'rev-parse', '--is-shallow-repository').strip() == b'true'
    with pytest.raises(AssertionError, match=f'Pinned commit unavailable in shallow clone: {data["ref"]}'):
        run_contract(shallow, data, monkeypatch)
    git(shallow, 'fetch', '--quiet', '--depth=1', 'origin', data['ref'])
    run_contract(shallow, data, monkeypatch)
    data['env']['SOURCE_SHA256'] = current_digest(shallow)
    with pytest.raises(AssertionError, match='SOURCE_SHA256'):
        run_contract(shallow, data, monkeypatch)
    data['ref'] = git(shallow, 'rev-parse', f'HEAD:{SPEC_PATH}').decode().strip()
    with pytest.raises(AssertionError, match='Pinned ref does not resolve to a commit'):
        run_contract(shallow, data, monkeypatch)


@pytest.mark.parametrize('warning_filter', ['ignore', 'error'])
def test_checkout_drift_is_advisory_and_visible(repository, monkeypatch, warning_filter):
    root, data, _ = repository
    for content in (b'component: second\n', b'uncommitted: change\n'):
        (root / SPEC_PATH).write_bytes(content)
        # The advisory must remain visible even under the suite's warning filters.
        with pytest.warns(UserWarning, match='Advisory: checkout specification differs from pinned blob'):
            warnings.simplefilter(warning_filter, UserWarning)
            run_contract(root, data, monkeypatch, 'test_checkout_spec_matches_pin_advisory')
        run_contract(root, data, monkeypatch)


def test_matching_checkout_has_no_drift_advisory(repository, monkeypatch):
    root, data, _ = repository
    (root / SPEC_PATH).write_bytes(git(root, 'cat-file', 'blob', f'{data["ref"]}:{SPEC_PATH}'))
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter('always')
        run_contract(root, data, monkeypatch, 'test_checkout_spec_matches_pin_advisory')
    assert not caught


def test_missing_git_executable_explicitly_skips_verification(repository, monkeypatch):
    root, data, _ = repository

    def missing_git(*args, **kwargs):
        raise FileNotFoundError('git')

    monkeypatch.setattr(contract.subprocess, 'run', missing_git)
    with pytest.raises(unittest.SkipTest, match='pinned blob NOT verified: git executable missing'):
        run_contract(root, data, monkeypatch)
