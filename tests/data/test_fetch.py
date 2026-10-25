# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Exercise retrieval against disposable stores and repository roots."""
import hashlib
import importlib.util
import os
from pathlib import Path
import subprocess
import sys
import stat
from types import SimpleNamespace

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/data/fetch.py"


@pytest.fixture
def fetch_module():
    spec = importlib.util.spec_from_file_location("store_fetch", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def store_case(tmp_path, monkeypatch):
    root = tmp_path / "repo"
    root.mkdir()
    (root / "data").mkdir()
    payload = b"stored payload\n"
    digest = hashlib.sha256(payload).hexdigest()
    store_id = f"blobs/sha256/{digest[:2]}/{digest}.dat"
    (root / "data/MANIFEST.tsv").write_text(
        "path\tbytes\tsha256\tstore_id\n"
        f"data/sample.dat\t{len(payload)}\t{digest}\t{store_id}\n",
        encoding="utf-8", newline="\n",
    )
    store = tmp_path / "store"
    blob = store / "data/digitalmodel" / store_id
    blob.parent.mkdir(parents=True)
    blob.write_bytes(payload)
    monkeypatch.setenv("ACE_DATA_ROOT", str(store))
    monkeypatch.delenv("ACE_DATA_REMOTE", raising=False)
    return root, blob, store_id, payload


@pytest.mark.parametrize("selector", ["data/sample.dat", "data/*.dat", "id"])
def test_fetch_by_path_glob_or_id(fetch_module, store_case, selector):
    root, _, store_id, payload = store_case
    selector = store_id if selector == "id" else selector
    assert fetch_module.main([selector], repo_root=root) == 0
    assert (root / "data/sample.dat").read_bytes() == payload


def test_corrupt_blob_preserves_destination(fetch_module, store_case):
    root, blob, store_id, _ = store_case
    blob.write_bytes(b"damaged payload")
    target = root / "data/sample.dat"
    target.write_bytes(b"keep existing bytes")
    assert fetch_module.main([store_id], repo_root=root) != 0
    assert target.read_bytes() == b"keep existing bytes"


def test_no_store_prints_instruction(fetch_module, store_case, monkeypatch, capsys):
    root, _, store_id, _ = store_case
    monkeypatch.delenv("ACE_DATA_ROOT")
    assert fetch_module.main([store_id], repo_root=root) != 0
    assert "ACE_DATA_ROOT" in capsys.readouterr().err
    assert not (root / "data/sample.dat").exists()


def test_reachable_store_missing_blob_fails(fetch_module, store_case):
    root, blob, _, _ = store_case
    row = fetch_module.read_manifest(root)[0]
    blob.unlink()
    with pytest.raises(FileNotFoundError) as error:
        fetch_module.fetch_row(root, row)
    assert not isinstance(error.value, fetch_module.StoreUnavailable)


def test_missing_mounted_blob_is_not_masked_by_remote(fetch_module, store_case, monkeypatch):
    root, blob, _, _ = store_case
    blob.unlink()
    monkeypatch.setenv("ACE_DATA_REMOTE", "store-role:archive")

    def unreachable(*args, **kwargs):
        raise subprocess.TimeoutExpired("scp", 120)

    monkeypatch.setattr(fetch_module.subprocess, "run", unreachable)
    with pytest.raises(FileNotFoundError) as error:
        fetch_module.fetch_row(root, fetch_module.read_manifest(root)[0])
    assert not isinstance(error.value, fetch_module.StoreUnavailable)


def test_malformed_manifest_is_reported(fetch_module, store_case, capsys):
    root, _, store_id, _ = store_case
    (root / "data/MANIFEST.tsv").write_text(
        "path\tbytes\tsha256\tstore_id\ndata/sample.dat\n", encoding="utf-8", newline="\n",
    )
    assert fetch_module.main([store_id], repo_root=root) != 0
    assert "Invalid manifest" in capsys.readouterr().err


def test_unreachable_mount_is_unavailable(fetch_module, store_case, monkeypatch, tmp_path):
    root, _, _, _ = store_case
    monkeypatch.setenv("ACE_DATA_ROOT", str(tmp_path / "unmounted"))
    with pytest.raises(fetch_module.StoreUnavailable):
        fetch_module.fetch_row(root, fetch_module.read_manifest(root)[0])


def test_unset_cli_is_nonzero(monkeypatch):
    monkeypatch.delenv("ACE_DATA_ROOT", raising=False)
    monkeypatch.delenv("ACE_DATA_REMOTE", raising=False)
    result = subprocess.run(
        [sys.executable, str(SCRIPT), "missing"],
        capture_output=True, text=True, env=os.environ.copy(),
    )
    assert result.returncode != 0
    assert "No manifest entry matches" in result.stderr


def test_path_traversal_rejected(fetch_module, store_case):
    root, _, store_id, payload = store_case
    manifest = root / "data/MANIFEST.tsv"
    manifest.write_text(
        manifest.read_text(encoding="utf-8").replace("data/sample.dat", "../escape.dat"),
        encoding="utf-8", newline="\n",
    )
    assert fetch_module.main([store_id], repo_root=root) != 0
    assert not (root.parent / "escape.dat").exists()


def test_symlink_destination_rejected(fetch_module, store_case, tmp_path):
    root, _, store_id, _ = store_case
    outside = tmp_path / "outside"
    outside.mkdir()
    (root / "data/sample.dat").symlink_to(outside / "sample.dat")
    assert fetch_module.main([store_id], repo_root=root) != 0
    assert not (outside / "sample.dat").exists()


def test_hardlinked_destination_preserves_other_link(fetch_module, store_case, tmp_path):
    root, _, store_id, payload = store_case
    outside = tmp_path / "shared.dat"
    outside.write_bytes(b"preserve other link")
    (root / "data/sample.dat").hardlink_to(outside)
    assert fetch_module.main([store_id], repo_root=root) == 0
    assert outside.read_bytes() == b"preserve other link"
    assert (root / "data/sample.dat").read_bytes() == payload


def test_restored_file_is_readable(fetch_module, store_case):
    root, _, store_id, _ = store_case
    assert fetch_module.main([store_id], repo_root=root) == 0
    assert (root / "data/sample.dat").stat().st_mode & 0o444 == 0o444


def test_remote_fetch_verifies_bytes(fetch_module, store_case, monkeypatch):
    root, blob, store_id, payload = store_case
    monkeypatch.delenv("ACE_DATA_ROOT")
    monkeypatch.setenv("ACE_DATA_REMOTE", "store-role:archive")
    commands = []

    def transfer(command, **kwargs):
        commands.append(command)
        Path(command[-1]).write_bytes(payload)
        return subprocess.CompletedProcess(command, 0)

    monkeypatch.setattr(fetch_module.subprocess, "run", transfer)
    assert fetch_module.main([store_id], repo_root=root) == 0
    assert commands[0][0] == "scp"
    assert commands[0][-2].endswith("/data/digitalmodel/" + store_id)
    assert (root / "data/sample.dat").read_bytes() == payload


@pytest.mark.parametrize("diagnostic,exception", [
    (b"Connection refused", "StoreUnavailable"),
    (b"Could not resolve hostname", "StoreUnavailable"),
    (b"Permission denied", "ValueError"),
    (b"No such file or directory", "ValueError"),
])
def test_remote_failure_layer(fetch_module, store_case, monkeypatch, diagnostic, exception):
    root, _, store_id, _ = store_case
    monkeypatch.delenv("ACE_DATA_ROOT")
    monkeypatch.setenv("ACE_DATA_REMOTE", "store-role:archive")

    def transfer(command, **kwargs):
        stderr = b"" if "-q" in command else diagnostic
        raise subprocess.CalledProcessError(255, command, stderr=stderr)

    monkeypatch.setattr(fetch_module.subprocess, "run", transfer)
    error_type = ValueError if exception == "ValueError" else fetch_module.StoreUnavailable
    with pytest.raises(error_type):
        fetch_module.fetch_row(root, fetch_module.select_rows(fetch_module.read_manifest(root), store_id)[0])


def test_store_marker_skip_reason(tmp_path, monkeypatch):
    monkeypatch.delenv("ACE_DATA_ROOT", raising=False)
    monkeypatch.delenv("ACE_DATA_REMOTE", raising=False)
    probe = tmp_path / "probe.py"
    probe.write_text(
        "import pytest\n"
        "from importlib.util import spec_from_file_location, module_from_spec\n"
        f"s = spec_from_file_location('store_fixtures', {str(SCRIPT.parents[2] / 'tests/conftest.py')!r})\n"
        "m = module_from_spec(s)\ns.loader.exec_module(m)\n"
        "store_file = m.store_file\n"
        "store_data_marker = m.store_data_marker\n"
        "@pytest.mark.store_data('missing-id')\n"
        "def test_missing():\n    raise AssertionError('must skip')\n",
        encoding="utf-8", newline="\n",
    )
    result = subprocess.run(
        [sys.executable, "-m", "pytest", "-c", str(SCRIPT.parents[2] / "pytest.ini"), str(probe), "-rs", "-o",
         f"cache_dir={tmp_path / 'pytest-cache'}"],
        cwd=SCRIPT.parents[2], capture_output=True, text=True,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "store_data: missing-id not available (store not reachable); run scripts/data/fetch.py missing-id" in result.stdout


def test_bare_store_marker_skip_reason(tmp_path, monkeypatch):
    monkeypatch.delenv("ACE_DATA_ROOT", raising=False)
    monkeypatch.delenv("ACE_DATA_REMOTE", raising=False)
    probe = tmp_path / "probe.py"
    probe.write_text(
        "import pytest\n"
        "from importlib.util import spec_from_file_location, module_from_spec\n"
        f"s = spec_from_file_location('store_fixtures', {str(SCRIPT.parents[2] / 'tests/conftest.py')!r})\n"
        "m = module_from_spec(s)\ns.loader.exec_module(m)\n"
        "store_file = m.store_file\n"
        "store_data_marker = m.store_data_marker\n"
        "@pytest.mark.store_data\n"
        "def test_missing():\n    raise AssertionError('must skip')\n",
        encoding="utf-8", newline="\n",
    )
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        env.pop(key, None)
    result = subprocess.run(
        [sys.executable, "-m", "pytest", "-c", str(SCRIPT.parents[2] / "pytest.ini"), str(probe), "-rs", "-o",
         f"cache_dir={tmp_path / 'pytest-cache'}"],
        cwd=SCRIPT.parents[2], capture_output=True, text=True, env=env,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "store_data: <id> not available (store not reachable); run scripts/data/fetch.py <id>" in result.stdout


@pytest.mark.parametrize("path", [".git/config", "data/.GIT/index", "data/.git. /config",
                                  "../escape.dat", "/absolute.dat", "data/./sample.dat"])
def test_direct_destination_rejects_invalid_paths(fetch_module, store_case, path):
    root, _, _, _ = store_case
    row = dict(fetch_module.read_manifest(root)[0], path=path)
    with pytest.raises(ValueError):
        fetch_module.destination(root, row)


@pytest.mark.parametrize("path", [".git/config", "data/.GIT/index", "data/.git. /config"])
def test_manifest_rejects_git_metadata(fetch_module, store_case, path):
    root, _, _, _ = store_case
    manifest = root / "data/MANIFEST.tsv"
    manifest.write_text(
        manifest.read_text(encoding="utf-8").replace("data/sample.dat", path),
        encoding="utf-8", newline="\n",
    )
    with pytest.raises(ValueError):
        fetch_module.read_manifest(root)


@pytest.mark.skipif(os.name != "posix", reason="POSIX creation-mask semantics")
@pytest.mark.parametrize("mask", [0o777, 0o477, 0o077])
def test_restrictive_mask_preserves_creator_access(fetch_module, store_case, mask):
    """Python 3.11 stat flags: creator read/write = S_IRUSR | S_IWUSR.

    Group/other bits are removed by each supplied mask; no execute bits survive.
    TemporaryDirectory is created first because a 0777 mask also blocks traversal.
    """
    root, _, _, payload = store_case
    (root / "data/sample.dat").write_bytes(b"replace")
    old_mask = os.umask(mask)
    try:
        target = fetch_module.fetch_row(root, fetch_module.read_manifest(root)[0])
    finally:
        os.umask(old_mask)
    assert target.read_bytes() == payload
    assert stat.S_IMODE(target.stat().st_mode) == stat.S_IRUSR | stat.S_IWUSR


def test_verified_existing_file_needs_no_store(fetch_module, store_case, monkeypatch):
    root, _, store_id, payload = store_case
    target = root / "data/sample.dat"
    target.write_bytes(payload)
    before = target.stat().st_ino
    monkeypatch.delenv("ACE_DATA_ROOT")
    assert fetch_module.main([store_id], repo_root=root) == 0
    assert target.stat().st_ino == before


@pytest.mark.skipif(os.name != "posix", reason="POSIX read-permission semantics")
@pytest.mark.parametrize("valid_blob", [True, False])
def test_unreadable_same_size_destination_requires_verified_restore(
        fetch_module, store_case, valid_blob):
    root, blob, _, payload = store_case
    target = root / "data/sample.dat"
    original = b"x" * len(payload)
    target.write_bytes(original)
    target.chmod(0)
    if not valid_blob:
        blob.write_bytes(b"y" * len(payload))
    try:
        row = fetch_module.read_manifest(root)[0]
        if valid_blob:
            assert fetch_module.fetch_row(root, row) == target
            assert target.read_bytes() == payload
        else:
            with pytest.raises(ValueError, match="SHA-256 or size verification"):
                fetch_module.fetch_row(root, row)
            assert stat.S_IMODE(target.stat().st_mode) == 0
            target.chmod(stat.S_IRUSR | stat.S_IWUSR)
            assert target.read_bytes() == original
    finally:
        target.chmod(stat.S_IRUSR | stat.S_IWUSR)


@pytest.mark.parametrize("component", ["data", "data/sample.dat"])
@pytest.mark.parametrize("existing", [True, False])
def test_reparse_point_destination_rejected(
        fetch_module, store_case, monkeypatch, component, existing):
    root, _, _, payload = store_case
    target = root / "data/sample.dat"
    if existing:
        target.write_bytes(payload)
    original_lstat = Path.lstat

    def attributes(path, *args, **kwargs):
        if path == root / component:
            return SimpleNamespace(st_mode=stat.S_IFDIR,
                                   st_file_attributes=stat.FILE_ATTRIBUTE_REPARSE_POINT)
        return original_lstat(path, *args, **kwargs)

    monkeypatch.setattr(Path, "lstat", attributes)
    with pytest.raises(ValueError, match="destination"):
        fetch_module.fetch_row(root, fetch_module.read_manifest(root)[0])
    if existing:
        assert target.read_bytes() == payload
    else:
        assert not target.exists()


def test_transfer_timeout_is_not_a_connection_failure(fetch_module, store_case, monkeypatch):
    root, _, _, _ = store_case
    monkeypatch.delenv("ACE_DATA_ROOT")
    monkeypatch.setenv("ACE_DATA_REMOTE", "store-role:archive")

    def transfer(command, **kwargs):
        raise subprocess.TimeoutExpired(command, kwargs["timeout"])

    monkeypatch.setattr(fetch_module.subprocess, "run", transfer)
    with pytest.raises(TimeoutError, match="transfer duration"):
        fetch_module.fetch_row(root, fetch_module.read_manifest(root)[0])


def test_remote_diagnostics_force_c_locale(fetch_module, store_case, monkeypatch):
    root, _, _, _ = store_case
    monkeypatch.delenv("ACE_DATA_ROOT")
    monkeypatch.setenv("ACE_DATA_REMOTE", "store-role:archive")
    monkeypatch.setenv("LC_ALL", "unavailable-locale")

    def transfer(command, **kwargs):
        assert kwargs["env"]["LC_ALL"] == "C"
        assert kwargs["env"]["LANG"] == "C"
        raise subprocess.CalledProcessError(255, command, stderr=b"Connection refused")

    monkeypatch.setattr(fetch_module.subprocess, "run", transfer)
    with pytest.raises(fetch_module.StoreUnavailable):
        fetch_module.fetch_row(root, fetch_module.read_manifest(root)[0])


@pytest.mark.skipif(os.name != "posix", reason="POSIX creation-mask semantics")
def test_restrictive_mask_creates_traversable_parent(fetch_module, store_case):
    root, _, _, payload = store_case
    row = dict(fetch_module.read_manifest(root)[0], path="new/nested/sample.dat")
    old_mask = os.umask(0o777)
    try:
        target = fetch_module.fetch_row(root, row)
    finally:
        os.umask(old_mask)
    assert target.read_bytes() == payload
    assert stat.S_IMODE(target.parent.stat().st_mode) == stat.S_IRWXU


@pytest.mark.parametrize("value", ["0", "-1", "nan", "inf"])
def test_invalid_transfer_limit_is_rejected(fetch_module, store_case, value, capsys):
    root, _, store_id, _ = store_case
    assert fetch_module.main([store_id, "--transfer-timeout", value], repo_root=root) != 0
    assert "finite and positive" in capsys.readouterr().err
