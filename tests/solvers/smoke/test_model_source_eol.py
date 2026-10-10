"""Hash-pinned governing files keep their reviewed LF bytes on every checkout (#2093)."""
import hashlib
import os
from pathlib import Path
import shutil
import subprocess

import pytest

from digitalmodel.solvers.smoke import model_source

REPO = Path(__file__).resolve().parents[3]
PINNED = {
    model_source.GOVERNING_SOURCE: model_source.GOVERNING_SHA256,
    model_source.REFERENCE_SOURCE: model_source.REFERENCE_SHA256,
}
_BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE",
             "GIT_OBJECT_DIRECTORY", "GIT_ALTERNATE_OBJECT_DIRECTORIES",
             "GIT_CEILING_DIRECTORIES", "GIT_CONFIG_PARAMETERS")


def _git(cwd, *args):
    env = {key: value for key, value in os.environ.items() if key not in _BINDINGS}
    return subprocess.run(["git", "-C", str(cwd), *args], env=env, capture_output=True,
                          text=True, check=True).stdout


@pytest.fixture(scope="module")
def checkout():
    if shutil.which("git") is None or not (REPO / ".git").exists():
        pytest.skip("requires a Git checkout of the repository")
    return REPO


@pytest.mark.parametrize("relative", sorted(PINNED))
def test_pinned_governing_file_is_bound_to_lf(checkout, relative):
    out = _git(checkout, "check-attr", "text", "eol", "--", relative)
    assert f"{relative}: text: set" in out
    assert f"{relative}: eol: lf" in out


def test_autocrlf_checkout_reproduces_pinned_digests(checkout, tmp_path):
    repo = tmp_path / "disposable"
    repo.mkdir()
    _git(repo, "init", "-q")
    _git(repo, "config", "core.autocrlf", "true")
    _git(repo, "config", "user.name", "fixture")
    _git(repo, "config", "user.email", "fixture@example.invalid")
    shutil.copyfile(checkout / ".gitattributes", repo / ".gitattributes")
    for relative, pinned in PINNED.items():
        target = repo / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        data = (checkout / relative).read_bytes().replace(b"\r\n", b"\n")
        assert hashlib.sha256(data).hexdigest() == pinned, "repository basis drifted from pin"
        target.write_bytes(data)
    _git(repo, "add", "-A")
    _git(repo, "commit", "-q", "-m", "fixture")
    for relative in PINNED:
        (repo / relative).unlink()
    _git(repo, "checkout", "--", ".")
    for relative, pinned in PINNED.items():
        assert hashlib.sha256((repo / relative).read_bytes()).hexdigest() == pinned
