"""Deployment acceptance is exact byte equality, per issue 2059.

Expected results are independent fixtures: committed bytes are b'original\\n';
a same-length b'tampered\\n' replacement must fail even with equal mtime.
No engineering coefficients or licensed solver execution are involved.
"""

import os
from pathlib import Path
import shutil
import subprocess

import pytest

ROOT = Path(__file__).resolve().parents[2]


@pytest.fixture
def deployment(tmp_path, request):
    repo = tmp_path / "repo"
    scripts = repo / "scripts/cfd"
    scripts.mkdir(parents=True)
    shutil.copytree(ROOT / "scripts/cfd/lib", scripts / "lib")
    shutil.copy(ROOT / "scripts/cfd/deploy_lane.sh", scripts)
    (scripts / "payload.sh").write_bytes(b"original\n")
    for name in ("stage45_driver.sh", "mesh_store.sh", "prune_case.sh", "solve_case.sh"):
        content = b"if then\n" if getattr(request, "param", None) == name else b"exit 0\n"
        (scripts / name).write_bytes(content)
    package = repo / "src/digitalmodel"
    package.mkdir(parents=True)
    (package / "payload.py").write_bytes(b"original\n")
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    for args in (["init", "-q"], ["add", "."]):
        subprocess.run(["git", *args], cwd=repo, env=env, check=True)
    # A detached tree fixture needs no commit or identity override.
    tree = subprocess.check_output(
        ["git", "write-tree"], cwd=repo, env=env, text=True
    ).strip()
    (repo / ".git/HEAD").write_text(tree + "\n")
    remote = tmp_path / "remote"
    remote.mkdir()
    commands = tmp_path / "commands"
    commands.mkdir()
    ssh = commands / "ssh"
    ssh.write_text("""#!/usr/bin/env bash
set -eu
shift 5
if [[ "$*" == *'/proc/'* ]]; then echo '0 0'; exit; fi
cd "$REMOTE"
HOME="$REMOTE" bash -c "$*"
""")
    rsync = commands / "rsync"
    rsync.write_text("""#!/usr/bin/env python3
import os, subprocess, sys
from pathlib import Path
args = [a.split(":", 1)[1] if a.startswith("target:") else a for a in sys.argv[1:]]
if "--checksum" in args and "--dry-run" in args and os.environ.get("BREAK"):
    payload = Path(os.environ["REMOTE"]) / os.environ["BREAK"]
    stat = payload.stat()
    payload.write_bytes(b"tampered\\n")
    os.utime(payload, ns=(stat.st_atime_ns, stat.st_mtime_ns))
if "--checksum" in args and "--dry-run" in args and os.environ.get("FAIL_VERIFY"):
    sys.exit(23)
os.chdir(os.environ["REMOTE"])
sys.exit(subprocess.call([os.environ["REAL_RSYNC"], *args]))
""")
    for command in (ssh, rsync):
        command.chmod(0o755)
    # deploy_lane.sh fails closed unless TMPDIR names its scratch directory;
    # hosted CI runners do not export TMPDIR, so the fixture supplies one.
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    env.update(
        PATH=f"{commands}:{env['PATH']}",
        REMOTE=str(remote),
        REAL_RSYNC=shutil.which("rsync"),
        TMPDIR=str(scratch),
    )
    return repo, remote, env, tree


def run_deploy(deployment, *args, **extra_env):
    repo, remote, env, tree = deployment
    result = subprocess.run(
        ["bash", str(repo / "scripts/cfd/deploy_lane.sh"), *args, "target", "campaign"],
        env={**env, **extra_env},
        cwd=repo,
        text=True,
        capture_output=True,
    )
    return result, remote / "cfd/campaign/scripts/DEPLOYED.txt", tree


def test_verified_stamp_identifies_exact_revision(deployment):
    result, stamp, tree = run_deploy(deployment)
    assert result.returncode == 0, result.stderr
    assert f"digitalmodel@{tree}" in stamp.read_text()
    assert "parity=verified" in stamp.read_text()


@pytest.mark.parametrize(
    "deployment",
    ["stage45_driver.sh", "mesh_store.sh", "prune_case.sh", "solve_case.sh"],
    indirect=True,
)
@pytest.mark.parametrize("historical_stamp", [False, True])
def test_invalid_remote_script_prevents_new_stamp(deployment, historical_stamp):
    _, remote, _, _ = deployment
    stamp = remote / "cfd/campaign/scripts/DEPLOYED.txt"
    previous = b"previous verified deployment\n"
    if historical_stamp:
        stamp.parent.mkdir(parents=True)
        stamp.write_bytes(previous)

    result, stamp, _ = run_deploy(deployment)

    assert result.returncode != 0
    assert "syntax error" in result.stderr
    assert "remote scripts parse OK" not in result.stdout
    if historical_stamp:
        assert stamp.read_bytes() == previous
    else:
        assert not stamp.exists()


@pytest.mark.parametrize(
    "path", ["cfd/campaign/scripts/payload.sh", "cfd/dm_src/digitalmodel/payload.py"]
)
def test_same_size_same_mtime_corruption_prevents_stamp(deployment, path):
    result, stamp, _ = run_deploy(deployment, BREAK=path)
    assert result.returncode != 0
    assert "parity" in result.stderr
    assert not stamp.exists()


def test_verification_transport_failure_prevents_stamp(deployment):
    result, stamp, _ = run_deploy(deployment, FAIL_VERIFY="yes")
    assert result.returncode != 0
    assert not stamp.exists()


def test_deploy_uses_repository_bytes_not_dirty_worktree(deployment):
    repo, remote, _, _ = deployment
    (repo / "scripts/cfd/payload.sh").write_bytes(b"tampered\n")
    result, _, _ = run_deploy(deployment)
    assert result.returncode == 0, result.stderr
    assert (remote / "cfd/campaign/scripts/payload.sh").read_bytes() == b"original\n"


def test_dry_run_never_writes_remote(deployment):
    result, stamp, _ = run_deploy(deployment, "--dry-run")
    assert result.returncode == 0, result.stderr
    assert not stamp.exists()


def test_failed_redeployment_preserves_previous_stamp(deployment):
    result, stamp, _ = run_deploy(deployment)
    assert result.returncode == 0, result.stderr
    previous = stamp.read_bytes()
    result, _, _ = run_deploy(deployment, FAIL_VERIFY="yes")
    assert result.returncode != 0
    assert stamp.read_bytes() == previous


def test_existing_same_size_same_mtime_drift_is_repaired(deployment):
    result, _, _ = run_deploy(deployment)
    assert result.returncode == 0, result.stderr
    _, remote, _, _ = deployment
    payload = remote / "cfd/campaign/scripts/payload.sh"
    stat = payload.stat()
    payload.write_bytes(b"tampered\n")
    os.utime(payload, ns=(stat.st_atime_ns, stat.st_mtime_ns))
    result, _, _ = run_deploy(deployment)
    assert result.returncode == 0, result.stderr
    assert payload.read_bytes() == b"original\n"
