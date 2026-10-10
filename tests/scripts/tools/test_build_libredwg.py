"""Build gate failures must prevent unverified source execution."""

import os
import subprocess
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[3] / "scripts/tools/build-libredwg.sh"
FINGERPRINT = "38A4167B0DB69E49C5F7216CB8C28866AB27A7A2"
pytestmark = pytest.mark.skipif(os.name != "posix", reason="requires POSIX build tools")


def executable(path, text):
    path.write_text("#!/bin/bash\nset -e\n" + text)
    path.chmod(0o755)


@pytest.fixture
def build_env(tmp_path):
    home, commands = tmp_path / "home", tmp_path / "bin"
    home.mkdir()
    commands.mkdir()
    env = dict(
        {key: value for key, value in os.environ.items() if not key.startswith("GIT_")},
        HOME=str(home),
        TMPDIR=str(tmp_path),
        PATH=f"{commands}:/usr/bin:/bin",
        BUILD_TEST_ROOT=str(tmp_path),
    )
    executable(commands / "pkg-config", "exit 0\n")
    executable(
        commands / "curl",
        """while (( $# )); do
if [[ "$1" == --output ]]; then printf source > "$2"; exit; fi
shift
done
exit 1
""",
    )
    executable(
        commands / "gpgv",
        f'printf "[GNUPG:] VALIDSIG {FINGERPRINT} 2026-01-01 0 0 4 0 1 10 00\\n"\n',
    )
    executable(
        commands / "tar",
        """while (( $# )); do
if [[ "$1" == -C ]]; then target="$2"; break; fi
shift
done
mkdir -p "$target/libredwg-0.14"
cat > "$target/libredwg-0.14/configure" <<'SH'
#!/bin/bash
printf '%s\\n' "$@" > "$BUILD_TEST_ROOT/configure-args"
SH
chmod +x "$target/libredwg-0.14/configure"
""",
    )
    executable(
        commands / "make", 'printf "%s\\n" "$*" >> "$BUILD_TEST_ROOT/make-calls"\n'
    )
    return env, commands, tmp_path


def run_build(env):
    return subprocess.run(
        ["bash", str(SCRIPT)],
        env=env,
        capture_output=True,
        text=True,
        timeout=10,
        check=False,
    )


def accept_digest(commands):
    executable(commands / "sha256sum", "cat >/dev/null; exit 0\n")


def test_archive_digest_mismatch_stops_before_source_execution(build_env):
    env, _, root = build_env
    result = run_build(env)
    assert result.returncode != 0
    assert "checksum" in result.stderr.lower()
    assert not (root / "make-calls").exists()


def test_invalid_signature_stops_before_source_execution(build_env):
    env, commands, root = build_env
    accept_digest(commands)
    executable(commands / "gpgv", "exit 1\n")
    result = run_build(env)
    assert result.returncode != 0
    assert "signature" in result.stderr.lower()
    assert not (root / "make-calls").exists()


def test_other_valid_signer_is_rejected(build_env):
    env, commands, root = build_env
    accept_digest(commands)
    executable(commands / "gpgv", 'printf "[GNUPG:] VALIDSIG ABCDEF 0\\n"\n')
    result = run_build(env)
    assert result.returncode != 0
    assert "signer" in result.stderr.lower()
    assert not (root / "make-calls").exists()


def test_verified_source_installs_to_user_versioned_prefix(build_env):
    env, commands, root = build_env
    accept_digest(commands)
    result = run_build(env)
    assert result.returncode == 0, result.stderr
    assert (
        f"--prefix={env['HOME']}/.local/opt/libredwg-0.14"
        in (root / "configure-args").read_text().splitlines()
    )
    assert (root / "make-calls").read_text().splitlines() == ["-j2", "install"]


def test_existing_prefix_is_preserved(build_env):
    env, _, root = build_env
    prefix = Path(env["HOME"]) / ".local/opt/libredwg-0.14"
    prefix.mkdir(parents=True)
    evidence = prefix / "existing"
    evidence.write_bytes(b"original installation")
    result = run_build(env)
    assert result.returncode != 0
    assert "prefix already exists" in result.stderr
    assert evidence.read_bytes() == b"original installation"
    assert not (root / "make-calls").exists()


def test_missing_release_signature_never_downgrades_to_checksum_only(build_env):
    env, commands, root = build_env
    accept_digest(commands)
    executable(
        commands / "curl",
        """for value in "$@"; do
[[ "$value" == *.sig ]] && exit 22
done
while (( $# )); do
if [[ "$1" == --output ]]; then printf source > "$2"; exit; fi
shift
done
exit 1
""",
    )
    result = run_build(env)
    assert result.returncode != 0
    assert "signature download failed" in result.stderr
    assert not (root / "make-calls").exists()
