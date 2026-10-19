"""Hostile contracts for shared execution guards; no solver is imported.

Expected counts follow hand calculation: requested 2 <= min(4, 3) = 3;
requested 4 > min(4, 3). These are synthetic inputs, not engineering limits.
Locators follow literal POSIX component concatenation, not code-derived output.
"""

import hashlib
import os
from pathlib import Path
import shutil
import sys

import pytest

from digitalmodel.workflows import hosted_execution as guards

requires_sealed_launch = pytest.mark.skipif(
    sys.platform != "linux" or not hasattr(os, "memfd_create"),
    reason="sealed launch requires Linux and Python memfd support",
)


@pytest.fixture(autouse=True)
def isolated_cache_ancestry(tmp_path, monkeypatch):
    """Model an isolated scratch root beneath a potentially Git-owned cache.

    Only ancestors strictly above the fixture root are excluded; production
    still checks every ancestor. Actual fixture directories, markers and
    descriptor opens remain real, including root and nested boundary tests.
    """
    ancestors = {(path.stat().st_dev, path.stat().st_ino) for path in tmp_path.parents}
    check = guards._outside_git

    def within_fixture(fd):
        metadata = os.fstat(fd)
        if (metadata.st_dev, metadata.st_ino) not in ancestors:
            check(fd)

    monkeypatch.setattr(guards, "_outside_git", within_fixture)


@pytest.mark.parametrize("bad", [True, False, 2.0, "2", None, 0, -1])
def test_resource_count_refuses_coercion(bad):
    with pytest.raises((TypeError, ValueError)):
        guards.resource_count(bad, name="ranks")


def test_resource_ceiling_is_strict_for_every_operand():
    assert guards.resource_count(2, name="ranks", ceilings=(4, 3)) == 2
    with pytest.raises(ValueError):
        guards.resource_count(4, name="ranks", ceilings=(4, 3))
    for bad in (True, "4", 4.0, 0):
        with pytest.raises((TypeError, ValueError)):
            guards.resource_count(2, name="ranks", ceilings=(bad,))


@pytest.mark.parametrize(
    "bad",
    [
        "/result",
        "../result",
        "a/../b",
        "a//b",
        "a/./b",
        "C:/result",
        "a\\b",
        "a\x00b",
        "",
        "a\nb",
        ".git/a",
    ],
)
def test_locator_rejects_nonportable_or_escaping_identifiers(bad):
    with pytest.raises((TypeError, ValueError)):
        guards.result_locator(bad)


def test_locator_has_no_absolute_root():
    assert guards.result_locator("batch/case/result.json") == "batch/case/result.json"


@pytest.mark.skipif(
    sys.platform != "linux", reason="descriptor contract requires Linux"
)
@pytest.mark.parametrize("marker", ["directory", "file", "symlink"])
def test_destination_checks_nested_checkout_and_worktree(tmp_path, marker):
    nested = tmp_path / "namespace"
    nested.mkdir()
    git = nested / ".git"
    if marker == "directory":
        git.mkdir()
    elif marker == "file":
        git.write_text("gitdir: ignored\n")
    else:
        git.symlink_to("missing")
    with pytest.raises(ValueError, match="Git"):
        with guards.destination_fd(tmp_path, "namespace"):
            pytest.fail("nested checkout accepted")


@pytest.mark.skipif(
    sys.platform != "linux", reason="descriptor contract requires Linux"
)
def test_destination_rejects_symlinks_and_checks_root_ancestry(tmp_path):
    (tmp_path / "real").mkdir()
    (tmp_path / "alias").symlink_to("real", target_is_directory=True)
    with pytest.raises(OSError):
        with guards.destination_fd(tmp_path, "alias"):
            pytest.fail("symlink accepted")
    (tmp_path / ".git").mkdir()
    with pytest.raises(ValueError, match="Git"):
        with guards.destination_fd(tmp_path / "real", "child"):
            pytest.fail("ancestor checkout accepted")


@pytest.mark.skipif(
    sys.platform != "linux", reason="descriptor contract requires Linux"
)
def test_destination_returns_live_descriptor_and_closes_it(tmp_path):
    (tmp_path / "namespace").mkdir()
    with guards.destination_fd(tmp_path, "namespace") as fd:
        assert os.fstat(fd).st_ino == (tmp_path / "namespace").stat().st_ino
    with pytest.raises(OSError):
        os.fstat(fd)


@pytest.mark.skipif(
    sys.platform != "linux", reason="descriptor contract requires Linux"
)
def test_invalid_requests_never_reach_mutation(tmp_path):
    def mutate(*args):
        (tmp_path / "residue").mkdir()

    with pytest.raises(TypeError):
        guards.validated_request(tmp_path, "missing", True, (4,), mutate)
    assert list(tmp_path.iterdir()) == []
    with pytest.raises(FileNotFoundError):
        guards.validated_request(tmp_path, "missing", 2, (4,), mutate)
    assert list(tmp_path.iterdir()) == []


@pytest.mark.skipif(
    sys.platform != "linux", reason="descriptor contract requires Linux"
)
def test_valid_request_mutates_through_destination_descriptor(tmp_path):
    (tmp_path / "namespace").mkdir()

    def mutate(fd, ranks, locator):
        os.mkdir("result", dir_fd=fd)
        return ranks, locator

    assert guards.validated_request(tmp_path, "namespace", 2, (4, 3), mutate) == (
        2,
        "namespace",
    )
    assert (tmp_path / "namespace" / "result").is_dir()


@pytest.fixture
def harmless_tool(tmp_path):
    # Public coreutils true/echo are test tools, never a licensed solver.
    source = Path(shutil.which("true")).resolve()
    path = tmp_path / "tool"
    shutil.copyfile(source, path)
    path.chmod(source.stat().st_mode)
    return path, hashlib.sha256(path.read_bytes()).hexdigest()


@requires_sealed_launch
def test_launch_binds_bytes_despite_path_replacement_and_inplace_write(
    harmless_tool, tmp_path
):
    path, digest = harmless_tool
    with guards.witnessed_argv([str(path)], {0: digest}) as argv:
        replacement = tmp_path / "replacement"
        replacement.write_bytes(b"invalid executable")
        replacement.replace(path)
        path.write_bytes(b"changed in place")
        assert guards.run_witnessed(argv, timeout=2).returncode == 0
        with pytest.raises(OSError):
            os.write(argv.fds[0], b"tamper")


@requires_sealed_launch
def test_nested_executable_is_bound_and_inherited(harmless_tool):
    path, digest = harmless_tool
    echo = Path(shutil.which("env")).resolve()
    echo_digest = hashlib.sha256(echo.read_bytes()).hexdigest()
    with guards.witnessed_argv(
        [str(echo), str(path)], {0: echo_digest, 1: digest}
    ) as argv:
        result = guards.run_witnessed(argv, timeout=2)
        assert (
            result.returncode == 0
        )  # env actually execs the nested witnessed true binary.
        assert all(os.path.exists(f"/proc/self/fd/{fd}") for fd in argv.fds)
    assert all(not os.path.exists(f"/proc/self/fd/{fd}") for fd in argv.fds)


@requires_sealed_launch
def test_digest_mismatch_and_unbound_launcher_refuse(harmless_tool):
    path, digest = harmless_tool
    with pytest.raises(ValueError, match="digest"):
        with guards.witnessed_argv([str(path)], {0: "0" * len(digest)}):
            pytest.fail("mismatch accepted")
    with pytest.raises(ValueError):
        with guards.witnessed_argv([str(path)], {}):
            pytest.fail("unbound launcher accepted")


@requires_sealed_launch
def test_script_and_symlink_refuse(tmp_path, harmless_tool):
    script = tmp_path / "script"
    script.write_bytes(b"#!/bin/sh\nexit 0\n")
    script.chmod(0o700)  # POSIX user rwx test fixture, not an engineering limit.
    with pytest.raises(ValueError, match="ELF"):
        with guards.witnessed_argv(
            [str(script)], {0: hashlib.sha256(script.read_bytes()).hexdigest()}
        ):
            pytest.fail("unbound interpreter accepted")
    path, digest = harmless_tool
    alias = tmp_path / "alias"
    alias.symlink_to(path)
    with pytest.raises(OSError):
        with guards.witnessed_argv([str(alias)], {0: digest}):
            pytest.fail("symlink accepted")


def test_unsupported_platform_fails_closed(monkeypatch):
    monkeypatch.setattr(guards.sys, "platform", "unsupported")
    with pytest.raises(NotImplementedError):
        with guards.destination_fd(Path("/"), "relative"):
            pytest.fail("unsupported platform accepted")


@pytest.mark.skipif(sys.platform != "linux", reason="Linux capability check")
def test_missing_memfd_fails_closed(monkeypatch, harmless_tool):
    path, digest = harmless_tool
    monkeypatch.delattr(os, "memfd_create", raising=False)
    with pytest.raises(NotImplementedError):
        with guards.witnessed_argv([str(path)], {0: digest}):
            pytest.fail("missing capability accepted")


@requires_sealed_launch
def test_expired_plan_cannot_launch_reused_descriptors(harmless_tool):
    path, digest = harmless_tool
    with guards.witnessed_argv([str(path)], {0: digest}) as plan:
        pass
    with pytest.raises(ValueError, match="expired"):
        guards.run_witnessed(plan, timeout=2)


@requires_sealed_launch
def test_bad_executable_digest_leaves_no_layout_residue(tmp_path, harmless_tool):
    path, digest = harmless_tool
    destination = tmp_path / "namespace"
    destination.mkdir()

    def mutate(fd, ranks, locator, plan):
        os.mkdir("residue", dir_fd=fd)

    with pytest.raises(ValueError, match="digest"):
        guards.validated_launch(
            tmp_path, "namespace", 2, (4,), [str(path)], {0: "0" * len(digest)}, mutate
        )
    assert list(destination.iterdir()) == []


@requires_sealed_launch
@pytest.mark.parametrize("timeout", [True, "2", 0, -1, float("inf"), float("nan")])
def test_invalid_timeout_never_launches(harmless_tool, timeout):
    path, digest = harmless_tool
    with guards.witnessed_argv([str(path)], {0: digest}) as plan:
        with pytest.raises((TypeError, ValueError)):
            guards.run_witnessed(plan, timeout=timeout)


@requires_sealed_launch
def test_partial_witness_failure_closes_previously_opened_snapshots(harmless_tool):
    path, digest = harmless_tool
    descriptors = set(Path("/proc/self/fd").iterdir())
    with pytest.raises(ValueError, match="digest"):
        with guards.witnessed_argv([str(path), str(path)], {0: digest, 1: "invalid"}):
            pytest.fail("bad nested digest accepted")
    assert set(Path("/proc/self/fd").iterdir()) == descriptors


@requires_sealed_launch
def test_inplace_source_write_cannot_change_sealed_launch(harmless_tool):
    path, digest = harmless_tool
    with guards.witnessed_argv([str(path)], {0: digest}) as plan:
        path.write_bytes(b"invalid executable")
        assert guards.run_witnessed(plan, timeout=2).returncode == 0


@requires_sealed_launch
def test_nonzero_exit_is_preserved():
    binary = Path(shutil.which("false")).resolve()
    digest = hashlib.sha256(binary.read_bytes()).hexdigest()
    with guards.witnessed_argv([str(binary)], {0: digest}) as plan:
        # coreutils false(1): unsuccessful exit status, independent of this code.
        assert guards.run_witnessed(plan, timeout=2).returncode != 0


@requires_sealed_launch
def test_validated_launch_uses_checked_destination(tmp_path, harmless_tool):
    path, digest = harmless_tool
    target = tmp_path / "namespace"
    target.mkdir()

    def mutate(fd, ranks, locator, plan):
        os.mkdir("result", dir_fd=fd)
        return guards.run_witnessed(plan, timeout=2, cwd_fd=fd).returncode, locator

    assert guards.validated_launch(
        tmp_path, "namespace", 2, (4,), [str(path)], {0: digest}, mutate
    ) == (0, "namespace")
    assert (target / "result").is_dir()
