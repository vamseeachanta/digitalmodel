"""Opt-in hosted execution contracts; no solver imports or implicit discovery.

Linux descriptor facilities fail closed elsewhere. See
``docs/solvers/hosted-execution.md`` for the trust boundary and adoption gaps.
"""

from __future__ import annotations

from contextlib import contextmanager, ExitStack
from dataclasses import dataclass, field
import errno
import hashlib
import math
import os
from pathlib import Path
import re
import shutil
import stat
import subprocess
import sys
from typing import Callable, Iterable, Iterator, Mapping, Sequence, TypeVar

T = TypeVar("T")
_COMPONENT = re.compile(r"[A-Za-z0-9][A-Za-z0-9_.-]*", re.ASCII)


def resource_count(value: object, *, name: str, ceilings: Iterable[int] = ()) -> int:
    """Require positive built-in integers at every resource boundary.

    Policy: issue 1596 (2026), strict scalar validation; no coercion, including
    bool (a Python int subclass). Ceilings are supplied capabilities, not defaults.
    """
    ceilings = tuple(ceilings)
    if type(value) is not int:
        raise TypeError(f"{name} requires integer counts")
    for item in (value, *ceilings):
        if type(item) is not int:
            raise TypeError(f"{name} requires integer counts")
        if item <= 0:
            raise ValueError(f"{name} requires positive counts")
    if any(value > ceiling for ceiling in ceilings):
        raise ValueError(f"{name} exceeds a supplied ceiling")
    return value


def result_locator(value: str) -> str:
    """Validate a portable relative identifier without normalizing traversal away."""
    if not isinstance(value, str) or not value:
        raise ValueError("result locator requires a relative identifier")
    if any(
        part in {".", "..", ".git"} or not _COMPONENT.fullmatch(part)
        for part in value.split("/")
    ):
        raise ValueError("result locator has invalid components")
    return value


def _linux() -> None:
    if sys.platform != "linux":
        raise NotImplementedError("hosted descriptor contracts require Linux")


def _outside_git(fd: int) -> None:
    try:
        os.stat(".git", dir_fd=fd, follow_symlinks=False)
    except FileNotFoundError:
        return
    raise ValueError("destination ancestry contains a Git boundary")


@contextmanager
def _directory(path: Path, *, check_git: bool) -> Iterator[int]:
    """Walk from filesystem root, never resolving symlinks or dot components."""
    _linux()
    if not path.is_absolute() or ".." in path.parts:
        raise ValueError("directory requires an absolute traversal-free path")
    # Python 3.11 os.open docs: dir_fd anchors relative names; Linux open(2)
    # O_DIRECTORY and O_NOFOLLOW reject non-directories and symlink traversal.
    flags = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW | os.O_CLOEXEC
    with ExitStack() as stack:
        fd = os.open(path.anchor, flags)
        stack.callback(os.close, fd)
        if check_git:
            _outside_git(fd)
        for component in path.parts[1:]:
            fd = os.open(component, flags, dir_fd=fd)
            stack.callback(os.close, fd)
            if check_git:
                _outside_git(fd)
        yield fd


@contextmanager
def destination_fd(root: Path, locator: str) -> Iterator[int]:
    """Open a precreated effective destination, checking its entire ancestry.

    The directory tree must be controlled by the operator throughout use.
    Keep this context open and mutate relative to the returned descriptor.
    Missing destinations fail without creating layout residue.
    """
    relative = result_locator(locator)
    with _directory(Path(root) / relative, check_git=True) as fd:
        yield fd


def validated_request(
    root: Path,
    locator: str,
    ranks: object,
    ceilings: Iterable[int],
    mutate: Callable[[int, int, str], T],
) -> T:
    """Finish every shared request check before invoking a layout/launch callback.

    Workflow-specific pure validators must run before this function; the callback
    must use descriptor-relative operations and return relative result locators.
    """
    ceilings = tuple(ceilings)
    if not ceilings:
        raise ValueError("request requires at least one ceiling")
    count = resource_count(ranks, name="ranks", ceilings=ceilings)
    relative = result_locator(locator)
    with destination_fd(root, relative) as fd:
        return mutate(fd, count, relative)


def _snapshot(path: str, expected: str) -> int:
    """Return a sealed anonymous ELF snapshot matching the supplied SHA-256."""
    _linux()
    import fcntl

    if not hasattr(os, "memfd_create"):
        raise NotImplementedError("Python runtime lacks sealed executable support")
    with _directory(Path(path).parent, check_git=False) as parent:
        source = os.open(
            Path(path).name,
            os.O_RDONLY | os.O_NOFOLLOW | os.O_CLOEXEC | os.O_NONBLOCK,
            dir_fd=parent,
        )
    with os.fdopen(source, "rb") as stream:
        metadata = os.fstat(stream.fileno())
        if not stat.S_ISREG(metadata.st_mode) or not metadata.st_mode & 0o111:
            # Linux man-pages 6.19 inode(7), file mode: user/group/other execute bits.
            raise ValueError("selected executable must be a regular executable file")
        flags = os.MFD_CLOEXEC | os.MFD_ALLOW_SEALING | getattr(os, "MFD_EXEC", 0)
        fd = os.memfd_create("hosted-executable", flags)
        try:
            with os.fdopen(os.dup(fd), "wb") as snapshot:
                shutil.copyfileobj(stream, snapshot)
            # Linux man-pages 6.19 memfd_create(2), sealing example: these seals
            # forbid writing, resizing, and changing the sealing policy.
            seals = (
                fcntl.F_SEAL_WRITE
                | fcntl.F_SEAL_GROW
                | fcntl.F_SEAL_SHRINK
                | fcntl.F_SEAL_SEAL
            )
            fcntl.fcntl(fd, fcntl.F_ADD_SEALS, seals)
            os.lseek(fd, 0, os.SEEK_SET)
            with os.fdopen(os.dup(fd), "rb") as snapshot:
                actual = hashlib.file_digest(snapshot, "sha256").hexdigest()
            if actual != expected:
                raise ValueError("executable digest mismatch")
            # Linux man-pages 6.19 elf(5), e_ident: 0x7f followed by 'ELF'. Scripts
            # would introduce an unwitnessed interpreter and are refused.
            if os.pread(fd, len(b"\x7fELF"), 0) != b"\x7fELF":
                raise ValueError("selected executable must be ELF")
            os.lseek(fd, 0, os.SEEK_SET)
            return fd
        except BaseException:
            os.close(fd)
            raise


@dataclass(frozen=True)
class WitnessedArgv:
    """Borrowed descriptor launch plan; valid only inside witnessed_argv."""

    argv: tuple[str, ...]
    fds: tuple[int, ...]
    _active: bool = field(default=True, init=False, repr=False)


@contextmanager
def witnessed_argv(
    argv: Sequence[str], executable_digests: Mapping[int, str]
) -> Iterator[WitnessedArgv]:
    """Bind argv[0] and all declared nested executable slots to sealed snapshots.

    Nested launchers must preserve inherited descriptors locally. No automatic
    MPI parsing or remote descriptor propagation is claimed.
    """
    _linux()
    if not argv or any(not isinstance(arg, str) or "\0" in arg for arg in argv):
        raise ValueError("argv requires nonempty string arguments")
    positions = tuple(executable_digests)
    if (
        any(type(pos) is not int or pos < 0 or pos >= len(argv) for pos in positions)
        or 0 not in positions
    ):
        raise ValueError("executable slots must include the launcher")
    rewritten = list(argv)
    with ExitStack() as stack:
        fds = []
        for pos in positions:
            fd = _snapshot(argv[pos], executable_digests[pos])
            stack.callback(os.close, fd)
            fds.append(fd)
            rewritten[pos] = f"/proc/self/fd/{fd}"
        plan = WitnessedArgv(tuple(rewritten), tuple(fds))
        try:
            yield plan
        finally:
            object.__setattr__(plan, "_active", False)


def run_witnessed(
    plan: WitnessedArgv, *, timeout: float, cwd_fd: int | None = None
) -> subprocess.CompletedProcess:
    """Run locally with inherited snapshot descriptors and an empty environment.

    Python 3.11 subprocess docs, Popen: pass_fds keeps listed descriptors open;
    executable overrides pathname lookup. Timeout bounds the direct child only.
    """
    _linux()
    if not plan._active:
        raise ValueError("witnessed launch plan has expired")
    if isinstance(timeout, bool) or not isinstance(timeout, (int, float)):
        raise TypeError("timeout must be a positive finite number")
    if not math.isfinite(timeout) or timeout <= 0:
        raise ValueError("timeout must be a positive finite number")
    if not plan.fds or plan.argv[0] not in {f"/proc/self/fd/{fd}" for fd in plan.fds}:
        raise ValueError("launcher must use a witnessed descriptor")
    inherited = plan.fds + (() if cwd_fd is None else (cwd_fd,))
    try:
        return subprocess.run(
            plan.argv,
            executable=plan.argv[0],
            pass_fds=inherited,
            cwd=None if cwd_fd is None else f"/proc/self/fd/{cwd_fd}",
            env={},
            stdin=subprocess.DEVNULL,
            capture_output=True,
            timeout=timeout,
            check=False,
        )
    except OSError as error:
        if error.errno == errno.EACCES:
            raise NotImplementedError(
                "executable memfd launch is denied by the runtime or kernel policy"
            ) from error
        raise


def validated_launch(
    root: Path,
    locator: str,
    ranks: object,
    ceilings: Iterable[int],
    argv: Sequence[str],
    executable_digests: Mapping[int, str],
    mutate: Callable[[int, int, str, WitnessedArgv], T],
) -> T:
    """Validate resources, ancestry and every executable before layout mutation."""

    def prepare(fd: int, count: int, relative: str) -> T:
        with witnessed_argv(argv, executable_digests) as plan:
            return mutate(fd, count, relative, plan)

    return validated_request(root, locator, ranks, ceilings, prepare)
