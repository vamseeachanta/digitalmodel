"""Immutable executable witnesses for external OpenFOAM launches."""

from __future__ import annotations

import hashlib
import os
from contextlib import ExitStack, contextmanager
from dataclasses import dataclass
from pathlib import Path
from stat import S_ISREG
from typing import Iterator, Mapping


@dataclass(frozen=True)
class ExecutableWitness:
    path: Path
    device: int
    inode: int
    size: int
    sha256: str


class BoundExecutable(str):
    """Descriptor-backed argv token retained through kernel exec."""

    def __new__(cls, fd: int):
        value = super().__new__(cls, f"/proc/self/fd/{fd}")
        value.pass_fd = fd
        return value


def _observe_fd(fd: int, path: Path) -> ExecutableWitness:
    stat = os.fstat(fd)
    if not S_ISREG(stat.st_mode):
        raise RuntimeError(f"selected executable is not a regular file: {path.name}")
    os.lseek(fd, 0, os.SEEK_SET)
    digest = hashlib.sha256()
    while chunk := os.read(fd, 65536):
        digest.update(chunk)
    return ExecutableWitness(path, stat.st_dev, stat.st_ino, stat.st_size, digest.hexdigest())


def _observe(path: Path) -> ExecutableWitness:
    fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
    try:
        return _observe_fd(fd, path)
    finally:
        os.close(fd)


class ExecutableSet:
    """Captured paths whose identity is checked around each subprocess."""

    def __init__(self, witnesses: Mapping[str, ExecutableWitness]):
        self._witnesses = dict(witnesses)

    @classmethod
    def capture(cls, selected: Mapping[str, Path]) -> "ExecutableSet":
        return cls({
            name: _observe(Path(os.path.abspath(path)))
            for name, path in selected.items()
        })

    def validate(self, name: str) -> None:
        expected = self._witnesses.get(name)
        if expected is None:
            raise RuntimeError(f"uncaptured executable launch is forbidden: {name}")
        if _observe(expected.path) != expected:
            raise RuntimeError(f"selected executable changed: {name}")

    def validate_all(self) -> None:
        for name in self._witnesses:
            self.validate(name)

    def _open(self, name: str) -> int:
        expected = self._witnesses.get(name)
        if expected is None:
            raise RuntimeError(f"uncaptured executable launch is forbidden: {name}")
        fd = os.open(expected.path, os.O_RDONLY | os.O_NOFOLLOW)
        if _observe_fd(fd, expected.path) != expected:
            os.close(fd)
            raise RuntimeError(f"selected executable changed: {name}")
        return fd

    @contextmanager
    def launch(self, name: str) -> Iterator[BoundExecutable]:
        fd = self._open(name)
        try:
            yield BoundExecutable(fd)
        finally:
            os.close(fd)
            self.validate(name)

    @contextmanager
    def launch_many(self, names: list[str]) -> Iterator[None]:
        for name in names:
            self.validate(name)
        try:
            yield
        finally:
            for name in reversed(names):
                self.validate(name)

    @contextmanager
    def launch_argv(
        self, argv: list[str], executable_names: list[str] | None = None
    ) -> Iterator[list[str]]:
        names = executable_names or [argv[0]]
        if any(name not in argv for name in names):
            raise RuntimeError("declared executable position is absent from argv")
        with ExitStack() as stack:
            bound_names = {name: stack.enter_context(self.launch(name)) for name in names}
            bound = [bound_names.get(token, token) for token in argv]
            yield bound
