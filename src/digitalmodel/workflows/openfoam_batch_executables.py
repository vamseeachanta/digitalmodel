"""Immutable executable witnesses for external OpenFOAM launches."""

from __future__ import annotations

import hashlib
import os
from contextlib import contextmanager
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


def _observe(path: Path) -> ExecutableWitness:
    fd = os.open(path, os.O_RDONLY | os.O_NOFOLLOW)
    try:
        stat = os.fstat(fd)
        if not S_ISREG(stat.st_mode):
            raise RuntimeError(f"selected executable is not a regular file: {path.name}")
        digest = hashlib.sha256()
        while chunk := os.read(fd, 65536):
            digest.update(chunk)
    finally:
        os.close(fd)
    return ExecutableWitness(path, stat.st_dev, stat.st_ino, stat.st_size, digest.hexdigest())


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

    @contextmanager
    def launch(self, name: str) -> Iterator[str]:
        self.validate(name)
        try:
            yield str(self._witnesses[name].path)
        finally:
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
    def launch_argv(self, argv: list[str]) -> Iterator[list[str]]:
        names = [token for token in argv if token in self._witnesses]
        for name in names:
            self.validate(name)
        bound = [
            str(self._witnesses[token].path) if token in self._witnesses else token
            for token in argv
        ]
        try:
            yield bound
        finally:
            for name in reversed(names):
                self.validate(name)
