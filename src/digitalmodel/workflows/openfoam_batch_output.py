"""Retained descriptor layout for external result publication."""

from __future__ import annotations

import os
from pathlib import Path
from stat import S_ISDIR

from digitalmodel.workflows.openfoam_batch_descriptor_io import (
    replace_regular_file,
    same_inode,
)

_DIR_FLAGS = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW


class OutputLayout:
    """Bind an input parent and every configured output ancestor by inode."""

    def __init__(self, parent_path: Path, parent_fd: int, nodes: list[tuple]):
        self.parent_path = parent_path
        self.parent_fd = parent_fd
        self.parent_stat = os.fstat(parent_fd)
        self.nodes = nodes
        self.output_fd = nodes[-1][1]

    @classmethod
    def create(cls, parent_path: Path, parent_fd: int, relative: str):
        parent_stat = os.fstat(parent_fd)
        current = parent_fd
        nodes = []
        try:
            _validate_path(parent_path, parent_stat)
            for component in Path(relative).parts:
                try:
                    os.mkdir(component, 0o700, dir_fd=current)
                except FileExistsError:
                    pass
                child = os.open(component, _DIR_FLAGS, dir_fd=current)
                nodes.append((component, child, os.fstat(child)))
                current = child
            layout = cls(parent_path, parent_fd, nodes)
            layout.validate()
            return layout
        except Exception:
            for _, fd, _ in reversed(nodes):
                os.close(fd)
            raise

    @property
    def path(self) -> Path:
        return self.parent_path.joinpath(*(name for name, _, _ in self.nodes))

    def validate(self) -> None:
        _validate_path(self.parent_path, self.parent_stat)
        parent = self.parent_fd
        for name, fd, expected in self.nodes:
            current = os.stat(name, dir_fd=parent, follow_symlinks=False)
            if not S_ISDIR(current.st_mode) or not same_inode(current, expected):
                raise ValueError("external output_dir changed")
            if not same_inode(os.fstat(fd), expected):
                raise ValueError("external output_dir descriptor changed")
            parent = fd

    def write(self, name: str, data: bytes) -> None:
        self.validate()
        replace_regular_file(self.output_fd, name, data)
        self.validate()

    def close(self) -> None:
        for _, fd, _ in reversed(self.nodes):
            os.close(fd)
        self.nodes = []
        if self.parent_fd is not None:
            os.close(self.parent_fd)
            self.parent_fd = None


def _validate_path(path: Path, expected) -> None:
    try:
        current = path.stat(follow_symlinks=False)
    except OSError as error:
        raise ValueError("external output_dir parent changed") from error
    if not S_ISDIR(current.st_mode) or not same_inode(current, expected):
        raise ValueError("external output_dir parent changed")
