"""Small descriptor-relative atomic I/O primitives for owned batch layouts."""

import ctypes
import json
import os
import secrets
from stat import S_ISDIR, S_ISREG

from digitalmodel.workflows.openfoam_batch_config import canonical_json_bytes

_DIRECTORY_FLAGS = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
_RENAME_NOREPLACE = 1


def rename_noreplace(source: str, target: str, source_fd: int, target_fd: int) -> None:
    libc = ctypes.CDLL(None, use_errno=True)
    renameat2 = getattr(libc, "renameat2", None)
    if renameat2 is None:
        raise RuntimeError("atomic no-replace rename is unavailable")
    result = renameat2(source_fd, source.encode(), target_fd, target.encode(), _RENAME_NOREPLACE)
    if result:
        error = ctypes.get_errno()
        raise OSError(error, os.strerror(error), target)


def same_inode(left: os.stat_result, right: os.stat_result) -> bool:
    return (left.st_dev, left.st_ino) == (right.st_dev, right.st_ino)


def read_json_at(parent_fd: int, name: str) -> tuple[dict, os.stat_result]:
    fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW, dir_fd=parent_fd)
    try:
        stat = os.fstat(fd)
        with os.fdopen(os.dup(fd), "rb") as stream:
            value = json.load(stream)
    finally:
        os.close(fd)
    if not isinstance(value, dict):
        raise ValueError("record must be an object")
    return value, stat


def write_new_at(parent_fd: int, name: str, payload: dict) -> os.stat_result:
    fd = os.open(name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600, dir_fd=parent_fd)
    try:
        os.write(fd, canonical_json_bytes(payload))
        os.fsync(fd)
        return os.fstat(fd)
    finally:
        os.close(fd)


def remove_tree_fd(directory_fd: int) -> None:
    for name in os.listdir(directory_fd):
        stat = os.stat(name, dir_fd=directory_fd, follow_symlinks=False)
        if not S_ISDIR(stat.st_mode):
            os.unlink(name, dir_fd=directory_fd)
            continue
        child = os.open(name, _DIRECTORY_FLAGS, dir_fd=directory_fd)
        try:
            remove_tree_fd(child)
        finally:
            os.close(child)
        os.rmdir(name, dir_fd=directory_fd)


def write_case_file(work_fd: int, case: str, name: str, data: bytes) -> None:
    try:
        os.mkdir(case, 0o700, dir_fd=work_fd)
    except FileExistsError:
        pass
    case_fd = os.open(case, _DIRECTORY_FLAGS, dir_fd=work_fd)
    temporary = f".{name}.tmp-{secrets.token_hex(16)}"
    try:
        fd = os.open(
            temporary,
            os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
            0o600,
            dir_fd=case_fd,
        )
        try:
            os.write(fd, data)
            os.fsync(fd)
        finally:
            os.close(fd)
        os.rename(temporary, name, src_dir_fd=case_fd, dst_dir_fd=case_fd)
    finally:
        try:
            os.unlink(temporary, dir_fd=case_fd)
        except FileNotFoundError:
            pass
        os.close(case_fd)


def read_regular_file(parent_fd: int, name: str) -> tuple[bytes, os.stat_result]:
    fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW, dir_fd=parent_fd)
    try:
        file_stat = os.fstat(fd)
        if not S_ISREG(file_stat.st_mode):
            raise RuntimeError("external mutation target is not a regular file")
        chunks = []
        while chunk := os.read(fd, 65536):
            chunks.append(chunk)
        return b"".join(chunks), file_stat
    finally:
        os.close(fd)


def replace_regular_file(parent_fd: int, name: str, data: bytes) -> None:
    expected_data = None
    expected_stat = None
    try:
        expected_data, expected_stat = read_regular_file(parent_fd, name)
    except FileNotFoundError:
        pass
    temporary = f".{name}.tmp-{secrets.token_hex(16)}"
    fd = os.open(
        temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
        0o600, dir_fd=parent_fd,
    )
    try:
        os.write(fd, data)
        os.fsync(fd)
    finally:
        os.close(fd)
    try:
        if expected_stat is None:
            rename_noreplace(temporary, name, parent_fd, parent_fd)
            return
        current_data, current_stat = read_regular_file(parent_fd, name)
        if not same_inode(expected_stat, current_stat) or current_data != expected_data:
            raise RuntimeError("external mutation target was substituted")
        _replace_observed(parent_fd, name, temporary, expected_stat, expected_data)
    finally:
        try:
            os.unlink(temporary, dir_fd=parent_fd)
        except FileNotFoundError:
            pass


def _replace_observed(parent_fd, name, temporary, expected_stat, expected_data) -> None:
    tombstone = f".{name}.replace-{secrets.token_hex(16)}"
    rename_noreplace(name, tombstone, parent_fd, parent_fd)
    moved_data, moved_stat = read_regular_file(parent_fd, tombstone)
    if not same_inode(expected_stat, moved_stat) or moved_data != expected_data:
        rename_noreplace(tombstone, name, parent_fd, parent_fd)
        raise RuntimeError("external mutation target was substituted")
    try:
        rename_noreplace(temporary, name, parent_fd, parent_fd)
    except Exception:
        try:
            rename_noreplace(tombstone, name, parent_fd, parent_fd)
        except FileExistsError:
            pass
        raise
    os.unlink(tombstone, dir_fd=parent_fd)
