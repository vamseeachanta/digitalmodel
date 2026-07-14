"""Owned external and legacy filesystem helpers for OpenFOAM batch cases."""

from __future__ import annotations

import contextlib
import ctypes
import errno
import hashlib
import json
import os
import re
import secrets
import time
from pathlib import Path
from stat import S_ISDIR
from typing import Iterator

from digitalmodel.workflows.openfoam_batch_config import (
    ExecutionAuthority,
    canonical_json_bytes,
)
from digitalmodel.workflows.openfoam_batch_descriptor_io import (
    write_case_file as _write_case_file,
)
from digitalmodel.workflows.openfoam_batch_legacy_layout import (  # noqa: F401
    DECOMPOSE_PAR_DICT,
    clean_case_dir,
    has_processor_dirs,
    prune_processor_dirs,
    set_start_from_latest_time,
    write_decompose_par_dict,
)

OWNER_FILENAME = ".digitalmodel-run-owner.json"
LOCK_SCHEMA = 1
OWNER_SCHEMA = 1
_DIRECTORY_FLAGS = os.O_RDONLY | os.O_DIRECTORY | os.O_NOFOLLOW
_CASE_PATTERN = re.compile(r"[A-Za-z0-9][A-Za-z0-9_.-]{0,127}\Z")
_RENAME_NOREPLACE = 1
def _rename_noreplace(source: str, target: str, source_fd: int, target_fd: int) -> None:
    libc = ctypes.CDLL(None, use_errno=True)
    renameat2 = getattr(libc, "renameat2", None)
    if renameat2 is None:
        raise RuntimeError("atomic no-replace rename is unavailable")
    result = renameat2(source_fd, source.encode(), target_fd, target.encode(), _RENAME_NOREPLACE)
    if result:
        error = ctypes.get_errno()
        raise OSError(error, os.strerror(error), target)


def _same_inode(left: os.stat_result, right: os.stat_result) -> bool:
    return (left.st_dev, left.st_ino) == (right.st_dev, right.st_ino)

def _read_json_at(parent_fd: int, name: str) -> tuple[dict, os.stat_result]:
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


def _write_new_at(parent_fd: int, name: str, payload: dict) -> os.stat_result:
    fd = os.open(name, os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW, 0o600, dir_fd=parent_fd)
    try:
        data = canonical_json_bytes(payload)
        os.write(fd, data)
        os.fsync(fd)
        return os.fstat(fd)
    finally:
        os.close(fd)


def _open_namespace(root_fd: int, namespace: Path) -> int:
    current = os.dup(root_fd)
    try:
        for component in namespace.parts:
            try:
                os.mkdir(component, 0o700, dir_fd=current)
            except FileExistsError:
                pass
            try:
                child = os.open(component, _DIRECTORY_FLAGS, dir_fd=current)
            except OSError as error:
                raise RuntimeError("external namespace collision") from error
            os.close(current)
            current = child
        return current
    except Exception:
        os.close(current)
        raise


def _owner_payload(identity: dict, root_stat: os.stat_result, token: str) -> dict:
    return {
        "schema_version": OWNER_SCHEMA,
        "uid": os.getuid(),
        "identity": identity,
        "operator_root_device": root_stat.st_dev,
        "operator_root_inode": root_stat.st_ino,
        "owner_token": token,
    }


def _remove_tree_fd(directory_fd: int) -> None:
    for name in os.listdir(directory_fd):
        stat = os.stat(name, dir_fd=directory_fd, follow_symlinks=False)
        if not S_ISDIR(stat.st_mode):
            os.unlink(name, dir_fd=directory_fd)
            continue
        child = os.open(name, _DIRECTORY_FLAGS, dir_fd=directory_fd)
        try:
            _remove_tree_fd(child)
        finally:
            os.close(child)
        os.rmdir(name, dir_fd=directory_fd)


def _lock_tombstone_name(name: str) -> str:
    return f"{name}.reclaim-{secrets.token_hex(16)}"
def _boot_id() -> str:
    try:
        return Path("/proc/sys/kernel/random/boot_id").read_text().strip()
    except OSError:
        return "unknown"


def _process_start_token(pid: int | None = None) -> str:
    process = os.getpid() if pid is None else pid
    return Path(f"/proc/{process}/stat").read_text().split()[21]

def _process_state(record: dict) -> str:
    try:
        actual = _process_start_token(int(record["pid"]))
    except FileNotFoundError:
        return "dead"
    except (OSError, ValueError, KeyError, IndexError):
        return "unknown"
    return "alive-match" if actual == record.get("process_start_token") else "alive-mismatch"

def lock_reclaimable(
    record: dict,
    *,
    owner_token: str,
    now: float,
    current_boot_id: str,
    process_state: str,
    stale_after: float,
) -> bool:
    """Require owner, expiry, and proof of prior boot or dead/reused PID."""
    if record.get("owner_token") != owner_token:
        return False
    heartbeat = record.get("heartbeat")
    if not isinstance(heartbeat, (int, float)) or now - heartbeat <= stale_after:
        return False
    if record.get("boot_id") != current_boot_id:
        return True
    return process_state in {"dead", "alive-mismatch"}


class WorkLayout:
    """Descriptor-retaining owned run layout for external execution."""

    def __init__(self, authority, identity, work_name, descriptors, marker, marker_stat):
        self.authority = authority
        self.identity = identity
        self.work_name = work_name
        self.root_fd, self.namespace_fd, self.run_fd, self.work_fd = descriptors
        self.root_path = authority.root
        self.namespace_path = authority.root / authority.namespace
        self.run_path = authority.root / authority.namespace / f"openfoam-run-{identity['identity_sha256']}"
        self.work_path = self.run_path / work_name
        self.owner_token = marker["owner_token"]
        self._root_stat = os.fstat(self.root_fd)
        self._namespace_stat = os.fstat(self.namespace_fd)
        self._run_stat = os.fstat(self.run_fd)
        self._marker_stat = marker_stat
        self._held: set[str] = set()

    @classmethod
    def create(cls, authority: ExecutionAuthority, identity: dict, work_name: str):
        if authority.context == "legacy" or authority.root is None or authority.namespace is None:
            raise ValueError("external execution requires operator authority")
        if not _CASE_PATTERN.fullmatch(work_name) or work_name == ".locks":
            raise ValueError("configured work directory must be one portable component")
        root_fd = os.open(authority.root, _DIRECTORY_FLAGS)
        namespace_fd = _open_namespace(root_fd, authority.namespace)
        try:
            cls._create_run(namespace_fd, os.fstat(root_fd), identity, work_name)
            return cls._open(authority, identity, work_name, root_fd, namespace_fd)
        except Exception:
            os.close(namespace_fd)
            os.close(root_fd)
            raise

    @staticmethod
    def _create_run(namespace_fd, root_stat, identity, work_name):
        final = f"openfoam-run-{identity['identity_sha256']}"
        try:
            existing_fd = os.open(final, _DIRECTORY_FLAGS, dir_fd=namespace_fd)
        except FileNotFoundError:
            pass
        else:
            os.close(existing_fd)
            return
        staging = f".{final}.creating-{secrets.token_hex(16)}"
        os.mkdir(staging, 0o700, dir_fd=namespace_fd)
        stage_fd = os.open(staging, _DIRECTORY_FLAGS, dir_fd=namespace_fd)
        try:
            token = secrets.token_hex(32)
            _write_new_at(stage_fd, OWNER_FILENAME, _owner_payload(identity, root_stat, token))
            os.mkdir(".locks", 0o700, dir_fd=stage_fd)
            os.mkdir(work_name, 0o700, dir_fd=stage_fd)
            os.fsync(stage_fd)
            _rename_noreplace(staging, final, namespace_fd, namespace_fd)
        except OSError as error:
            if error.errno != errno.EEXIST:
                raise
        finally:
            os.close(stage_fd)
            try:
                import shutil

                shutil.rmtree(staging, dir_fd=namespace_fd)
            except FileNotFoundError:
                pass

    @classmethod
    def _open(cls, authority, identity, work_name, root_fd, namespace_fd):
        name = f"openfoam-run-{identity['identity_sha256']}"
        try:
            run_fd = os.open(name, _DIRECTORY_FLAGS, dir_fd=namespace_fd)
            marker, marker_stat = _read_json_at(run_fd, OWNER_FILENAME)
            expected = _owner_payload(identity, os.fstat(root_fd), marker.get("owner_token", ""))
            if marker != expected or len(marker.get("owner_token", "")) < 32:
                raise ValueError("marker mismatch")
            work_fd = os.open(work_name, _DIRECTORY_FLAGS, dir_fd=run_fd)
        except (OSError, ValueError, json.JSONDecodeError) as error:
            if "run_fd" in locals():
                os.close(run_fd)
            raise RuntimeError("preexisting path is not the expected owned run") from error
        return cls(authority, identity, work_name, (root_fd, namespace_fd, run_fd, work_fd), marker, marker_stat)

    def close(self) -> None:
        for name in ("work_fd", "run_fd", "namespace_fd", "root_fd"):
            fd = getattr(self, name, None)
            if fd is not None:
                os.close(fd)
                setattr(self, name, None)

    def __enter__(self):
        return self

    def __exit__(self, *_args):
        self.close()

    def validate_owner(self) -> None:
        try:
            root_stat = os.stat(self.root_path, follow_symlinks=False)
            namespace_stat = os.stat(self.namespace_path, follow_symlinks=False)
            run_stat = os.stat(self.run_path.name, dir_fd=self.namespace_fd, follow_symlinks=False)
            marker, marker_stat = _read_json_at(self.run_fd, OWNER_FILENAME)
        except (OSError, ValueError, json.JSONDecodeError) as error:
            raise RuntimeError("owned run validation failed") from error
        expected = _owner_payload(self.identity, self._root_stat, self.owner_token)
        stable = _same_inode(root_stat, self._root_stat)
        stable &= _same_inode(namespace_stat, self._namespace_stat)
        stable &= _same_inode(run_stat, self._run_stat)
        stable &= _same_inode(marker_stat, self._marker_stat)
        if not stable or marker != expected:
            raise RuntimeError("owned run validation failed")

    def case_path(self, case: str) -> Path:
        if not isinstance(case, str) or not _CASE_PATTERN.fullmatch(case) or case in {".", "..", ".locks", self.work_name}:
            raise ValueError("case must be one strict portable descendant")
        return self.work_path / case

    def _remove_named(self, parent_fd: int, name: str, mutation_hook=None) -> None:
        try:
            opened = os.open(name, _DIRECTORY_FLAGS, dir_fd=parent_fd)
        except FileNotFoundError:
            return
        original = os.fstat(opened)
        tombstone = f".{name}.delete-{secrets.token_hex(16)}"
        try:
            if mutation_hook:
                mutation_hook(parent_fd, name)
            _rename_noreplace(name, tombstone, parent_fd, parent_fd)
            moved = os.stat(tombstone, dir_fd=parent_fd, follow_symlinks=False)
            if not _same_inode(original, moved):
                _rename_noreplace(tombstone, name, parent_fd, parent_fd)
                raise RuntimeError("destructive target was substituted")
            _remove_tree_fd(opened)
            os.rmdir(tombstone, dir_fd=parent_fd)
        finally:
            os.close(opened)

    def clean_case(self, case: str, mutation_hook=None) -> None:
        self.validate_owner()
        self.case_path(case)
        self._remove_named(self.work_fd, case, mutation_hook)

    def prune_processors(self, case: str, mutation_hook=None) -> None:
        self.validate_owner()
        self.case_path(case)
        try:
            case_fd = os.open(case, _DIRECTORY_FLAGS, dir_fd=self.work_fd)
        except FileNotFoundError:
            return
        try:
            for name in os.listdir(case_fd):
                if re.fullmatch(r"processor[0-9]+", name):
                    self._remove_named(case_fd, name, mutation_hook)
        finally:
            os.close(case_fd)

    def _lock_record(self, now: float) -> dict:
        return {
            "schema_version": LOCK_SCHEMA,
            "owner_token": self.owner_token,
            "boot_id": _boot_id(),
            "pid": os.getpid(),
            "process_start_token": _process_start_token(),
            "heartbeat": now,
        }

    def _lock_name(self, key: str) -> str:
        return "run.lock" if key == "run" else f"case-{hashlib.sha256(key.encode()).hexdigest()}.lock"

    def _try_reclaim(self, locks_fd, name, now, stale_after) -> None:
        try:
            record, observed = _read_json_at(locks_fd, name)
        except (OSError, ValueError, json.JSONDecodeError):
            return
        state = _process_state(record)
        if not lock_reclaimable(record, owner_token=self.owner_token, now=now, current_boot_id=_boot_id(), process_state=state, stale_after=stale_after):
            return
        tombstone = _lock_tombstone_name(name)
        try:
            _rename_noreplace(name, tombstone, locks_fd, locks_fd)
        except FileExistsError as error:
            raise RuntimeError("lock reclaim tombstone collision") from error
        moved = os.stat(tombstone, dir_fd=locks_fd, follow_symlinks=False)
        if not _same_inode(observed, moved):
            _rename_noreplace(tombstone, name, locks_fd, locks_fd)
            raise RuntimeError("lock reclaim target was substituted")
        os.unlink(tombstone, dir_fd=locks_fd)

    @contextlib.contextmanager
    def lock(self, key: str = "run", *, poll_interval=0.05, stale_after=120.0, now=time.time) -> Iterator[None]:
        self.validate_owner()
        if key != "run":
            self.case_path(key)
        name = self._lock_name(key)
        locks_fd = os.open(".locks", _DIRECTORY_FLAGS, dir_fd=self.run_fd)
        acquired_stat = None
        try:
            while acquired_stat is None:
                moment = now()
                try:
                    acquired_stat = _write_new_at(locks_fd, name, self._lock_record(moment))
                except FileExistsError:
                    self._try_reclaim(locks_fd, name, moment, stale_after)
                    time.sleep(poll_interval)
            self._held.add(key)
            yield
        finally:
            self._held.discard(key)
            if acquired_stat is not None:
                current = os.stat(name, dir_fd=locks_fd, follow_symlinks=False)
                if _same_inode(current, acquired_stat):
                    os.unlink(name, dir_fd=locks_fd)
            os.close(locks_fd)

    def require_locks(self, case: str) -> None:
        if not {"run", case}.issubset(self._held):
            raise RuntimeError("external checkpoint requires run and case locks")
        self.validate_owner()

    def read_case_file(self, case: str, name: str, limit: int) -> bytes | None:
        self.case_path(case)
        try:
            case_fd = os.open(case, _DIRECTORY_FLAGS, dir_fd=self.work_fd)
            fd = os.open(name, os.O_RDONLY | os.O_NOFOLLOW, dir_fd=case_fd)
        except FileNotFoundError:
            return None
        try:
            data = os.read(fd, limit + 1)
            return None if len(data) > limit else data
        finally:
            os.close(fd)
            os.close(case_fd)

    def write_case_file(self, case: str, name: str, data: bytes) -> None:
        self.case_path(case)
        _write_case_file(self.work_fd, case, name, data)
