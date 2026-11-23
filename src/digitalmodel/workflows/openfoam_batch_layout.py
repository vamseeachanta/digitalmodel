"""Owned external work layout and conservative destructive operations."""

from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass
import json
import os
from pathlib import Path
import secrets
import subprocess
import threading
import time
from typing import Iterator


MARKER = ".digitalmodel-run-owner.json"


@dataclass(frozen=True)
class WorkLayout:
    operator_root: Path
    namespace: str
    identity_sha256: str
    run_dir: Path
    marker_path: Path
    owner_token: str
    root_device: int
    root_inode: int
    run_identity: dict

    @classmethod
    def create(cls, operator_root: Path, namespace: str, identity_sha256: str,
               run_identity: dict | None = None) -> "WorkLayout":
        root = Path(operator_root).resolve()
        if not root.is_dir() or root.is_symlink():
            raise ValueError("operator root must be a real precreated directory")
        namespace_dir = root
        for component in namespace.split("/"):
            namespace_dir = namespace_dir / component
            try:
                namespace_dir.mkdir()
            except FileExistsError:
                if namespace_dir.is_symlink() or not namespace_dir.is_dir():
                    raise ValueError("external namespace contains symlink or non-directory")
        run_dir = namespace_dir / f"openfoam-run-{identity_sha256}"
        created = False
        try:
            run_dir.mkdir()
            created = True
        except FileExistsError:
            if run_dir.is_symlink() or not run_dir.is_dir():
                raise ValueError("external run directory is a symlink or non-directory")
        marker = run_dir / MARKER
        stat = root.stat()
        if created:
            token = secrets.token_hex(16)
            payload = _marker(identity_sha256, token, stat)
            _atomic_json(marker, payload)
            (run_dir / ".locks").mkdir()
        payload = _read_marker(marker)
        _validate_marker(payload, identity_sha256, stat)
        return cls(root, namespace, identity_sha256, run_dir, marker,
                   payload["owner_token"], stat.st_dev, stat.st_ino,
                   run_identity or {"identity_sha256": identity_sha256})

    def case_dir(self, case: str) -> Path:
        if not case or case in {".", ".."} or "/" in case or "\\" in case:
            raise ValueError("case name is not a strict descendant")
        return self.run_dir / case

    def assert_owned(self) -> None:
        stat = self.operator_root.stat()
        if stat.st_dev != self.root_device or stat.st_ino != self.root_inode:
            raise ValueError("operator root identity changed")
        if self.run_dir.is_symlink() or not self.run_dir.is_dir():
            raise ValueError("external run directory changed")
        payload = _read_marker(self.marker_path)
        _validate_marker(payload, self.identity_sha256, stat, self.owner_token)

    def clean_case(self, case: str) -> None:
        self.assert_owned()
        target = self.case_dir(case)
        if not target.exists():
            return
        if target.is_symlink() or not target.is_dir():
            raise ValueError("refusing to clean a non-directory or symlink case")
        before = target.stat()
        tombstone = self.run_dir / f".delete-{case}-{secrets.token_hex(8)}"
        os.replace(target, tombstone)
        after = tombstone.stat(follow_symlinks=False)
        if (tombstone.is_symlink() or not tombstone.is_dir()
                or (after.st_dev, after.st_ino) != (before.st_dev, before.st_ino)):
            if not target.exists():
                os.replace(tombstone, target)
            raise ValueError("case changed during clean")
        _dispose_tombstone(self.run_dir, tombstone, before)

    def prune_processors(self, case: str) -> None:
        self.assert_owned()
        case_dir = self.case_dir(case)
        for child in case_dir.glob("processor*") if case_dir.is_dir() else ():
            if child.is_symlink() or not child.is_dir():
                raise ValueError("refusing to prune substituted processor path")
            before = child.stat()
            tombstone = self.run_dir / f".delete-{case}-{child.name}-{secrets.token_hex(8)}"
            os.replace(child, tombstone)
            after = tombstone.stat(follow_symlinks=False)
            if (after.st_dev, after.st_ino) != (before.st_dev, before.st_ino):
                raise ValueError("processor directory changed during prune")
            _dispose_tombstone(self.run_dir, tombstone, before)

    @contextmanager
    def lock(self, name: str, stale_seconds: int = 3600) -> Iterator[None]:
        lock_dir = self.run_dir / ".locks" / name
        self.assert_owned()
        _acquire_lock(lock_dir, self.owner_token, stale_seconds)
        stop = threading.Event()
        heartbeat = threading.Thread(
            target=_heartbeat_lock,
            args=(lock_dir, self.owner_token, stop, max(1.0, min(30.0, stale_seconds / 3))),
            daemon=True,
        )
        heartbeat.start()
        try:
            yield
        finally:
            stop.set()
            heartbeat.join(timeout=5)
            if lock_dir.exists():
                _quarantine_and_dispose(lock_dir, "release")


def _marker(identity: str, token: str, stat: os.stat_result) -> dict:
    return {"schema": 1, "uid": getattr(os, "getuid", lambda: None)(),
            "identity_sha256": identity, "root_device": stat.st_dev,
            "root_inode": stat.st_ino, "owner_token": token}


def _validate_marker(payload: dict, identity: str, stat: os.stat_result,
                     token: str | None = None) -> None:
    expected = {"schema": 1, "identity_sha256": identity,
                "root_device": stat.st_dev, "root_inode": stat.st_ino}
    if any(payload.get(key) != value for key, value in expected.items()):
        raise ValueError("external run owner marker mismatch")
    if token is not None and payload.get("owner_token") != token:
        raise ValueError("external run owner marker token mismatch")
    if not isinstance(payload.get("owner_token"), str) or not payload["owner_token"]:
        raise ValueError("external run owner marker malformed")


def _read_marker(path: Path) -> dict:
    try:
        payload = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as exc:
        raise ValueError("external run owner marker missing or malformed") from exc
    if not isinstance(payload, dict):
        raise ValueError("external run owner marker malformed")
    return payload


def _atomic_json(path: Path, payload: dict) -> None:
    tmp = path.with_name(f".{path.name}.{secrets.token_hex(8)}.tmp")
    tmp.write_text(json.dumps(payload, sort_keys=True) + "\n")
    os.replace(tmp, path)


def _acquire_lock(path: Path, owner_token: str, stale_seconds: int) -> None:
    try:
        path.mkdir()
    except FileExistsError:
        meta_path = path / "meta.json"
        try:
            meta = json.loads(meta_path.read_text())
        except (OSError, json.JSONDecodeError):
            raise RuntimeError(f"lock {path.name!r} has unknown liveness")
        age = time.time() - float(meta.get("heartbeat_epoch", 0))
        same_boot = meta.get("boot_id") == _boot_id()
        live = same_boot and _process_matches(meta.get("pid"), meta.get("process_start"))
        if meta.get("owner_token") != owner_token or age <= stale_seconds or live:
            raise RuntimeError(f"lock {path.name!r} is held")
        tombstone = path.with_name(f".stale-{path.name}-{secrets.token_hex(8)}")
        before = path.stat(follow_symlinks=False)
        os.replace(path, tombstone)
        after = tombstone.stat(follow_symlinks=False)
        if not _same_inode(before, after):
            raise RuntimeError(f"lock {path.name!r} changed during stale reclaim")
        _dispose_tombstone(path.parent, tombstone, before)
        path.mkdir()
    _atomic_json(path / "meta.json", {"schema": 1, "pid": os.getpid(),
                 "process_start": _process_start(os.getpid()),
                 "boot_id": _boot_id(), "owner_token": owner_token,
                 "heartbeat_epoch": time.time()})


_CACHED_BOOT_ID: str | None = None


def _boot_id() -> str:
    global _CACHED_BOOT_ID
    if _CACHED_BOOT_ID is not None:
        return _CACHED_BOOT_ID
    system_id = Path("/proc/sys/kernel/random/boot_id")
    try:
        value = system_id.read_text().strip()
    except OSError:
        command = ["powershell", "-NoProfile", "-NonInteractive", "-Command",
                   "(Get-CimInstance Win32_OperatingSystem).LastBootUpTime.ToFileTimeUtc()"]
        result = subprocess.run(command, capture_output=True, text=True, timeout=15)
        if result.returncode or not result.stdout.strip().isdigit():
            raise RuntimeError("cannot establish stable operating-system boot identity")
        value = f"windows-filetime-{result.stdout.strip()}"
    _CACHED_BOOT_ID = value
    return value


def _process_matches(value: object, expected_start: object) -> bool:
    try:
        pid = int(value)
        if pid <= 0 or not isinstance(expected_start, str) or not expected_start:
            return False
        return _process_start(pid) == expected_start
    except (TypeError, ValueError, OSError):
        return False


def _process_start(pid: int) -> str:
    if os.name != "nt":
        fields = Path(f"/proc/{pid}/stat").read_text().split()
        return f"proc-start-{fields[21]}"
    import ctypes
    kernel = ctypes.windll.kernel32
    handle = kernel.OpenProcess(0x1000, False, pid)
    if not handle:
        raise OSError(f"process {pid} is unavailable")
    created, exited, kernel_time, user_time = (ctypes.c_ulonglong() for _ in range(4))
    try:
        ok = kernel.GetProcessTimes(handle, *(ctypes.byref(value) for value in
                                             (created, exited, kernel_time, user_time)))
        if not ok:
            raise OSError(f"process {pid} start time is unavailable")
        return f"windows-filetime-{created.value}"
    finally:
        kernel.CloseHandle(handle)


def _heartbeat_lock(path: Path, owner_token: str, stop: threading.Event,
                    interval: float) -> None:
    while not stop.wait(interval):
        meta_path = path / "meta.json"
        try:
            meta = json.loads(meta_path.read_text())
            if (meta.get("owner_token") != owner_token
                    or not _process_matches(meta.get("pid"), meta.get("process_start"))):
                return
            meta["heartbeat_epoch"] = time.time()
            _atomic_json(meta_path, meta)
        except (OSError, ValueError, json.JSONDecodeError):
            return


def _dispose_tombstone(run_dir: Path, tombstone: Path,
                       expected: os.stat_result) -> None:
    actual = tombstone.stat(follow_symlinks=False)
    if not _same_inode(actual, expected):
        raise ValueError("tombstone changed before quarantine retention")
    # Python exposes no portable primitive that atomically binds rmdir/unlink
    # to an already-verified directory handle. Retain the random quarantine;
    # any later name-based removal would reintroduce a replacement race.


def _quarantine_and_dispose(path: Path, label: str) -> None:
    if path.is_symlink() or not path.is_dir():
        raise ValueError(f"refusing to dispose substituted {label} directory")
    before = path.stat(follow_symlinks=False)
    tombstone = path.with_name(f".{label}-{path.name}-{secrets.token_hex(8)}")
    os.replace(path, tombstone)
    after = tombstone.stat(follow_symlinks=False)
    if not _same_inode(before, after):
        raise ValueError(f"{label} directory changed during quarantine")
    _dispose_tombstone(path.parent, tombstone, before)


def _same_inode(left: object, right: object) -> bool:
    return (getattr(left, "st_dev", None), getattr(left, "st_ino", None)) == (
        getattr(right, "st_dev", None), getattr(right, "st_ino", None))
