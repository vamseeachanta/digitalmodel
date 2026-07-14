"""Owned external work layout and conservative destructive operations."""

from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass
import json
import os
from pathlib import Path
import secrets
import shutil
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

    @classmethod
    def create(cls, operator_root: Path, namespace: str, identity_sha256: str) -> "WorkLayout":
        root = Path(operator_root).resolve()
        if not root.is_dir() or root.is_symlink():
            raise ValueError("operator root must be a real precreated directory")
        namespace_dir = root.joinpath(*namespace.split("/"))
        namespace_dir.mkdir(parents=True, exist_ok=True)
        run_dir = namespace_dir / f"openfoam-run-{identity_sha256}"
        created = False
        try:
            run_dir.mkdir()
            created = True
        except FileExistsError:
            pass
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
                   payload["owner_token"], stat.st_dev, stat.st_ino)

    def case_dir(self, case: str) -> Path:
        if not case or case in {".", ".."} or "/" in case or "\\" in case:
            raise ValueError("case name is not a strict descendant")
        return self.run_dir / case

    def assert_owned(self) -> None:
        stat = self.operator_root.stat()
        payload = _read_marker(self.marker_path)
        _validate_marker(payload, self.identity_sha256, stat, self.owner_token)

    def clean_case(self, case: str) -> None:
        self.assert_owned()
        target = self.case_dir(case)
        if not target.exists():
            return
        if target.is_symlink() or not target.is_dir():
            raise ValueError("refusing to clean a non-directory or symlink case")
        tombstone = self.run_dir / f".delete-{case}-{secrets.token_hex(8)}"
        os.replace(target, tombstone)
        if tombstone.is_symlink() or not tombstone.is_dir():
            if not target.exists():
                os.replace(tombstone, target)
            raise ValueError("case changed during clean")
        shutil.rmtree(tombstone)

    def prune_processors(self, case: str) -> None:
        self.assert_owned()
        case_dir = self.case_dir(case)
        for child in case_dir.glob("processor*") if case_dir.is_dir() else ():
            if child.is_symlink() or not child.is_dir():
                raise ValueError("refusing to prune substituted processor path")
            shutil.rmtree(child)

    @contextmanager
    def lock(self, name: str, stale_seconds: int = 3600) -> Iterator[None]:
        lock_dir = self.run_dir / ".locks" / name
        self.assert_owned()
        _acquire_lock(lock_dir, self.owner_token, stale_seconds)
        try:
            yield
        finally:
            shutil.rmtree(lock_dir, ignore_errors=True)


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
        if meta.get("owner_token") != owner_token or age <= stale_seconds:
            raise RuntimeError(f"lock {path.name!r} is held")
        tombstone = path.with_name(f".stale-{path.name}-{secrets.token_hex(8)}")
        os.replace(path, tombstone)
        shutil.rmtree(tombstone)
        path.mkdir()
    _atomic_json(path / "meta.json", {"schema": 1, "pid": os.getpid(),
                 "owner_token": owner_token, "heartbeat_epoch": time.time()})
