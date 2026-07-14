"""Atomic stale-lock retirement without an unguarded canonical-name gap."""

import json
import os

from digitalmodel.workflows.openfoam_batch_descriptor_io import (
    read_json_at,
    rename_exchange,
    same_inode,
)


def retire_observed_lock(
    locks_fd: int, name: str, tombstone: str, observed, record: dict, before_move
) -> None:
    try:
        sentinel_fd = os.open(
            tombstone,
            os.O_WRONLY | os.O_CREAT | os.O_EXCL | os.O_NOFOLLOW,
            0o600,
            dir_fd=locks_fd,
        )
    except FileExistsError as error:
        raise RuntimeError("lock reclaim tombstone collision") from error
    os.close(sentinel_fd)
    exchanged = False
    try:
        before_move(locks_fd, name)
        rename_exchange(name, tombstone, locks_fd, locks_fd)
        exchanged = True
        moved_record, moved_stat = read_json_at(locks_fd, tombstone)
        if not same_inode(observed, moved_stat) or moved_record != record:
            rename_exchange(name, tombstone, locks_fd, locks_fd)
            exchanged = False
            raise RuntimeError("lock reclaim source changed")
        os.unlink(tombstone, dir_fd=locks_fd)
        os.unlink(name, dir_fd=locks_fd)
    except (OSError, ValueError, json.JSONDecodeError):
        if exchanged:
            rename_exchange(name, tombstone, locks_fd, locks_fd)
            exchanged = False
        raise RuntimeError("lock reclaim source changed") from None
    finally:
        if not exchanged:
            try:
                os.unlink(tombstone, dir_fd=locks_fd)
            except FileNotFoundError:
                pass
