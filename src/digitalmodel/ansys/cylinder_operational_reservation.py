"""Cooperative PID-file reservation compatible with the existing local queue.

This does not acquire a vendor licence or reserve nonparticipating routes.
The trusted caller determines settlement from owned-process evidence. There is
no context-manager release, stale takeover, retry or destructor cleanup.
"""
import hashlib
import os
from pathlib import Path
import stat


class ReservationError(RuntimeError):
    """Reservation ownership or lifecycle cannot be established."""


def _identity(info):
    return info.st_dev, info.st_ino


def _path(value):
    path = Path(value).absolute()
    for item in (path, *path.parents):
        if item.is_symlink() or item.is_junction():
            raise ReservationError('reservation owner path is redirected')
    return path


class LocalReservation:
    """Handle for one exclusive creation; file identity is never reacquired."""

    def __init__(self, path, pid, identity, raw):
        self._path = path
        self._pid = pid
        self._identity = identity
        self._raw = raw
        self._released = False

    def evidence(self):
        if self._released:
            raise ReservationError('reservation already released')
        try:
            path = _path(self._path)
            with path.open('rb') as stream:
                info = os.fstat(stream.fileno())
                raw = stream.read(len(self._raw) + 1)
            current = path.stat()
        except OSError as error:
            raise ReservationError('reservation owner file missing or unreadable') from error
        if (not stat.S_ISREG(info.st_mode)
                or _identity(info) != self._identity
                or _identity(current) != self._identity or raw != self._raw):
            raise ReservationError('reservation owner identity or bytes changed')
        return {'pid': self._pid, 'path': str(path.resolve()),
                'device': info.st_dev, 'inode': info.st_ino,
                'content_sha256': hashlib.sha256(raw).hexdigest()}

    def release(self, *, no_owned_processes):
        """Release only after explicit settlement and current owner verification.

        Participating routes cannot replace a held lock. This identity check is
        not an atomic conditional unlink against a hostile external writer.
        """
        if type(no_owned_processes) is not bool:
            raise TypeError('settlement requires an explicit boolean')
        self.evidence()
        if not no_owned_processes:
            return False
        try:
            self._path.unlink()
        except OSError as error:
            raise ReservationError('reservation owner release failed; retain evidence') from error
        self._released = True
        return True


def acquire_local_reservation(lock_path):
    """Create the existing PID-format lock once; retain partial failures.

    The parent directory must already exist under the operator's bound queue.
    Failure after exclusive creation preserves the file for owner disposition.
    """
    path = _path(lock_path)
    pid = os.getpid()
    raw = str(pid).encode('ascii')
    try:
        fd = os.open(path, os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
    except FileExistsError as error:
        raise ReservationError('licensed seat lock busy') from error
    except OSError as error:
        raise ReservationError('reservation owner file creation failed') from error
    try:
        with os.fdopen(fd, 'wb') as stream:
            stream.write(raw)
            stream.flush()
            os.fsync(stream.fileno())
            identity = _identity(os.fstat(stream.fileno()))
        owned = LocalReservation(path, pid, identity, raw)
        owned.evidence()
        return owned
    except OSError as error:
        raise ReservationError('partial reservation retained after creation failure') from error
