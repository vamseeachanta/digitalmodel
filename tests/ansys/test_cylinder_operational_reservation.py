"""RED contract: temporary cooperative locks, never the operational queue.

The production caller will supply process-settlement evidence. Synthetic bools
exercise the file lifecycle only; they do not establish licence ownership.
"""
import hashlib
import json
import os

import pytest

from digitalmodel.ansys.cylinder_operational_reservation import (
    ReservationError,
    acquire_local_reservation,
)


def test_acquisition_is_compatible_with_existing_pid_lock(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    assert path.read_bytes() == str(os.getpid()).encode('ascii')
    assert owned.release(no_owned_processes=True) is True
    assert not path.exists()


def test_busy_lock_is_not_adopted_or_rewritten(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    path.write_bytes(b'12345')
    before = path.stat()
    with pytest.raises(ReservationError, match='busy'):
        acquire_local_reservation(path)
    assert path.read_bytes() == b'12345'
    assert path.stat().st_ino == before.st_ino


def test_same_pid_existing_lock_is_still_busy(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    path.write_bytes(str(os.getpid()).encode('ascii'))
    with pytest.raises(ReservationError, match='busy'):
        acquire_local_reservation(path)
    assert path.exists()


def test_uncertain_settlement_retains_owner_then_settlement_releases(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    before = path.stat()
    assert owned.release(no_owned_processes=False) is False
    assert path.stat().st_ino == before.st_ino
    assert path.read_bytes() == str(os.getpid()).encode('ascii')
    assert owned.release(no_owned_processes=True) is True
    assert not path.exists()


def test_prelaunch_exception_can_release_when_no_owned_process_exists(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    try:
        raise RuntimeError('prelaunch input refusal')
    except RuntimeError:
        assert owned.release(no_owned_processes=True) is True
    assert not path.exists()


def test_exception_after_launch_does_not_implicitly_release(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    try:
        raise RuntimeError('simulated uncertain child settlement')
    except RuntimeError:
        assert owned.release(no_owned_processes=False) is False
    with pytest.raises(ReservationError, match='busy'):
        acquire_local_reservation(path)
    assert path.exists()


def test_replaced_owner_bytes_are_never_deleted(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    path.write_bytes(b'other-owner')
    with pytest.raises(ReservationError, match='owner|identity|changed'):
        owned.release(no_owned_processes=True)
    assert path.read_bytes() == b'other-owner'


def test_replaced_file_with_identical_pid_is_not_owned(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    other = tmp_path / 'replacement'
    other.write_bytes(path.read_bytes())
    assert other.stat().st_ino != path.stat().st_ino
    os.replace(other, path)
    with pytest.raises(ReservationError, match='owner|identity|changed'):
        owned.release(no_owned_processes=True)
    assert path.exists()


def test_missing_owner_file_is_not_reported_as_successful_release(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    path.unlink()
    with pytest.raises(ReservationError, match='owner|identity|missing'):
        owned.release(no_owned_processes=True)


@pytest.mark.parametrize('unproved', [None, 1, 0, 'true', [], {}])
def test_settlement_flag_requires_explicit_boolean(tmp_path, unproved):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    with pytest.raises((ReservationError, TypeError, ValueError)):
        owned.release(no_owned_processes=unproved)
    assert path.exists()


def test_old_handle_cannot_delete_successor_reservation(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    first = acquire_local_reservation(path)
    assert first.release(no_owned_processes=True) is True
    second = acquire_local_reservation(path)
    with pytest.raises(ReservationError, match='released|owner|identity'):
        first.release(no_owned_processes=True)
    assert path.exists()
    assert second.release(no_owned_processes=True) is True


def test_evidence_binds_current_pid_file_identity_and_raw_bytes(tmp_path):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    current = path.stat()
    evidence = owned.evidence()
    assert json.loads(json.dumps(evidence)) == evidence
    assert evidence['pid'] == os.getpid()
    assert evidence['path'] == str(path.resolve())
    assert evidence['device'] == current.st_dev
    assert evidence['inode'] == current.st_ino
    assert evidence['content_sha256'] == hashlib.sha256(path.read_bytes()).hexdigest()
    assert owned.release(no_owned_processes=True) is True


@pytest.mark.parametrize('change', ['modified', 'replaced', 'missing'])
def test_evidence_refuses_lost_owner_identity(tmp_path, change):
    path = tmp_path / '.licensed-run-seat.lock'
    owned = acquire_local_reservation(path)
    if change == 'modified':
        path.write_bytes(b'other-owner')
    elif change == 'replaced':
        replacement = tmp_path / 'replacement'
        replacement.write_bytes(path.read_bytes())
        assert replacement.stat().st_ino != path.stat().st_ino
        os.replace(replacement, path)
    else:
        path.unlink()
    with pytest.raises(ReservationError, match='owner|identity|changed|missing'):
        owned.evidence()
