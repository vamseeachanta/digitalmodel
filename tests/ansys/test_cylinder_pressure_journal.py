"""Synthetic durable-record and time-boundary tests; no native process."""
import json
from pathlib import Path

import pytest

from digitalmodel.ansys.analysis_records import digest_bytes
from digitalmodel.ansys.cylinder_pressure_journal import InvocationClock, PressureJournal


class Clock:
    mono = 0
    wall = 1_000_000_000_000

    def monotonic_ns(self):
        return self.mono

    def time_ns(self):
        return self.wall

    def advance(self, seconds):
        self.mono += seconds * 1_000_000_000
        self.wall += seconds * 1_000_000_000


def test_clock_uses_fixed_window_and_refuses_rollback():
    clock = Clock()
    window = InvocationClock(clock)
    clock.advance(235)
    assert window.remaining_seconds() == 365
    clock.wall -= 1
    with pytest.raises(ValueError, match='backward'):
        window.remaining_seconds()


def test_forward_wall_step_cannot_extend_window():
    clock = Clock()
    window = InvocationClock(clock)
    clock.wall += 300_000_000_000
    assert window.remaining_seconds() == 300


@pytest.mark.parametrize('field', ['mono', 'wall'])
def test_clock_rejects_noninteger_values(field):
    clock = Clock()
    setattr(clock, field, True)
    with pytest.raises(ValueError):
        InvocationClock(clock)


def make(tmp_path):
    parent = tmp_path / ('a' * 64 + '.json')
    parent.write_bytes(b'{"parent":"synthetic"}')
    return parent, PressureJournal(parent, digest_bytes(parent.read_bytes()))


def test_exclusive_records_retain_original_and_refuse_restart(tmp_path):
    parent, journal = make(tmp_path)
    original = parent.read_bytes()
    journal.start({'synthetic': True})
    journal.claim({'ordinal': 2})
    journal.terminal({'reason': 'PLANNED_SCOPE_STOP'})
    assert parent.read_bytes() == original
    assert journal.consumed is True
    assert json.loads(journal.paths['claim'].read_bytes()) == {'ordinal': 2}
    with pytest.raises(FileExistsError):
        PressureJournal(parent, digest_bytes(original))


def test_partial_invocation_refuses_without_replacement(tmp_path):
    parent, journal = make(tmp_path)
    journal.paths['invocation'].write_bytes(b'{')
    with pytest.raises(FileExistsError):
        journal.start({})
    assert journal.paths['invocation'].read_bytes() == b'{'
    assert journal.consumed is False


def test_changed_parent_refuses_successor(tmp_path):
    parent, journal = make(tmp_path)
    journal.start({})
    parent.write_bytes(b'changed')
    with pytest.raises(ValueError, match='parent'):
        journal.claim({})
    assert not journal.paths['claim'].exists()


def test_partial_successor_consumes_even_when_writer_fails(tmp_path, monkeypatch):
    _, journal = make(tmp_path)
    journal.start({})
    original = Path.open

    def partial(path, *args, **kwargs):
        if path == journal.paths['claim'] and args == ('xb',):
            path.write_bytes(b'{')
            raise OSError('synthetic crash after exclusive create')
        return original(path, *args, **kwargs)

    monkeypatch.setattr(Path, 'open', partial)
    with pytest.raises(OSError):
        journal.claim({})
    assert journal.consumed is True


def test_alternate_claim_name_refuses(tmp_path):
    parent, _ = make(tmp_path)
    parent.with_name(parent.stem + '.ordinal-2.other.json').write_bytes(b'{}')
    with pytest.raises(FileExistsError):
        PressureJournal(parent, digest_bytes(parent.read_bytes()))


def test_concurrent_invocation_writers_have_one_winner(tmp_path):
    from concurrent.futures import ThreadPoolExecutor
    from threading import Barrier
    parent, first = make(tmp_path)
    second = PressureJournal(parent, digest_bytes(parent.read_bytes()))
    barrier = Barrier(2)
    def start(journal):
        barrier.wait(timeout=5)
        try:
            journal.start({'synthetic':'concurrent'})
            return True
        except FileExistsError:
            return False
    with ThreadPoolExecutor(max_workers=2) as pool:
        assert sorted(pool.map(start, [first, second])) == [False, True]
    assert not first.consumed and not second.consumed
