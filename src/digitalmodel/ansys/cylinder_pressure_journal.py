"""One-shot ordinal records and clocks; no native execution or restart recovery."""
import os
from pathlib import Path
import re
import stat

from digitalmodel.ansys.analysis_records import canonical_bytes, digest_bytes, parse_json


def owned_path(value):
    path = Path(value).absolute()
    for item in (path, *path.parents):
        if item.is_symlink() or item.is_junction():
            raise ValueError('pressure record path is redirected')
    if path != path.resolve():
        raise ValueError('pressure record path is not canonical')
    return path


def write_exclusive(path, value):
    path = owned_path(path)
    raw = canonical_bytes(value)
    with path.open('xb') as stream:
        stream.write(raw)
        stream.flush()
        os.fsync(stream.fileno())
    if path.read_bytes() != raw:
        raise OSError('pressure record readback differs')


class InvocationClock:
    """Use the smaller wall/monotonic allowance; rollback never extends it."""

    def __init__(self, clock):
        self.clock = clock
        self.start_mono, self.start_wall = self._sample()
        self.last_mono, self.last_wall = self.start_mono, self.start_wall

    def _sample(self):
        values = self.clock.monotonic_ns(), self.clock.time_ns()
        if any(type(v) is not int or v < 0 for v in values):
            raise ValueError('clock requires nonnegative integer nanoseconds')
        return values

    def remaining_seconds(self):
        mono, wall = self._sample()
        if mono < self.last_mono or wall < self.last_wall:
            raise ValueError('invocation clock moved backward')
        self.last_mono, self.last_wall = mono, wall
        elapsed = max(mono - self.start_mono, wall - self.start_wall)
        return (600_000_000_000 - elapsed) / 1_000_000_000

    def record(self):
        return dict(start_utc_unix_ns=self.start_wall,
                    deadline_utc_unix_ns=self.start_wall + 600_000_000_000,
                    start_monotonic_ns=self.start_mono,
                    clock_identity='process-local monotonic_ns; no restart recovery',
                    maximum_seconds=600)


class PressureJournal:
    """Fixed parent-stem names refuse partial, alternate and repeated records."""

    def __init__(self, parent, parent_sha256, *, writer=write_exclusive,
                 ordinal=2, predecessor=None):
        if type(ordinal) is not int or ordinal not in (2, 3):
            raise ValueError('only integer pressure ordinals 2 and 3 supported')
        if (ordinal == 2 and predecessor is not None
                or ordinal == 3 and (not isinstance(predecessor, dict)
                or set(predecessor) != {'path', 'sha256'})):
            raise ValueError('ordinal predecessor descriptor differs')
        self.ordinal = ordinal
        self.predecessor = dict(predecessor) if predecessor is not None else None
        self.parent = owned_path(parent)
        if not re.fullmatch(r'[0-9a-f]{64}', self.parent.stem):
            raise ValueError('fixed parent claim stem required')
        self.parent_sha256 = parent_sha256
        self.writer = writer
        stem = self.parent.stem + f'.ordinal-{ordinal}'
        self.paths = {key: self.parent.with_name(stem + suffix) for key, suffix in
                      [('invocation', '.invocation.json'), ('claim', '.json'),
                       ('terminal', '.terminal.json')]}
        self._parent()
        if any(self.parent.parent.glob(stem + '*')):
            raise FileExistsError(f'ordinal-{ordinal} record already present; no restart')
        self.started = False

    def _parent(self):
        path = owned_path(self.parent)
        info = path.stat()
        if (not stat.S_ISREG(info.st_mode) or info.st_nlink != 1
                or digest_bytes(path.read_bytes()) != self.parent_sha256):
            raise ValueError('original parent claim changed or aliased')
        self._predecessor()

    def _predecessor(self):
        if self.ordinal == 2:
            return
        expected = self.parent.with_name(self.parent.stem + '.ordinal-2.json')
        try:
            path = owned_path(self.predecessor['path'])
            info = path.stat()
            if path != expected or not stat.S_ISREG(info.st_mode) or info.st_nlink != 1:
                raise ValueError('coarse predecessor path or identity differs')
            raw = path.read_bytes()
            if digest_bytes(raw) != self.predecessor['sha256']:
                raise ValueError('coarse predecessor digest differs')
            row = parse_json(raw)
            if (type(row.get('ordinal')) is not int or row['ordinal'] != 2
                    or row.get('state') != 'attempt_consumed'
                    or row.get('case_id') != 'ocv-t60-p10-n4'
                    or row.get('parent_sha256') != self.parent_sha256):
                raise ValueError('consumed coarse predecessor binding differs')
        except (OSError, TypeError, KeyError) as error:
            raise ValueError('coarse predecessor unavailable or malformed') from error

    @property
    def consumed(self):
        # A partially created successor consumes the ordinal even after an error.
        return os.path.lexists(self.paths['claim'])

    def start(self, value):
        self._parent()
        self.writer(self.paths['invocation'], value)
        self.started = True

    def claim(self, value):
        if not self.started:
            raise ValueError('durable invocation required before claim')
        self._parent()
        self.writer(self.paths['claim'], value)

    def terminal(self, value):
        if not self.started:
            raise ValueError('durable invocation required before terminal record')
        if self.ordinal == 3:
            self._parent()
        self.writer(self.paths['terminal'], value)
