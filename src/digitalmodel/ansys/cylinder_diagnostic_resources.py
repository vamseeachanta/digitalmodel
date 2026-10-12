"""Operational checks for the one-case diagnostic; never engineering acceptance."""
from decimal import Decimal, InvalidOperation
import re

from digitalmodel.ansys.cylinder_results import decimal_context


ENVIRONMENT = {'ANSYS261_PRODUCT': 'ansys', 'ANS_CONSEC': 'YES'}
MINIMUM_MEMORY = 8 * 1024 ** 3
MAXIMUM_AGE = Decimal('30')


def _decimal(value):
    if (not isinstance(value, str) or len(value) > 40
            or not re.fullmatch(r'(?:0|[1-9][0-9]*)(?:\.[0-9]+)?', value)):
        raise ValueError('finite decimal text required')
    try:
        number = Decimal(value)
    except InvalidOperation as error:
        raise ValueError('invalid decimal text') from error
    if not number.is_finite():
        raise ValueError('finite decimal text required')
    return number


def validate_environment(approval, environment):
    """Verify inherited values without changing process or machine settings."""
    expected = approval.get('launch_environment')
    if expected != ENVIRONMENT or any(environment.get(k) != v for k, v in ENVIRONMENT.items()):
        raise ValueError('bound inherited launch environment differs')
    return dict(ENVIRONMENT)


def _sample(row):
    timestamp, interval = _decimal(row['observed_at']), _decimal(row['interval_seconds'])
    cpu = _decimal(row['cpu_percent'])
    cores, memory = row['logical_processors'], row['available_memory_bytes']
    if type(cores) is not int or not 1 <= cores <= 4096 or type(memory) is not int:
        raise ValueError('physical resource counts require integer observations')
    if not 1 <= interval <= 2 or not 0 <= cpu <= 100:
        raise ValueError('invalid one-second CPU sample')
    idle = Decimal(cores) * (1 - cpu / 100)
    if idle < 2 or memory < MINIMUM_MEMORY:
        raise ValueError('operational CPU or available-memory threshold not met')
    return timestamp, interval, idle, memory


@decimal_context
def validate_capacity(observation, *, now, requested_cores=1):
    """Require five fresh samples; observed idle capacity is not a reservation."""
    now = _decimal(now)
    rows = observation.get('samples')
    if type(requested_cores) is not int or requested_cores != 1:
        raise ValueError('only the fixed single-core profile is supported')
    if not isinstance(rows, list) or len(rows) != 5:
        raise ValueError('exactly five capacity samples required')
    samples = [_sample(row) for row in rows]
    for previous, current in zip(samples, samples[1:]):
        elapsed = current[0] - previous[0]
        if not current[1] <= elapsed <= Decimal('2.1'):
            raise ValueError('capacity samples are not a consecutive window')
    age = now - samples[-1][0]
    if age < 0 or age > MAXIMUM_AGE:
        raise ValueError('capacity observation is future or stale')
    return {'status': 'PASS', 'required_idle_core_equivalents': '2',
            'minimum_idle_core_equivalents': format(min(s[2] for s in samples).normalize(), 'f'),
            'minimum_available_memory_bytes': min(s[3] for s in samples),
            'maximum_age_seconds': '30', 'observed_age_seconds': str(age)}


def _increments(text, feature):
    starts = list(re.finditer(r'^Feature "([^"\r\n]+)" ', text, re.MULTILINE))
    if not starts:
        raise ValueError('licence feature records absent')
    records = []
    for index, start in enumerate(starts):
        if start[1] != feature:
            continue
        end = starts[index + 1].start() if index + 1 < len(starts) else len(text)
        block = text[start.start():end]
        version = re.match(r'Feature "[^"]+" v(\d{4}\.\d{4}),', block)
        issued = re.search(r'Total of (\d+) licenses? issued;', block)
        used = re.search(r'Total of (\d+) floating non-reserved licenses in use', block)
        queue = re.search(r'Total of (\d+) users queued;', block)
        reserved = re.search(r'Total of (\d+) licenses reserved', block)
        if not all((version, issued, used, queue, reserved)):
            raise ValueError('incomplete licence increment layout')
        if 'expiry: permanent(no expiration date)' not in block.splitlines()[0]:
            raise ValueError('unsupported licence-expiry evidence')
        records.append((version[1], *[int(item[1]) for item in (issued, used, queue, reserved)]))
    return records


def parse_compatible_license(raw, *, feature, minimum_version):
    """Parse observed increment availability; this never performs a checkout."""
    if feature != 'ansys' or not isinstance(raw, bytes):
        raise ValueError('original ansys feature-query bytes required')
    minimum = _decimal(minimum_version)
    try:
        text = raw.decode('utf-8-sig')
    except UnicodeDecodeError as error:
        raise ValueError('licence observation decoding failed') from error
    candidates, available = [], 0
    for version, issued, used, queued, reserved in _increments(text, feature):
        if used + reserved > issued:
            raise ValueError('contradictory licence counts')
        if _decimal(version) < minimum:
            continue
        if queued:
            raise ValueError('compatible licence increment has queued users')
        remaining = issued - used - reserved
        if remaining:
            candidates.append(version)
            available += remaining
    if available < 1:
        raise ValueError('no unused compatible licence candidate observed')
    return {'compatible_available': available, 'candidate_versions': candidates,
            'checkout_performed': False}
