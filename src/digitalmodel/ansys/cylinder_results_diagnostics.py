"""Native diagnostic envelope grammar; ordinary output remains unclassified."""
import hashlib
import re

KNOWN = {'NOTE', 'WARNING', 'ERROR', 'FATAL', 'FATAL ERROR'}
ENVELOPE = re.compile(rb'\b(?:CP|TIME)\s*=|\bELAPSED TIME\s*=')
HEADER = re.compile(rb'^\s*\*\*\*\s+([A-Z][A-Z ]*?)\s+\*\*\*(?:\s.*)?$')
TOTAL = re.compile(rb'^\s*NUMBER OF (WARNING|ERROR)\s+MESSAGES ENCOUNTERED\s*=\s*(\d+)\s*$')
SUPPRESSION = re.compile(rb'(?:messages?\s+(?:suppressed|truncated)|output\s+truncated|'
                         rb'maximum\s+number\s+of\s+(?:warning|error)\s+messages)', re.I)


def _events(raw, label, errors):
    events, totals, offset = [], {}, 0
    for line in raw.splitlines(keepends=True):
        text = line.rstrip(b'\r\n')
        match = HEADER.fullmatch(text)
        severity = match[1].decode('ascii').strip() if match else None
        candidate = severity in KNOWN or bool(ENVELOPE.search(text))
        if match and candidate:
            events.append({'file': label, 'byte_offset': offset, 'severity': severity,
                           'header_sha256': hashlib.sha256(text).hexdigest()})
            if severity != 'NOTE':
                errors.append(f'{label}: blocking or unsupported severity {severity}')
        elif re.match(rb'^\s*\*{2,}', text) and (
                re.match(rb'^\s*\*{2,}\s*(?:WARNING|ERROR|FATAL|NOTE)\b', text)
                or (re.match(rb'^\s*\*{3}(?!\*)', text) and ENVELOPE.search(text)
                    and b'***' in text.lstrip()[3:])):
            # Untimed informational headings and routine banners are not diagnostics.
            errors.append(f'{label}: malformed diagnostic candidate at {offset}')
        total = TOTAL.fullmatch(text)
        if total:
            name = total[1].decode('ascii')
            if name in totals:
                errors.append(f'{label}: duplicate final total')
            totals[name] = int(total[2])
        elif b'MESSAGES ENCOUNTERED' in text:
            errors.append(f'{label}: malformed final total')
        if SUPPRESSION.search(text):
            errors.append(f'{label}: suppressed or truncated diagnostic evidence')
        offset += len(line)
    return events, totals


def classify_diagnostics(output, error_file, stdout, stderr, *, nerr_nmerr=None):
    """Return COMPLETE/INCOMPLETE; caller must bind the parsed NERR state to bytes."""
    errors, events, totals, coverage = [], [], {}, []
    if type(nerr_nmerr) is not int or nerr_nmerr < 0:
        errors.append('Missing or negative /NERR message limit')
    for label, raw in [('output', output), ('error', error_file),
                       ('stdout', stdout), ('stderr', stderr)]:
        if not isinstance(raw, bytes):
            errors.append(f'{label}: missing captured bytes')
            continue
        coverage.append({'file': label, 'bytes': len(raw),
                         'sha256': hashlib.sha256(raw).hexdigest()})
        if label in ('stdout', 'stderr') and raw:
            errors.append(f'{label}: strict process stream must be byte-empty')
        try:
            raw.decode('utf-8', errors='strict')
        except UnicodeDecodeError:
            errors.append(f'{label}: decoding loss')
        if raw and not raw.endswith(b'\n'):
            errors.append(f'{label}: unterminated/truncated output')
        found, counts = _events(raw, label, errors)
        events.extend(found)
        if label == 'output':
            totals = counts
    if set(totals) != {'WARNING', 'ERROR'}:
        errors.append('Missing final native warning/error totals')
    for severity in ('WARNING', 'ERROR'):
        if totals.get(severity, 0):
            errors.append(f'Nonzero final {severity} total')
        observed = sum(e['severity'] == severity and e['file'] == 'output' for e in events)
        if severity in totals and observed != totals[severity]:
            errors.append(f'Inconsistent final {severity} total')
    return {'status': 'INCOMPLETE' if errors else 'COMPLETE', 'errors': errors,
            'events': events, 'totals': totals, 'coverage': coverage}
