"""Text-extractable credential screening; compressed/binary semantics are not covered."""
import argparse
import json
from pathlib import Path
import re

COVERAGE = 'text-extractable-only; no compressed or binary semantic coverage'
BUILTINS = (
    ('github-token', r'\b(?:gh[pousr]_[A-Za-z0-9]{36}|github_pat_[A-Za-z0-9_]{60,})\b'),
    ('provider-token', r'\bsk-(?:proj-|ant-api[0-9]+-)?[A-Za-z0-9_-]{32,}\b'),
    ('private-key', r'-----BEGIN (?:RSA |EC |DSA |OPENSSH |ENCRYPTED )?PRIVATE KEY-----'),
    ('aws-access-key', r'\b(?:AKIA|ASIA)[A-Z0-9]{16}\b'),
    ('credential-assignment', r'(?i)\b(?:[a-z][a-z0-9]*[_-])*'
     r'(?:password|passwd|api[_-]?key|access[_-]?token|client[_-]?secret)'
     r'''["']?\s*[:=]\s*(?:["'][^"'\r\n]{8,}["']|[^\s"'`$;,{}\[\]][^\s"'`;,{}\[\]]{7,})'''),
)


def _rules(deny_rules):
    if not isinstance(deny_rules, list):
        raise ValueError('Deny rules must be a list')
    rules = [(name, re.compile(pattern)) for name, pattern in BUILTINS]
    ids = {name for name, _ in rules}
    for row in deny_rules:
        if (not isinstance(row, dict) or set(row) != {'id','pattern','case_sensitive','severity'}
                or not isinstance(row['id'], str) or not re.fullmatch(r'[a-zA-Z0-9_-]+',row['id'])
                or row['id'] in ids or not isinstance(row['pattern'], str) or not row['pattern']
                or type(row['case_sensitive']) is not bool
                or not isinstance(row['severity'], str) or not row['severity'].strip()):
            raise ValueError('Invalid or duplicate deny rule')
        try:
            pattern = re.compile(row['pattern'], 0 if row['case_sensitive'] else re.IGNORECASE)
        except re.error as error:
            raise ValueError('Invalid deny pattern') from error
        ids.add(row['id'])
        rules.append((row['id'], pattern))
    return rules


def screen_bytes(raw, deny_rules=None):
    """Return matching rule IDs only, never matched values or qualification."""
    if not isinstance(raw, bytes):
        raise ValueError('Raw bytes required')
    rules = _rules([] if deny_rules is None else deny_rules)
    texts = [raw.decode('ascii', errors='replace')]
    for encoding in ('utf-16-le','utf-16-be'):
        for offset in (0,1):
            text = raw[offset:]
            texts.append(text[:len(text)//2*2].decode(encoding, errors='replace'))
    return sorted({name for name, pattern in rules if any(pattern.search(text) for text in texts)})


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('files', nargs='+')
    parser.add_argument('--deny-rules')
    args = parser.parse_args()
    try:
        rules = json.loads(Path(args.deny_rules).read_text('utf-8')) if args.deny_rules else []
        ids = sorted({rule for name in args.files for rule in screen_bytes(Path(name).read_bytes(), rules)})
    except (ValueError, OSError) as error:
        parser.exit(2, type(error).__name__+': screening failed\n')
    print(json.dumps(dict(rule_ids=ids, coverage=COVERAGE), sort_keys=True))
    return 1 if ids else 0


if __name__ == '__main__':
    raise SystemExit(main())
