# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Verify standard-header alignment preserves source bodies and parsed code."""

import argparse
import ast
import hashlib
import json
from pathlib import Path


def _body(text: str, plain: str, prefixed: str) -> str:
    for header in (prefixed, plain):
        if text.startswith(header + '\n'):
            return text[len(header):].lstrip('\n')
    return text


def main() -> None:
    """Compare saved pre-alignment bytes with each current Python file."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--before-root', type=Path, required=True)
    parser.add_argument('paths', nargs='+', type=Path)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    plain = (root / '.license-header.txt').read_text().rstrip('\r\n')
    prefixed = '\n'.join('# ' + line if line.strip() else line for line in plain.splitlines())
    records = []
    for path in args.paths:
        if path.is_absolute() or '..' in path.parts or path.suffix != '.py':
            raise ValueError('proof inputs must be relative Python paths')
        target = (root / path).resolve()
        if not target.is_relative_to(root):
            raise ValueError('proof input resolves outside the repository')
        before = (args.before_root / path).read_bytes()
        after = target.read_bytes()
        old, new = before.decode('utf-8'), after.decode('utf-8')
        if not new.startswith(prefixed + '\n\n'):
            raise ValueError('current source lacks the configured standard header')
        if _body(old, plain, prefixed) != _body(new, plain, prefixed):
            raise ValueError('a change extends beyond the standard header')
        if ast.dump(ast.parse(old)) != ast.dump(ast.parse(new)):
            raise ValueError('parsed code differs after header alignment')
        records.append({'path': path.as_posix(), 'before_sha256': hashlib.sha256(before).hexdigest(),
                        'after_sha256': hashlib.sha256(after).hexdigest(), 'header_only': True})
    print(json.dumps(records, indent=2, sort_keys=True))


if __name__ == '__main__':
    main()
