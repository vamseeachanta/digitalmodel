# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Build an offline, reviewed inventory without executing report producers.

Usage: uv run --no-project python scripts/reporting/capability_report_inventory.py
--root . --observations docs/audits/capability-report-inventory.json --out-dir OUTPUT

The JSON's observations block is explicit regeneration input. Generated counts,
coverage and gaps are separate. Source changes require a new review and new
source digests; a rebuild never silently updates an observation. RETAINED means
an accessible result record and criterion, never engineering qualification.

HTML links always target the publication location docs/audits, even when outputs
are staged elsewhere. Both outputs are byte deterministic for unchanged inputs;
there is no generated timestamp. Observation time is retained verbatim.
"""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import os
import re
import subprocess
from datetime import datetime
from html import escape
from pathlib import Path, PurePosixPath

STEM = 'capability-report-inventory'
STATUSES = ('RETAINED', 'INDICATIVE', 'MISSING', 'UNKNOWN')
DEFAULT_ROOTS = ('docs/reports', 'docs/domains/openfoam', 'docs/benchmarks',
                 'docs/subsea/cross_sections')
EXCLUDED_PARTS = {'private', 'confidential', '.git', '.agents', '.claude', 'audits'}


class InventoryError(ValueError):
    """Indicate invalid input without exposing source contents or host paths."""


def tracked_files(root: Path) -> set[str]:
    """Return versioned paths, without consuming untracked report inputs."""
    try:
        result = subprocess.run(['git', '-C', str(root), 'ls-files', '-z'],
                                capture_output=True, check=True, timeout=60)
    except (OSError, subprocess.SubprocessError) as exc:
        raise InventoryError('tracked-source discovery unavailable') from exc
    return set(result.stdout.decode('utf-8').split('\0')) - {''}


def checkout_revision(root: Path) -> str:
    """Read the checkout revision without changing Git metadata."""
    try:
        result = subprocess.run(['git', '-C', str(root), 'rev-parse', 'HEAD'],
                                capture_output=True, text=True, check=True, timeout=60)
    except (OSError, subprocess.SubprocessError) as exc:
        raise InventoryError('checkout revision unavailable') from exc
    return result.stdout.strip()


def _text(value, field: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise InventoryError(f'{field}: nonempty text required')
    return value


def _list(value, field: str) -> list:
    if not isinstance(value, list):
        raise InventoryError(f'{field}: list required')
    return value


def _path_name(value) -> str:
    _text(value, 'path')
    path = PurePosixPath(value)
    if (path.is_absolute() or '\\' in value or ':' in value or '#' in value
            or '?' in value or '%' in value or any(p in ('', '.', '..') for p in value.split('/'))
            or path.as_posix() != value):
        raise InventoryError('path must be normalized and repository-relative')
    return value


def _public(value: str) -> bool:
    return not (set(p.lower() for p in PurePosixPath(value).parts) & EXCLUDED_PARTS)


def _resolve(root: Path, value: str, tracked: set[str]) -> Path:
    name = _path_name(value)
    target = root / name
    if not target.resolve().is_relative_to(root):
        raise InventoryError('path escapes the checkout')
    # Symlinks cannot disguise private paths or substitute an untracked input.
    if any(parent.is_symlink() for parent in [target, *target.parents] if parent != root):
        raise InventoryError('path uses a symlink')
    if not _public(name) or name not in tracked or not target.is_file():
        raise InventoryError(f'source unavailable: {name}')
    return target


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def discover_reports(root: Path, roots: list[str], tracked: set[str]) -> list[str]:
    """Enumerate tracked HTML under reviewed roots, excluding audit outputs."""
    for scope in roots:
        _path_name(scope)
        if not (root / scope).resolve().is_relative_to(root):
            raise InventoryError('scope path escapes the checkout')
        if not (root / scope).is_dir():
            raise InventoryError(f'scope unavailable: {scope}')
    found = []
    for name in sorted(tracked):
        if (_public(name) and name.endswith('.html')
                and any(PurePosixPath(name).is_relative_to(scope) for scope in roots)):
            _resolve(root, name, tracked)
            found.append(name)
    return found


def _unique_object(pairs):
    result = {}
    for key, value in pairs:
        if key in result:
            raise InventoryError('duplicate JSON key')
        result[key] = value
    return result


def read_observations(path: Path) -> dict:
    """Read either an observations object or a previously generated inventory."""
    try:
        document = json.loads(path.read_text(encoding='utf-8'),
                              object_pairs_hook=_unique_object)
    except (OSError, UnicodeError, json.JSONDecodeError) as exc:
        raise InventoryError('observations JSON is unreadable or malformed') from exc
    if not isinstance(document, dict):
        raise InventoryError('observations must be an object')
    observations = document.get('observations', document)
    if not isinstance(observations, dict) or not isinstance(observations.get('reports'), list):
        raise InventoryError('observations reports must be a list')
    return observations


def _validate_row(root: Path, row: dict, tracked: set[str]) -> dict:
    if not isinstance(row, dict):
        raise InventoryError('report observation must be an object')
    row = copy.deepcopy(row)
    identifier = _text(row.get('id'), 'id')
    if not re.fullmatch('[a-z][a-z0-9-]*', identifier):
        raise InventoryError('id must use lowercase letters, digits and hyphens')
    for field in ('capability', 'criterion', 'observation'):
        _text(row.get(field), field)
    if row.get('evidence_status') not in STATUSES:
        raise InventoryError('unsupported evidence status')
    if row.get('qualification') not in ('UNKNOWN', 'INDICATIVE'):
        raise InventoryError('qualification cannot be established by this inventory')
    limitations = _list(row.get('limitations'), 'limitations')
    if not limitations:
        raise InventoryError('limitations must be explicit')
    for limitation in limitations:
        _text(limitation, 'limitation')
    sources = set()
    for field in ('report_path', 'generator_path', 'proposed_card_link'):
        value = row.get(field)
        if value is not None:
            _resolve(root, value, tracked)
            sources.add(value)
    if row.get('report_path') is None and row['evidence_status'] != 'MISSING':
        raise InventoryError('report unavailable: MISSING status required')
    if row.get('proposed_card_link') not in (None, row.get('report_path')):
        raise InventoryError('proposed card link must target the reviewed report')
    for path in _list(row.get('test_paths'), 'test_paths'):
        _resolve(root, path, tracked)
        sources.add(path)
    refs = _list(row.get('evidence_refs'), 'evidence_refs')
    kinds = set()
    implementation = {row.get('report_path'), row.get('generator_path'), *row['test_paths']}
    for ref in refs:
        if not isinstance(ref, dict) or ref.get('kind') not in ('result', 'criterion', 'reference'):
            raise InventoryError('evidence reference kind required')
        path = ref.get('path')
        _resolve(root, path, tracked)
        if (path in implementation or (ref['kind'] in ('reference', 'criterion')
                                       and PurePosixPath(path).parts[0] in ('src', 'scripts', 'tests'))):
            raise InventoryError('circular evidence reference to report implementation')
        sources.add(path)
        kinds.add(ref['kind'])
    if row['evidence_status'] == 'RETAINED' and not {'result', 'criterion'} <= kinds:
        raise InventoryError('RETAINED requires a result record and criterion source')
    if row['evidence_status'] == 'INDICATIVE' and not refs:
        raise InventoryError('INDICATIVE requires an accessible evidence reference')
    digests = row.get('source_digests')
    if not isinstance(digests, dict) or set(digests) != sources:
        raise InventoryError('source digest coverage mismatch')
    for path, expected in sorted(digests.items()):
        if not isinstance(expected, str) or not re.fullmatch('[0-9a-f]{64}', expected):
            raise InventoryError('invalid source digest')
        if _sha256(_resolve(root, path, tracked)) != expected:
            raise InventoryError(f'stale observation digest: {path}')
    if 'priority' in row and (type(row['priority']) is not int or row['priority'] < 1):
        raise InventoryError('priority must be a positive integer')
    if 'next_action' in row:
        _text(row['next_action'], 'next_action')
    row['test_paths'] = sorted(set(row['test_paths']))
    row['evidence_refs'] = sorted(refs, key=lambda ref: (ref['kind'], ref['path']))
    return row


def build_inventory(root: Path, observations: dict) -> dict:
    """Validate reviewed evidence and derive counts without changing judgments."""
    root = root.resolve()
    if not isinstance(observations, dict):
        raise InventoryError('observations object required')
    observations = copy.deepcopy(observations)
    revision = observations.get('source_revision')
    if not isinstance(revision, str) or not re.fullmatch('[0-9a-f]{40,64}', revision):
        raise InventoryError('source revision required')
    timestamp = _text(observations.get('observed_at'), 'observed_at')
    try:
        parsed = datetime.fromisoformat(timestamp.replace('Z', '+00:00'))
    except ValueError as exc:
        raise InventoryError('observation timestamp is invalid') from exc
    if parsed.tzinfo is None:
        raise InventoryError('observation timestamp requires a timezone')
    history = _list(observations.get('revision_history'), 'revision_history')
    if not history:
        raise InventoryError('revision history required')
    for entry in history:
        if not isinstance(entry, dict):
            raise InventoryError('revision row must be an object')
        for key in ('revision', 'date', 'change'):
            _text(entry.get(key), f'revision {key}')
    roots = _list(observations.get('scope_roots'), 'scope_roots')
    if not roots:
        raise InventoryError('scope roots required')
    tracked = tracked_files(root)
    discovered = discover_reports(root, roots, tracked)
    rows = [_validate_row(root, row, tracked)
            for row in _list(observations.get('reports'), 'reports')]
    ids = [row['id'] for row in rows]
    if len(ids) != len(set(ids)):
        raise InventoryError('duplicate report ids')
    rows.sort(key=lambda row: row['id'])
    observations['reports'] = rows
    observations['scope_roots'] = sorted(set(roots))
    reviewed = {row['report_path'] for row in rows if row.get('report_path')}
    if not reviewed <= set(discovered):
        raise InventoryError('reviewed report path is outside discovery scope')
    gaps = []
    for row in rows:
        status = row['evidence_status']
        if status in ('MISSING', 'UNKNOWN'):
            gaps.append({'report_id': row['id'], 'kind': 'missing-evidence' if status == 'MISSING'
                         else 'unreviewed-evidence', 'priority': row.get('priority', 2),
                         'action': row.get('next_action', 'Review accessible evidence against a named criterion.')})
        if status == 'INDICATIVE' or (status == 'RETAINED' and row['qualification'] == 'INDICATIVE'):
            gaps.append({'report_id': row['id'], 'kind': 'qualification',
                         'priority': row.get('priority', 1),
                         'action': row.get('next_action', 'Resolve the stated limitations before stronger claims.')})
        for field in ('generator_path', 'test_paths', 'proposed_card_link'):
            if not row.get(field):
                gaps.append({'report_id': row['id'], 'kind': f'missing-{field}',
                             'priority': 2, 'action': f'Locate and review {field}.'})
    unreviewed = len(set(discovered) - reviewed)
    if unreviewed:
        gaps.append({'report_id': None, 'kind': 'coverage', 'priority': 3,
                     'action': f'Review {unreviewed} additional tracked HTML pages in the declared roots; '
                               'these include supporting pages and are not counts of missing reports.'})
    gaps.sort(key=lambda gap: (gap['priority'], gap['report_id'] or '', gap['kind']))
    return {
        'schema_version': 1, 'source_revision': revision, 'observations': observations,
        'reports': copy.deepcopy(rows), 'gaps': gaps,
        'generated': {
            'counts': {status: sum(row['evidence_status'] == status for row in rows) for status in STATUSES},
            'discovered_roots': sorted({scope for scope in roots
                                        if any(PurePosixPath(path).is_relative_to(scope) for path in discovered)}),
            'discovered_report_count': len(discovered),
            'unreviewed_report_count': unreviewed,
            'discovery_sha256': hashlib.sha256('\n'.join(discovered).encode()).hexdigest(),
            'source_digests': {path: digest for row in rows
                               for path, digest in sorted(row['source_digests'].items())},
        },
    }


def _link(path: str | None) -> str:
    if path is None:
        return 'Not identified'
    href = os.path.relpath(path, 'docs/audits').replace(os.sep, '/')
    return f'<a href="{escape(href, quote=True)}">{escape(path)}</a>'


def render_html(document: dict) -> str:
    """Render a standalone inventory with links relative to docs/audits."""
    counts = document['generated']['counts']
    observations = document['observations']
    parts = ['<!doctype html>', '<html lang="en"><head><meta charset="utf-8">',
             '<meta name="viewport" content="width=device-width, initial-scale=1">',
             '<title>Capability report evidence inventory</title>',
             '<style>body{font:16px system-ui;line-height:1.5;max-width:1100px;margin:2rem auto;padding:0 1rem}',
             'table{border-collapse:collapse;width:100%}th,td{border:1px solid #bbb;padding:.5rem;text-align:left;vertical-align:top}',
             'code{overflow-wrap:anywhere}a{overflow-wrap:anywhere}section{margin:2rem 0}',
             '@media print{body{max-width:none}a{color:inherit}}</style></head><body>',
             '<h1>Capability report evidence inventory</h1>', '<h2>Summary</h2>',
             f'<p>{len(document["reports"])} reviewed inventory records: '
             + '; '.join(f'{status}: {counts[status]}' for status in STATUSES) + '.</p>',
             '<p>RETAINED requires an accessible result record, source digest and criterion. '
             'It does not establish engineering qualification. Tests listed here are source pointers; '
             'this inventory does not execute them or infer execution from a green check.</p>',
             '<h2>Revision history</h2>',
             '<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead><tbody>']
    for row in observations['revision_history']:
        parts.append('<tr>' + ''.join(f'<td>{escape(row[key])}</td>'
                                      for key in ('revision', 'date', 'change')) + '</tr>')
    parts += ['</tbody></table>', '<h2>Scope and method</h2>',
              f'<p>Observation time: {escape(observations["observed_at"])}. '
              f'Source revision: <code>{document["source_revision"]}</code>.</p>',
              '<p>Discovery roots: ' + ', '.join(escape(p) for p in observations['scope_roots']) + '.</p>',
              f'<p>{document["generated"]["discovered_report_count"]} tracked HTML pages were enumerated; '
              f'{document["generated"]["unreviewed_report_count"]} are outside the reviewed subset. '
              'Supporting pages are included in discovery. This is not a complete repository census.</p>',
              '<p>Only tracked, public paths are eligible. No report producer or solver runs. '
              'Audit outputs are excluded from discovery and source hashes. '
              'The JSON observations block preserves judgments, criteria, observation time and source digests. '
              'Regeneration refuses stale source digests. Links assume publication in docs/audits; '
              'an output directory elsewhere is staging only. There is no generated timestamp to exclude.</p>',
              '<h2>Reviewed reports and proposed card links</h2>']
    for row in document['reports']:
        parts += [f'<section id="{escape(row["id"], quote=True)}"><h3>{escape(row["capability"])}</h3>',
                  f'<p>Evidence status: <strong>{row["evidence_status"]}</strong>; '
                  f'qualification: {row["qualification"]}.</p>',
                  f'<p>Report and proposed card target: {_link(row.get("proposed_card_link"))}.</p>',
                  f'<p>Report source: {_link(row.get("report_path"))}. '
                  f'Generator: {_link(row.get("generator_path"))}.</p>',
                  '<p>Test sources (execution not asserted): '
                  + (', '.join(_link(p) for p in row['test_paths']) or 'Not identified') + '.</p>',
                  f'<p>Criterion: {escape(row["criterion"])}</p>',
                  f'<p>Observation: {escape(row["observation"])}</p>', '<p>Evidence references:</p><ul>']
        parts += [f'<li>{escape(ref["kind"])}: {_link(ref["path"])}</li>' for ref in row['evidence_refs']]
        parts += ['</ul><p>Limitations:</p><ul>']
        parts += [f'<li>{escape(item)}</li>' for item in row['limitations']]
        parts += ['</ul><details><summary>Reviewed source digests (SHA-256)</summary><ul>']
        parts += [f'<li>{_link(path)}: <code>{digest}</code></li>'
                  for path, digest in sorted(row['source_digests'].items())]
        parts += ['</ul></details></section>']
    parts += ['<h2>Evidence gaps and prioritized next actions</h2>', '<ol>']
    for gap in document['gaps']:
        parts.append(f'<li>Priority {gap["priority"]}; {escape(gap["report_id"] or "coverage")}; '
                     f'{escape(gap["kind"])}: {escape(gap["action"])}</li>')
    parts += ['</ol>', '<h2>Known limits</h2>',
              '<p>This inventory establishes record accessibility and review boundaries, not engineering '
              'accuracy or completion of missing reports. Private data, external records, solver results '
              'not retained here, engineering calculations and website linking are not evaluated. '
              'Implementation sources cannot serve as independent engineering references. '
              'Proposed links are local files; website changes require separate work.</p>',
              '</body></html>']
    return '\n'.join(parts) + '\n'


def write_outputs(document: dict, out_dir: Path) -> None:
    """Write only the two named artifacts, refusing symlink output targets."""
    out_dir.mkdir(parents=True, exist_ok=True)
    targets = [out_dir / f'{STEM}.json', out_dir / f'{STEM}.html']
    if out_dir.is_symlink() or any(path.is_symlink() for path in targets):
        raise InventoryError('output path uses a symlink')
    targets[0].write_text(json.dumps(document, indent=2, sort_keys=True, ensure_ascii=False) + '\n',
                          encoding='utf-8')
    targets[1].write_text(render_html(document), encoding='utf-8')


def initial_observations(root: Path) -> dict:
    """Create unknown observations; never guess a producer or evidence status."""
    tracked = tracked_files(root)
    roots = [path for path in DEFAULT_ROOTS if (root / path).is_dir()]
    rows = []
    for number, path in enumerate(discover_reports(root, roots, tracked), 1):
        rows.append({
            'id': f'report-{number}', 'report_path': path, 'generator_path': None,
            'test_paths': [], 'capability': 'Unreviewed report', 'evidence_status': 'UNKNOWN',
            'qualification': 'UNKNOWN', 'evidence_refs': [],
            'criterion': 'No engineering criterion has been reviewed.',
            'observation': 'A tracked HTML page exists; report purpose and evidence need review.',
            'limitations': ['Discovery alone does not establish a capability.'],
            'proposed_card_link': None, 'source_digests': {path: _sha256(root / path)},
        })
    now = datetime.now().astimezone()
    return {'source_revision': checkout_revision(root), 'observed_at': now.isoformat(),
            'scope_roots': roots, 'reports': rows,
            'revision_history': [{'revision': '0', 'date': now.date().isoformat(),
                                  'change': 'Unreviewed discovery; evidence review is required.'}]}


def main(argv=None) -> int:
    """Run the offline builder and report validation errors without source data."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path('.'))
    parser.add_argument('--observations', type=Path)
    parser.add_argument('--out-dir', type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        root = args.root.resolve()
        observations = (read_observations(args.observations) if args.observations
                        else initial_observations(root))
        document = build_inventory(root, observations)
        out = args.out_dir.resolve()
        if out.is_relative_to(root) and out != root / 'docs/audits':
            raise InventoryError('output inside checkout must use docs/audits')
        write_outputs(document, args.out_dir)
    except InventoryError as exc:
        parser.exit(2, f'inventory rejected: {exc}\n')
    print(json.dumps(document['generated']['counts'], sort_keys=True))
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
