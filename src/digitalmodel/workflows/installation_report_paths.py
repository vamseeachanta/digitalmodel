"""Presentation-only case mappings for retained installation summaries.

``dataset_root`` is relative to the report directory; ``case_run_dirs`` maps
all stringified case indices to POSIX paths beneath that dataset root. Source
rows remain unchanged. Without a mapping, existing link behavior is retained.
This resolves locators only; it does not verify the linked evidence contents.
"""
import os
from pathlib import Path, PurePosixPath


def display_cases(summary, config, base):
    """Resolve explicit portable link mappings without changing pinned source rows."""
    if 'case_run_dirs' not in config:
        return summary['cases']
    mapping = config['case_run_dirs']
    expected = {str(row['index']) for row in summary['cases']}
    if not isinstance(mapping, dict) or set(mapping) != expected:
        raise ValueError('Complete case_run_dirs mapping required')
    locator = config.get('dataset_root')
    if not isinstance(locator, str) or not locator or locator.startswith('/') or '\\' in locator or ':' in locator:
        raise ValueError('dataset_root must be an explicit relative locator')
    root = Path(os.path.normpath(os.path.join(base, locator))).resolve()
    rows = []
    for row in summary['cases']:
        entry = mapping[str(row['index'])]
        if (not isinstance(entry, str) or not entry or '\\' in entry or ':' in entry
                or entry.startswith('/') or '..' in PurePosixPath(entry).parts):
            raise ValueError('Case mapping entries must be relative without traversal')
        run_dir = os.path.normpath(os.path.join(base, locator, entry))
        resolved = Path(run_dir).resolve()
        if not resolved.is_relative_to(root):
            raise ValueError('Case mapping resolves outside dataset root')
        rows.append(dict(row, run_dir=run_dir))
    return rows
