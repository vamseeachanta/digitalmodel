"""Isolated citation metadata; maintained pages when explicitly configured.

Unconfigured runs use synthetic resolver records, which do not qualify factors.
Configured runs copy the actual page and cannot fall back to those records.
Production calculations resolve the maintained wiki without this fixture.
"""
import os
from pathlib import Path
import shutil

import pytest


@pytest.fixture(autouse=True)
def citation_metadata(tmp_path, monkeypatch):
    maintained_root = os.environ.get('LLM_WIKI_PATH')
    records = [
        ('engineering', 'ocimf-meg4', 'OCIMF-MEG4', 'OCIMF', '4th-Edition-2018'),
    ]
    for domain, slug, code, publisher, revision in records:
        page = tmp_path / 'wikis' / domain / 'wiki' / 'standards' / f'{slug}.md'
        page.parent.mkdir(parents=True, exist_ok=True)
        if maintained_root:
            relative = page.relative_to(tmp_path)
            source = Path(maintained_root) / relative
            if not source.is_file():
                source = Path(maintained_root) / 'knowledge' / relative
            # Missing or mismatched maintained metadata must fail; no synthetic fallback.
            shutil.copyfile(source, page)
        else:
            page.write_text(f'---\ncode_id: {code}\npublisher: "{publisher}"\n'
                            f'revision: "{revision}"\n---\nSynthetic resolver metadata.\n')
    monkeypatch.setenv('LLM_WIKI_PATH', str(tmp_path))
    return tmp_path
