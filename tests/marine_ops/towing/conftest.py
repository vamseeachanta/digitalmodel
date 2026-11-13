"""Synthetic metadata for offline citation resolution; no licensed source text.

These records exercise resolver wiring only. They do not independently qualify
any engineering factors. Production calculations resolve the maintained wiki.
"""
import pytest


@pytest.fixture(autouse=True)
def citation_metadata(tmp_path, monkeypatch):
    records = [
        ('engineering', 'ocimf-meg4', 'OCIMF-MEG4', 'OCIMF', '4th-Edition-2018'),
        ('engineering-standards', 'dnvgl-st-n001', 'dnvgl-st-n001',
         'DNV GL (now DNV)', '2016-06'),
    ]
    for domain, slug, code, publisher, revision in records:
        page = tmp_path / 'wikis' / domain / 'wiki' / 'standards' / f'{slug}.md'
        page.parent.mkdir(parents=True, exist_ok=True)
        page.write_text(f'---\ncode_id: {code}\npublisher: "{publisher}"\n'
                        f'revision: "{revision}"\n---\nSynthetic resolver metadata.\n')
    monkeypatch.setenv('LLM_WIKI_PATH', str(tmp_path))
    return tmp_path
