# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Public report checks, independent arithmetic anchors and rebuild consistency."""

import importlib
import json
import math
import re
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / 'docs/domains/cathodic_protection/reports/published-jacket-84m'


@pytest.fixture
def builder(monkeypatch):
    monkeypatch.syspath_prepend(str(ROOT / 'scripts/reporting'))
    return importlib.import_module('build_published_jacket_analysis')


def test_three_hand_calculation_anchors(builder):
    cfg = builder.calculate()
    demand = cfg['results']['current_demand_A']
    # Hand calculation: sum(area × initial density) + 9 × 2 = 2688.285 A.
    assert demand['total_initial_A'] == pytest.approx(2688.285, abs=1e-9)
    # Hand calculation: 652.9×.02 + 3726.8×.07 + 15737×.06 + 10814.8×.02 + 18.
    assert demand['total_mean_A'] == pytest.approx(1452.450, abs=1e-9)
    # Hand calculation: 652.9×.04 + 3726.8×.1 + 15737×.08 + 10814.8×.02 + 18.
    assert demand['total_final_A'] == pytest.approx(1892.052, abs=1e-9)
    # Published-output initial count: ceil(2688.285 / 5.01) = 537.
    assert math.ceil(demand['total_initial_A'] / 5.01) == 537


def test_saved_numbers_match_fresh_workflow(builder):
    saved = json.loads((OUTPUT / 'results.json').read_text(encoding='utf-8'))
    fresh = builder.numerical_record(builder.calculate())
    assert saved['current_A'] == pytest.approx(fresh['current_A'], rel=1e-12)
    assert saved['anode_counts'] == fresh['anode_counts']
    assert saved['anode_counts'] == {
        'installed': 569, 'mass': 547, 'initial': 575, 'final': 556, 'recommended': 575,
    }


def test_public_page_and_rebuild(builder, tmp_path):
    builder.build(tmp_path)
    for path in OUTPUT.iterdir():
        if not path.is_file() or path.name == 'checksums.json':
            continue
        assert (tmp_path / path.name).read_bytes() == path.read_bytes(), (
            'Report sources changed: rebuild with scripts/reporting/'
            'build_published_jacket_analysis.py and commit the report.'
        )
    builder.verify_pack(tmp_path)
    builder.verify_pack(OUTPUT)
    page = (tmp_path / 'cathodic-protection-analysis.html').read_text(encoding='utf-8')
    assert 'Draft for maintainer review' in page
    assert 'worked example' in page
    assert '22 ohm.cm × 0.01 = 0.22 ohm.m' in page
    assert 'Hand calculations check' in page
    assert 'Tool checks compare' in page
    assert 'separate reviewed issue' in page
    assert 'stated assumption' in page
    for phrase in ('Internal review draft', 'client use', 'client-facing', 'previous project',
                   'protective potential criterion</td>',
                   *(f'/{part}/' for part in ('home', 'mnt'))):
        assert phrase not in page
    assert page.index('Executive Summary</h2>') < page.index('Revision history</h2>')
    assert page.index('Revision history</h2>') < page.index('Introduction</h2>')
    assert page.count('class="l1head"') == 13  # ten sections, history, two appendices
    assert 'showSaveFilePicker' in page and 'changes.json' in page
    assert not re.search(r'<(?:script|link)[^>]+(?:src|href)=["\']https?://', page)
    assert 'data-type="depth_true"' in page
    manifest = json.loads((tmp_path / 'cathodic-protection-analysis_manifest.json').read_text(encoding='utf-8'))
    assert manifest['pdf']['rendered'] is False
    citations = json.loads((tmp_path / 'cathodic-protection-analysis_citations.json').read_text(encoding='utf-8'))
    assert all('potential' not in c['section'] for c in citations['citations'])
    assert all('stated assumption' in c['note'] for c in citations['citations'])


@pytest.mark.parametrize('value', [None, float('nan'), float('inf'), 'not applicable'])
def test_missing_or_invalid_criterion_fails_closed(builder, value):
    cfg = yaml.safe_load(builder.FIXTURE.read_text(encoding='utf-8'))
    cfg['inputs']['structure']['report_geometry']['criterion_V'] = value
    with pytest.raises(ValueError, match='criterion'):
        builder.calculate(cfg)


def test_missing_criterion_and_basis_fail_closed(builder):
    for key in ('criterion_V', 'criterion_source'):
        cfg = yaml.safe_load(builder.FIXTURE.read_text(encoding='utf-8'))
        del cfg['inputs']['structure']['report_geometry'][key]
        with pytest.raises(ValueError, match='criterion'):
            builder.calculate(cfg)


def test_missing_published_output_fails_closed(builder):
    cfg = builder.calculate()
    cfg['inputs']['structure']['verification_evidence']['targets'][3]['printed'] = None
    with pytest.raises(ValueError, match='published'):
        builder.build_document(cfg)


@pytest.mark.parametrize('group, key', [
    ('design_data', 'design_life'), ('environment', 'seawater_resistivity_ohm_m'),
    ('anode', 'count'),
])
def test_required_input_never_defaults_to_a_pass(builder, group, key):
    cfg = yaml.safe_load(builder.FIXTURE.read_text(encoding='utf-8'))
    del cfg['inputs'][group][key]
    with pytest.raises(ValueError, match='NOT_EVALUATED'):
        builder.calculate(cfg)
