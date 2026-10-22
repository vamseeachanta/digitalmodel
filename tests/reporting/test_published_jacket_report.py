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

from digitalmodel.citations import CitationResolutionError

ROOT = Path(__file__).resolve().parents[2]
OUTPUT = ROOT / 'docs/domains/cathodic_protection/reports/published-jacket-84m'
ARTIFACTS = {
    'cathodic-protection-analysis.html', 'cathodic-protection-analysis_citations.json',
    'cathodic-protection-analysis_manifest.json', 'changes.json', 'checksums.json', 'results.json',
}


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


def test_anode_counts_have_independent_hand_calculations(builder):
    cfg = builder.calculate()
    saved = json.loads((OUTPUT / 'results.json').read_text(encoding='utf-8'))
    # Fixture verification_evidence.assumptions: 25 yr, 2000 Ah/kg, 0.90
    # utilisation, 2750 kg/m³ and equivalent cylinders at the 2.020 m length.
    # These inputs are assumed, based on previous experience. The paper gives
    # 323.4 kg, 0.22 ohm.m, 0.25 V and 569 installed: p. 242, Section 2.
    # Method: DNV-RP-B401, May 2021, Anode mass; Anode current output;
    # Anode resistance, Table 8-7 (long slender stand-off anode).
    # Hand mass: 1452.450 × 25 × 8760 / (2000 × 0.90) = 176714.8 kg.
    required_mass = 1452.450 * 25 * 8760 / (2000 * 0.90)
    assert required_mass == pytest.approx(176714.8, abs=0.05)
    # r = sqrt(m / (π L ρ)); R = ρ_water (ln(4 L / r) − 1) / (2 π L).
    # Fresh: r = 0.136130 m, R = 0.053449 ohm, I = 0.25 / R = 4.6773 A.
    fresh_radius = math.sqrt(323.4 / (math.pi * 2.020 * 2750))
    fresh_resistance = 0.22 * (math.log(4 * 2.020 / fresh_radius) - 1) / (2 * math.pi * 2.020)
    fresh_output = 0.25 / fresh_resistance
    assert fresh_output == pytest.approx(4.6773, abs=0.00005)
    # Depleted: 10 % mass remains; r = 0.043048 m, R = 0.073405 ohm.
    # I_final = 0.25 / R_final = 3.4057 A, with length held at 2.020 m.
    depleted_radius = math.sqrt(323.4 * (1 - 0.90) / (math.pi * 2.020 * 2750))
    depleted_resistance = 0.22 * (math.log(4 * 2.020 / depleted_radius) - 1) / (2 * math.pi * 2.020)
    depleted_output = 0.25 / depleted_resistance
    assert depleted_output == pytest.approx(3.4057, abs=0.00005)
    # ceil(176714.8/323.4)=547; ceil(2688.285/4.6773)=575;
    # ceil(1892.052/3.4057)=556; governing count=max(547,575,556)=575.
    counts = {
        'installed': 569,
        'mass': math.ceil(required_mass / 323.4),
        'initial': math.ceil(2688.285 / fresh_output),
        'final': math.ceil(1892.052 / depleted_output),
    }
    counts['recommended'] = max(counts[key] for key in ('mass', 'initial', 'final'))
    assert saved['anode_counts'] == counts
    assert builder.numerical_record(cfg)['anode_counts'] == counts


def test_citations_resolve_from_repository_root(builder, tmp_path, monkeypatch):
    page = tmp_path / 'knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-b401-2021.md'
    page.parent.mkdir(parents=True)
    page.write_text('---\ncode_id: dnv-rp-b401\npublisher: DNV\nrevision: "2021-05"\n---\n',
                    encoding='utf-8', newline='\n')
    monkeypatch.setattr(builder, 'ROOT', tmp_path)
    monkeypatch.setattr(builder, 'DESTINATION', tmp_path / 'reports')
    spec, _ = builder.build_document()
    assert spec.citations[0]['wiki_path'] == page.relative_to(tmp_path / 'knowledge').as_posix()


def test_missing_canonical_citation_fails_closed(builder, tmp_path, monkeypatch):
    monkeypatch.setattr(builder, 'ROOT', tmp_path)
    with pytest.raises(CitationResolutionError, match='page_missing'):
        builder.build_document()


def test_public_page_and_rebuild(builder, tmp_path):
    assert {p.relative_to(OUTPUT).as_posix() for p in OUTPUT.rglob('*') if p.is_file()} == ARTIFACTS
    builder.build(tmp_path)
    assert {p.relative_to(tmp_path).as_posix() for p in tmp_path.rglob('*') if p.is_file()} == ARTIFACTS
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
