"""Synthetic generator isolation for packaging; no native or include-closure claim."""
import importlib.util
import json
from datetime import date
from pathlib import Path
import sys
import pytest

ROOT = Path(__file__).resolve().parents[2]


def _load_packaging_offline():
    missing = object()
    prior_api = sys.modules.get('OrcFxAPI', missing)
    prior_path = sys.path[:]
    try:
        sys.modules['OrcFxAPI'] = None
        spec = importlib.util.spec_from_file_location(
            'mooring_packaging', ROOT/'scripts/prepare_mooring_qualification.py')
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    finally:
        sys.path[:] = prior_path
        if prior_api is missing:
            sys.modules.pop('OrcFxAPI', None)
        else:
            sys.modules['OrcFxAPI'] = prior_api


packaging = _load_packaging_offline()


def test_packaging_loader_restores_optional_api_and_sys_path(monkeypatch):
    sentinel = object()
    monkeypatch.setitem(sys.modules, 'OrcFxAPI', sentinel)
    before = sys.path[:]
    assert _load_packaging_offline().prepare_bundle
    assert sys.modules['OrcFxAPI'] is sentinel
    assert sys.path == before


@pytest.fixture
def isolated(tmp_path, monkeypatch):
    source = tmp_path/'template.yml'
    source.write_bytes(b'environment:\n  water_depth: 100\n')
    reference = tmp_path/'reference.yml'
    reference.write_bytes(b'General: {}\n')
    monkeypatch.setattr(packaging, 'SOURCE', source)
    monkeypatch.setattr(packaging, 'REFERENCE', reference)
    monkeypatch.setenv('PYTHONHASHSEED', '0')
    monkeypatch.setattr(packaging, 'load_contract', lambda: {'case_id': 'synthetic', 'source': 'governing-template'})
    monkeypatch.setattr(packaging.subprocess, 'check_output', lambda *a, **kw: 'a'*40)
    class SyntheticGenerator:
        def __init__(self, selected):
            self.selected = selected
        def generate(self, root):
            root.mkdir()
            (root/'master.yml').write_text('- includefile: 01_general.yml\n', encoding='utf-8')
            (root/'01_general.yml').write_text('General: {}\n', encoding='utf-8')
    monkeypatch.setattr(packaging, 'ModularModelGenerator', SyntheticGenerator)
    # Manifest coverage is checked here; this stub does not exercise native include closure.
    monkeypatch.setattr(packaging, 'verify_manifest', lambda path: None)
    derived = tmp_path/'derived.yml'
    derived.write_bytes(source.read_bytes()+b'  raw_properties:\n    WindType: Constant\n    VerticalWindVariationFactor: null\n')
    return source, reference, derived


def test_exact_two_leaf_derivation_and_raw_template_retained(tmp_path, isolated):
    source, reference, derived = isolated
    raw, refraw = source.read_bytes(), reference.read_bytes()
    path = packaging.prepare_bundle(tmp_path/'output', source_path=derived)
    manifest = json.loads(path.read_text(encoding='utf-8'))
    root = path.parent
    assert (root/'source/template.yml').read_bytes() == raw
    assert (root/'source/spec.yml').read_bytes() == derived.read_bytes()
    assert source.read_bytes() == raw and reference.read_bytes() == refraw
    derivation = json.loads((root/'source/derivation.json').read_text(encoding='utf-8'))
    assert derivation['applied_keys'] == ['/environment/raw_properties/VerticalWindVariationFactor',
                                          '/environment/raw_properties/WindType']
    assert derivation['contract_source_role'] == 'governing_template'
    assert derivation['template_sha256'] == packaging.digest(source)
    assert derivation['source_sha256'] == packaging.digest(derived)
    assert derivation['reference_sha256'] == packaging.digest(reference)
    files = {item['path']: item['sha256'] for item in manifest['files']}
    assert files['source/derivation.json'] == packaging.digest(root/'source/derivation.json')
    assert files['source/template.yml'] == packaging.digest(source)
    assert files['reference-differences.json'] == packaging.digest(root/'reference-differences.json')
    report = json.loads((root/'reference-differences.json').read_text(encoding='utf-8'))
    assert report['contract_source_role'] == 'governing_template'
    assert 'reference-differences.json' not in {item['path'] for item in report['input_files']}


@pytest.mark.parametrize('mutation', ['physics', 'wind', 'null', 'duplicate', 'alias', 'missing'])
def test_unreviewed_source_semantics_refuse(tmp_path, isolated, mutation):
    source, _, derived = isolated
    text = derived.read_text(encoding='utf-8')
    if mutation == 'physics':
        text = text.replace('100', '101')
    elif mutation == 'wind':
        text = text.replace('Constant', 'Other')
    elif mutation == 'null':
        text = text.replace('null', '1')
    elif mutation == 'duplicate':
        text += '    WindType: Constant\n'
    elif mutation == 'alias':
        text = text.replace('Constant', '&wind Constant')
    else:
        text = source.read_text(encoding='utf-8')
    derived.write_text(text, encoding='utf-8')
    with pytest.raises(ValueError):
        packaging.prepare_bundle(tmp_path/'output', source_path=derived)
    assert not (tmp_path/'output').exists()


def test_default_template_semantics_remain_available(tmp_path, isolated):
    source, _, _ = isolated
    # Default argument binds the real template at definition time; pass patched source explicitly.
    result = packaging.prepare_bundle(tmp_path/'default-output', source_path=source)
    derivation = json.loads((result.parent/'source/derivation.json').read_text(encoding='utf-8'))
    assert derivation['applied_keys'] == []


def test_checked_in_derived_spec_has_only_reviewed_semantic_changes():
    derived = ROOT/'docs/benchmarks/mooring_buoy/spec_constant_wind.yml'
    assert packaging._selected_source(derived) == [
        '/environment/raw_properties/VerticalWindVariationFactor',
        '/environment/raw_properties/WindType']


def test_duplicate_template_keys_are_rejected(tmp_path, isolated):
    source, _, derived = isolated
    source.write_bytes(b'environment: {}\nenvironment: {}\n')
    with pytest.raises(ValueError, match='duplicate'):
        packaging.prepare_bundle(tmp_path/'output', source_path=derived)
    assert not (tmp_path/'output').exists()


def test_reference_report_exists_before_manifest_verification(tmp_path, isolated, monkeypatch):
    _, _, derived = isolated
    seen = []
    def verify(path):
        manifest = json.loads(path.read_text(encoding='utf-8'))
        files = {row['path']: row['sha256'] for row in manifest['files']}
        assert files['reference-differences.json'] == packaging.digest(path.parent/'reference-differences.json')
        seen.append(True)
    monkeypatch.setattr(packaging, 'verify_manifest', verify)
    packaging.prepare_bundle(tmp_path/'output', source_path=derived)
    assert seen == [True]


def test_changed_selected_bytes_after_validation_refuse(tmp_path, isolated, monkeypatch):
    _, _, derived = isolated
    original = packaging._selected_source
    changed = []
    def check(path, **kwargs):
        result = original(path, **kwargs)
        if not changed:
            derived.write_text(derived.read_text(encoding='utf-8').replace('100', '101'), encoding='utf-8')
            changed.append(True)
        return result
    monkeypatch.setattr(packaging, '_selected_source', check)
    with pytest.raises(ValueError, match='source difference'):
        packaging.prepare_bundle(tmp_path/'output', source_path=derived)
    assert (tmp_path/'output/source/spec.yml').is_file()
    assert (tmp_path/'output/source/template.yml').is_file()
    assert not (tmp_path/'output/manifest.json').exists()


def test_generator_failure_preserves_owned_partial_evidence(tmp_path, isolated, monkeypatch):
    _, _, derived = isolated
    failure = RuntimeError('synthetic generator failure')
    class FailedGenerator:
        def __init__(self, source):
            pass
        def generate(self, root):
            root.mkdir()
            (root/'partial.yml').write_bytes(b'# SYNTHETIC partial output\n')
            raise failure
    monkeypatch.setattr(packaging, 'ModularModelGenerator', FailedGenerator)
    with pytest.raises(RuntimeError) as captured:
        packaging.prepare_bundle(tmp_path/'failed', source_path=derived)
    assert captured.value is failure
    root = tmp_path/'failed'
    assert (root/'source/spec.yml').read_bytes() == derived.read_bytes()
    assert (root/'source/template.yml').is_file()
    assert (root/'source/derivation.json').is_file()
    assert (root/'bundle/partial.yml').is_file()
    assert not (root/'manifest.json').exists()


@pytest.mark.parametrize('first,second', [
    ('General: {}\nGeneral: {}\n', 'Environment: {}'),
    ('General: &a {}\nOther: *a\n', 'Environment: {}'),
    ('General: {}', 'General: {}'),
])
def test_generated_report_refuses_hidden_assignments(tmp_path, first, second):
    (tmp_path / 'master.yml').write_text('- includefile: a.yml\n- includefile: b.yml\n', encoding='utf-8')
    (tmp_path / 'a.yml').write_text(first, encoding='utf-8')
    (tmp_path / 'b.yml').write_text(second, encoding='utf-8')
    with pytest.raises(ValueError, match='duplicate|aliases|General'):
        packaging._generated_sections(tmp_path)


def test_ordered_object_updates_are_retained_and_reported(tmp_path, isolated):
    _, reference, _ = isolated
    bundle = tmp_path / 'bundle'
    bundle.mkdir()
    (bundle / 'master.yml').write_text('- includefile: a.yml\n- includefile: b.yml\n', encoding='utf-8')
    (bundle / 'a.yml').write_text('Lines: [{Name: Mooring, Length: 1}]\n', encoding='utf-8')
    (bundle / 'b.yml').write_text('Lines: [{Name: Mooring, Length: 2}]\n', encoding='utf-8')
    reference.write_bytes((bundle / 'b.yml').read_bytes())
    packaging._reference_report(tmp_path, {'source': {'sha256': 'a'*64}, 'files': []})
    report = json.loads((tmp_path / 'reference-differences.json').read_bytes())
    assert report['difference_count'] == 0
    assert report['reference_compatible'] is False
    assert len(report['ordered_updates']) == 1
    update = report['ordered_updates'][0]
    assert update['section'] == 'Lines'
    assert update['previous_include'] == 'a.yml'
    assert update['current_include'] == 'b.yml'
    assert update['previous_value_sha256'] == 'd83822bbb3f89a8110928bd97b53d38a5398f94c236d7b371811c489ef75ef44'
    assert update['current_value_sha256'] == '057cdddf8118dcb18cec9733a19ffc35f6ee38d9168ed9f4367abe967ec7edd9'


def test_reference_report_serializes_presence_aware_missing_hashes(tmp_path, isolated):
    _, reference, _ = isolated
    reference.write_bytes(b'General:\n  ExplicitNull: null\n')
    bundle = tmp_path / 'bundle'
    bundle.mkdir()
    (bundle / 'master.yml').write_bytes(b'- includefile: 01_general.yml\n')
    (bundle / '01_general.yml').write_bytes(b'General: {}\n')
    packaging._reference_report(tmp_path, {'source': {'sha256': 'a' * 64}, 'files': []})
    report = json.loads((tmp_path / 'reference-differences.json').read_bytes())
    difference, = report['differences']
    rule = report['missing_member_hash_rule']
    decoded = bytes.fromhex(rule['absent_preimage_hex'])
    assert set(report) == {
        'schema_version', 'missing_member_hash_rule', 'reference_sha256',
        'source_sha256', 'input_files', 'reference_compatible', 'ordered_updates',
        'difference_count', 'differences', 'comparison', 'native_verified',
        'engineering_parity', 'contract_source_role',
    }
    assert report['schema_version'] == 2
    assert rule['id'] == 'digitalmodel.missing-member.sha256.v1'
    assert rule['algorithm'] == 'sha256'
    assert rule['absent_sha256'] == (
        'feb0e82e6d4e278cde90a7ae8c85488c89227c452d53bb0384db99b02a2ebbab')
    assert report['difference_count'] == 1
    assert difference['path'] == '/General/ExplicitNull'
    assert difference['reference_present'] is True
    assert difference['candidate_present'] is False
    assert difference['reference_value_sha256'] == '74234e98afe7498fb5daf1f36ac2d78acc339464f950703b8c019892f982b90b'
    assert decoded == packaging.ABSENT_VALUE_BYTES
    assert packaging.hashlib.sha256(decoded).hexdigest() == rule['absent_sha256']
    assert difference['candidate_value_sha256'] == rule['absent_sha256']
    assert rule['container_limit'] == {
        'id': 'recursive-leaf-only-v2',
        'mapping_key_missing_nonempty_mapping_action': 'recurse_with_empty_mapping',
        'mapping_key_missing_nonempty_mapping_container_row_emitted': False,
        'mapping_key_missing_nonempty_mapping_distinguishes_absent_from_empty': False,
        'mapping_key_missing_other_value_action': 'hash_whole_value',
        'list_missing_member_action': 'hash_whole_member',
        'list_missing_member_row_emitted': True,
    }
    assert 'mapping-key parent' in report['comparison']
    assert 'missing list member hashes the whole member' in report['comparison']


def test_reference_report_declares_reproducible_present_encoder(tmp_path, isolated):
    _, reference, _ = isolated
    reference.write_bytes(b'General:\n  ExplicitNull: null\n')
    bundle = tmp_path / 'bundle'
    bundle.mkdir()
    (bundle / 'master.yml').write_bytes(b'- includefile: 01_general.yml\n')
    (bundle / '01_general.yml').write_bytes(b'General: {}\n')
    packaging._reference_report(tmp_path, {'source': {'sha256': 'a' * 64}, 'files': []})
    report = json.loads((tmp_path / 'reference-differences.json').read_bytes())
    encoder = report['missing_member_hash_rule']['present_encoder']
    assert report['missing_member_hash_rule']['present_value_digest_uniqueness'] is False
    assert encoder == {
        'function': 'json.dumps', 'sort_keys': True, 'default': 'str',
        'ensure_ascii': True, 'allow_nan': True, 'skipkeys': False,
        'check_circular': True, 'indent': None, 'separators': [', ', ': '],
        'text_encoding': 'utf-8',
    }
    encoded = json.dumps(None, sort_keys=encoder['sort_keys'], default=str,
                         ensure_ascii=encoder['ensure_ascii'], allow_nan=encoder['allow_nan'],
                         skipkeys=encoder['skipkeys'], check_circular=encoder['check_circular'],
                         indent=encoder['indent'], separators=tuple(encoder['separators']))
    assert packaging.hashlib.sha256(encoded.encode(encoder['text_encoding'])).hexdigest() == (
        '74234e98afe7498fb5daf1f36ac2d78acc339464f950703b8c019892f982b90b')
    assert packaging._value_hash(date(2024, 1, 1)) == packaging._value_hash('2024-01-01')


def test_present_and_ordered_update_hashes_remain_legacy_compatible():
    assert packaging._value_hash(3) == '4e07408562bedb8b60ce05c1decfe3ad16b72230967de01f640b7e4729b49fce'
