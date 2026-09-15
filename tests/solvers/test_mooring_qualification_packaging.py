"""Synthetic generator isolation for packaging; no native or include-closure claim."""
import importlib.util
import json
from pathlib import Path
import pytest

ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location('mooring_packaging', ROOT/'scripts/prepare_mooring_qualification.py')
packaging = importlib.util.module_from_spec(spec)
spec.loader.exec_module(packaging)


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
    assert update['previous_value_sha256'] != update['current_value_sha256']
