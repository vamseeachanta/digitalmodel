# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Fail-closed publication and provenance regressions for the second review."""

import importlib.util
import json
import sys
from hashlib import sha256
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
GENERIC = 'docs/domains/orcaflex/mooring/buoy'


def implementation():
    spec = importlib.util.spec_from_file_location(
        'review_preparation', ROOT / 'scripts/orcaflex/prepare_legacy_library.py')
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def write(root, relative, text='General: {}\nEnvironment: {}\n'):
    path = root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding='utf-8')
    return path


@pytest.mark.parametrize('field', ['Prepared by', 'Verified by', 'Checked by',
                                   'PREPARED BY', 'PreparedBy'])
@pytest.mark.parametrize('comment', ['', '# '])
def test_person_fields_fail_without_disclosing_values(tmp_path, field, comment):
    module = implementation()
    path = write(tmp_path, 'model.yml', f'{comment}{field}: synthetic-person\nGeneral: {{}}\n')
    result = module.inspection.inspect_model(path, 'model.yml', module.inspection.gate.load_rules())
    assert result['status'] == 'fail'
    assert 'person-field' in result['rules']
    assert 'synthetic-person' not in json.dumps(result)


def test_blank_person_fields_do_not_invent_findings(tmp_path):
    module = implementation()
    path = write(tmp_path, 'model.yml', '# Prepared by: \nGeneral: {}\n')
    result = module.inspection.inspect_model(path, 'model.yml', module.inspection.gate.load_rules())
    assert result['status'] == 'pass'


def test_public_fallback_withholds_outside_inputs_and_dependent_models(tmp_path):
    module = implementation()
    outside = 'docs/domains/orcaflex/training/unreviewed.yml'
    write(tmp_path, outside, '# synthetic withheld input\nGeneral: {}\n')
    write(tmp_path, GENERIC + '/dependent.yml', 'BaseFile: ../../training/unreviewed.yml\n')
    write(tmp_path, GENERIC + '/clean.yml')
    # A sibling prefix must not qualify as the reviewed tree.
    write(tmp_path, GENERIC + '_extra/model.yml')
    report = module.inspection.audit(tmp_path)
    withheld = next(item for item in report['models'] if item['path'] == outside)
    assert withheld['status'] == 'fail'
    assert 'publication-withheld' in withheld['rules']
    result = module.inspection.publication_records(report, tmp_path)
    assert [r['path'] for r in result['models']] == [GENERIC + '/clean.yml']
    assert outside not in json.dumps(result)
    assert withheld['sha256'] not in json.dumps(result)
    assert result['counts']['models_with_failed_inputs'] == 3


@pytest.mark.parametrize('name', ['model.yml', 'case/master.yml'])
def test_unparseable_dependencies_exclude_models(tmp_path, name):
    module = implementation()
    write(tmp_path, GENERIC + '/' + name, 'General: [\n')
    record = module.build_inventory(tmp_path, 'a' * 40, '2026-10-18', 123)
    assert record['models'] == []
    assert record['inputs'] == []
    assert record['counts']['models_with_unknown_dependencies'] == 1


def test_reuse_rejects_changed_library_counts(tmp_path):
    module = implementation()
    write(tmp_path, GENERIC + '/model.yml')
    record = module.build_inventory(tmp_path, 'a' * 40, '2026-10-18', 123)
    write(tmp_path, 'docs/domains/orcaflex/training/withheld.yml')
    with pytest.raises(ValueError, match='regenerate legacy-round-trip records'):
        module.verify_reused_inventory(record, tmp_path)


def test_private_rule_digest_binds_effective_list_and_reuse(tmp_path, monkeypatch):
    module = implementation()
    rules = module.inspection.gate.load_rules()
    rules['_private_names'] = ['synthetic-one']
    monkeypatch.setattr(module.inspection.gate, 'load_rules', lambda: rules)
    write(tmp_path, GENERIC + '/model.yml')
    record = module.build_inventory(tmp_path, 'a' * 40, '2026-10-18', 123)
    first = module.inspection.rule_provenance(rules)
    assert len(first['private_rules_sha256']) == 64
    assert 'synthetic-one' not in json.dumps(first)
    rules['_private_names'] = ['synthetic-two']
    assert module.inspection.rule_provenance(rules)['private_rules_sha256'] != first['private_rules_sha256']
    with pytest.raises(ValueError, match='rule provenance mismatch'):
        module.verify_reused_inventory(record, tmp_path)


def test_missing_input_has_locator_free_regeneration_message(tmp_path):
    module = implementation()
    path = write(tmp_path, GENERIC + '/withheld-locator.yml')
    record = module.build_inventory(tmp_path, 'a' * 40, '2026-10-18', 123)
    path.unlink()
    with pytest.raises(ValueError, match='regenerate legacy-round-trip records') as error:
        module.verify_inputs(record, tmp_path)
    assert 'withheld-locator' not in str(error.value)
    assert str(tmp_path) not in str(error.value)


def cli_fixture(tmp_path, monkeypatch):
    module = implementation()
    root = tmp_path / 'source'
    write(root, GENERIC + '/model.yml')
    for name in ('yaml_validator.py', 'yaml_timestep_validation.py'):
        relative = 'src/digitalmodel/solvers/orcaflex/' + name
        write(root, relative, (ROOT / relative).read_text(encoding='utf-8'))
    criterion = tmp_path / 'criterion.json'
    criterion.write_bytes(module.encode({'recorded_utc': '2026-10-18T00:00:00+00:00'}))
    output = tmp_path / 'output'
    monkeypatch.setattr(module, 'ROOT', root)
    args = ['prepare', '--output', str(output), '--source-ref', 'a' * 40,
            '--retrieved', '2026-10-18', '--repository-id', '123', '--prepare',
            '--criterion', str(criterion), '--validation-cache', str(tmp_path / 'cache')]
    monkeypatch.setattr(sys, 'argv', args)
    return module, output, criterion, args


def test_preparation_records_criterion_order_and_binds_skill_outputs(tmp_path, monkeypatch):
    module, output, criterion, args = cli_fixture(tmp_path, monkeypatch)
    monkeypatch.setattr(module, 'build_jumper_skill', lambda *_: {
        'jumper-groups.json': b'{}\n', 'orcaflex-jumper-analysis/SKILL.md': b'synthetic\n'})
    args.append('--skill')
    assert module.main() == 0
    manifest = json.loads((output / 'native-manifest.template.json').read_bytes())
    for name in ('jumper-groups.json', 'orcaflex-jumper-analysis/SKILL.md'):
        assert manifest['files'][name] == sha256((output / name).read_bytes()).hexdigest()
    for name in ('validators.json', 'comparators.json'):
        run = json.loads((output / name).read_bytes())
        module.verify_criterion_order(criterion.read_bytes(), run)


def test_preparation_refuses_future_criterion(tmp_path, monkeypatch):
    module, output, criterion, _ = cli_fixture(tmp_path, monkeypatch)
    criterion.write_bytes(module.encode({'recorded_utc': '2099-01-01T00:00:00+00:00'}))
    with pytest.raises(ValueError, match='precede'):
        module.main()
    assert not (output / 'native-manifest.template.json').exists()


def test_unknown_timed_case_raises_value_error_without_a_locator(tmp_path, monkeypatch):
    module, _, _, args = cli_fixture(tmp_path, monkeypatch)
    timed = tmp_path / 'timed-preparation.json'
    timed.write_bytes(module.encode({'path': 'withheld-locator.yml', 'source_sha256': '0' * 64}))
    args.extend(['--timed-preparation', str(timed)])
    with pytest.raises(ValueError, match='timed case') as error:
        module.main()
    assert 'withheld-locator' not in str(error.value)


def test_cache_docstring_states_integrity_limit():
    module = implementation()
    assert 'deliberate' in module.cache_validation.__doc__


@pytest.mark.parametrize('generic,expected', [
    ({'lines': [{'name': 'synthetic-line'}], 'line_types': [{'name': 'synthetic-type'}]},
     '1 line objects and 1 line types'),
    ({'line_types': [{'name': 'synthetic-type'}]}, 'no line objects'),
    ({}, 'Parameter override'),
])
def test_skill_describes_declared_objects_and_missing_units(tmp_path, monkeypatch, generic, expected):
    import yaml

    module = implementation()
    rules = module.inspection.gate.load_rules()
    rules['_private_names'] = ['synthetic-denied-token']
    monkeypatch.setattr(module.inspection.gate, 'load_rules', lambda: rules)
    for group in ('manifold_to_plet', 'plet_to_plem', 'sut_mm', 'sut_mm/resonance'):
        write(tmp_path, 'docs/domains/orcaflex/jumper/' + group + '/spec.yml',
              yaml.safe_dump({'generic': generic, 'environment': {'waves': {'type': 'Dean stream'}}}))
    record = module.build_inventory(tmp_path, 'a' * 40, '2026-10-18', 123)
    skill = module.build_jumper_skill(record, tmp_path)['orcaflex-jumper-analysis/SKILL.md'].decode()
    assert expected in skill
    assert 'crane wires' not in skill
    assert 'wave type Dean stream' in skill
    assert 'time step not recorded.' in skill
    assert 'Current not recorded;' in skill
    if generic.get('lines'):
        assert 'synthetic-line' in skill
    if generic.get('line_types'):
        assert 'synthetic-type' in skill


def test_validator_retains_malformed_yaml_failure_coverage(tmp_path):
    module = implementation()
    path = write(tmp_path, GENERIC + '/model.yml')
    record = module.build_inventory(tmp_path, 'a' * 40, '2026-10-18', 123)
    path.write_text('General: [\n', encoding='utf-8')
    record['inputs'][0]['sha256'] = sha256(path.read_bytes()).hexdigest()
    record['inputs'][0]['size_bytes'] = path.stat().st_size
    result = module.validate_models(record, tmp_path)['models'][0]
    assert result['status'] == 'fail'
    assert result['errors'] > 0
    assert 'General: [' not in json.dumps(result)
    # Validation can record an invalid supplied input; publication excludes it.
    assert module.build_inventory(tmp_path, 'a' * 40, '2026-10-18', 123)['models'] == []
