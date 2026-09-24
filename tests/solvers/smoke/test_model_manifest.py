"""Pinned local qualification must reject drift before importing a solver."""
import hashlib
import json
from pathlib import Path

import pytest

from digitalmodel.solvers.smoke.model_manifest import load_contract, verify_manifest


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


@pytest.fixture
def manifest(tmp_path, monkeypatch):
    monkeypatch.setenv("PYTHONHASHSEED", "0")
    contract = load_contract()
    repo = Path(__file__).resolve().parents[3]
    template = repo / contract['source']
    (tmp_path / 'source').mkdir()
    (tmp_path / 'source.yml').write_bytes(template.read_bytes())
    (tmp_path / 'source/template.yml').write_bytes(template.read_bytes())
    reference = repo / 'docs/domains/orcaflex/examples/yml/C07/C07 Metocean buoy in deep water.yml'
    provenance = dict(template_sha256=sha(template), reference_sha256=sha(reference),
                      source_sha256=sha(template), applied_keys=[], contract_source_role='governing_template')
    (tmp_path / 'source/derivation.json').write_text(json.dumps(provenance), encoding='utf-8')
    (tmp_path / "master.yml").write_text("- includefile: general.yml\n")
    (tmp_path / "general.yml").write_text("General:\n  RestartStateRecordingTest: ''\n")
    data = dict(schema_version=1, case_id=contract["case_id"], run_id="test-run",
                source_revision="a" * 40, source={"path": "source.yml", "sha256": sha(tmp_path / "source.yml")},
                master="master.yml", contract=contract, python_hash_seed="0",
                files=[{"path": name, "sha256": sha(tmp_path / name)}
                       for name in ("general.yml", "master.yml", "source.yml", "source/template.yml", "source/derivation.json")],
                outputs={"simulation": "solve/model.sim", "solve_results": "solve/results.json",
                         "readback_results": "readback/results.json"})
    path = tmp_path / "manifest.json"
    path.write_text(json.dumps(data))
    return path, data


def update(path, data):
    path.write_text(json.dumps(data))


def rehash(path, data, filename):
    for record in data["files"]:
        if record["path"] == filename:
            record["sha256"] = sha(path.parent / filename)
    update(path, data)


def test_valid_bundle_returns_resolved_root(manifest):
    path, _ = manifest
    assert verify_manifest(path)["_root"] == path.parent


@pytest.mark.parametrize("field,value", [("schema_version", True), ("extra", 1),
                                        ("source_revision", "bad"), ("python_hash_seed", "1")])
def test_invalid_manifest_fields_rejected(manifest, field, value):
    path, data = manifest
    data[field] = value
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_environment_seed_must_match(manifest, monkeypatch):
    monkeypatch.delenv("PYTHONHASHSEED")
    with pytest.raises(ValueError):
        verify_manifest(manifest[0])


def test_contract_cannot_drop_a_required_result(manifest):
    path, data = manifest
    data["contract"]["results"] = []
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_contract_boolean_cannot_impersonate_thread_count(manifest):
    path, data = manifest
    data["contract"]["resources"]["threads"] = True
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)


@pytest.mark.parametrize("name", ["../escape.yml", "C:/escape.yml", "sub/../general.yml"])
def test_path_escapes_rejected(manifest, name):
    path, data = manifest
    data["master"] = name
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_changed_file_is_rejected(manifest):
    path, _ = manifest
    (path.parent / "general.yml").write_text("General: {}")
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_unlisted_include_rejected_even_when_hashes_match(manifest):
    path, data = manifest
    (path.parent / "extra.yml").write_text("General: {}")
    (path.parent / "master.yml").write_text("- includefile: extra.yml\n")
    rehash(path, data, "master.yml")
    with pytest.raises(ValueError):
        verify_manifest(path)


@pytest.mark.parametrize("value", ["~", "'print(1)'", "false"])
def test_nonempty_or_null_script_rejected(manifest, value):
    path, data = manifest
    (path.parent / "general.yml").write_text("General:\n  RestartStateRecordingTest: " + value)
    rehash(path, data, "general.yml")
    with pytest.raises(ValueError):
        verify_manifest(path)


@pytest.mark.parametrize("text", ["General: &a {Loop: *a}",
                                 "General: {}\nGeneral: {}",
                                 "General: {PythonScript: run_me}",
                                 "Environment: {WaveTimeHistoryFileName: external.txt}",
                                 "- includefile: master.yml"])
def test_ambiguous_hooks_and_include_cycles_rejected(manifest, text):
    path, data = manifest
    (path.parent / "general.yml").write_text(text)
    rehash(path, data, "general.yml")
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_duplicate_manifest_keys_rejected(manifest):
    path, _ = manifest
    text = path.read_text()
    path.write_text('{"schema_version": 1,' + text[1:])
    with pytest.raises(ValueError):
        verify_manifest(path)


def test_duplicate_file_identity_rejected(manifest):
    path, data = manifest
    data["files"].append(data["files"][0])
    update(path, data)
    with pytest.raises(ValueError):
        verify_manifest(path)


@pytest.mark.parametrize('damage', ['missing-template', 'missing-derivation', 'template',
                                   'source', 'applied-keys', 'role', 'source-digest', 'reference-digest'])
def test_rehashed_source_provenance_cannot_bypass_governing_basis(manifest, damage):
    path, data = manifest
    if damage.startswith('missing-'):
        remove = 'source/template.yml' if damage == 'missing-template' else 'source/derivation.json'
        data['files'] = [r for r in data['files'] if r['path'] != remove]
    elif damage in ('template', 'source'):
        name = 'source/template.yml' if damage == 'template' else 'source.yml'
        (path.parent / name).write_bytes(b'physical_basis: altered\n')
        if damage == 'source':
            data['source']['sha256'] = sha(path.parent / name)
        rehash(path, data, name)
    else:
        name = 'source/derivation.json'
        doc = json.loads((path.parent / name).read_bytes())
        key, value = {'applied-keys': ('applied_keys', ['/unreviewed']),
                      'role': ('contract_source_role', 'selected_source'),
                      'source-digest': ('source_sha256', '0'*64),
                      'reference-digest': ('reference_sha256', '0'*64)}[damage]
        doc[key] = value
        (path.parent / name).write_text(json.dumps(doc), encoding='utf-8')
        rehash(path, data, name)
    update(path, data)
    with pytest.raises(ValueError, match='governing|derivation|source provenance'):
        verify_manifest(path)


@pytest.mark.parametrize('change_physics', [False, True])
def test_fully_rehashed_derivation_still_requires_exact_semantics(manifest, change_physics):
    import yaml
    path, data = manifest
    repo = Path(__file__).resolve().parents[3]
    original = repo / 'docs/benchmarks/mooring_buoy/spec_constant_wind.yml'
    selected = path.parent / 'source.yml'
    selected.write_bytes(original.read_bytes())
    if change_physics:
        document = yaml.safe_load(selected.read_bytes())
        document['simulation']['time_step'] = 0.02
        selected.write_bytes(yaml.safe_dump(document, sort_keys=False).encode('utf-8'))
    data['source']['sha256'] = sha(selected)
    rehash(path, data, 'source.yml')
    provenance = path.parent / 'source/derivation.json'
    doc = json.loads(provenance.read_bytes())
    doc.update(source_sha256=sha(selected), applied_keys=[
        '/environment/raw_properties/VerticalWindVariationFactor',
        '/environment/raw_properties/WindType'])
    provenance.write_text(json.dumps(doc), encoding='utf-8')
    rehash(path, data, 'source/derivation.json')
    if change_physics:
        with pytest.raises(ValueError, match='unreviewed semantic'):
            verify_manifest(path)
    else:
        assert verify_manifest(path)['_root'] == path.parent
